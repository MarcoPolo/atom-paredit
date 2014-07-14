(ns paredit.core
  (:require [clojure.string :as str]
            [paredit.atom :as atom]))

;; Constants

(def matching-tags
  {"[" "]"
   "(" ")"
   "{" "}"
   "]" "["
   ")" "("
   "}" "{"})

(def closing-tag-pair
  {"(" ")"
   "{" "}"
   "[" "]"})

(def closing-tags
  #{"}" ")" "]"})

(def opening-tags
  #{"{" "(" "["})

(def whitespace
  #{" " "\n" "\t"})

(def tags (apply hash-set (keys matching-tags)))


;; Stuff to implement

(defn get-end-of-line
  "Should return the position of the end of the line"
  [editor position]
  (let [point (atom/offset->point editor position)]
    (set! (.-column point) js/Infinity)
    (atom/point->offset editor point)))

(defn move-position-forward [editor position]
  (inc position))

(defn move-position-backward [editor position]
  (dec position))

(defn get-text-btwn [editor position1 position2]
  (let [buffer (atom/ed->buffer editor)]
    (.getTextInRange buffer
                     (atom/->range
                      (atom/offset->point editor position1)
                      (atom/offset->point editor position2)))))

(defn get-char-at-position [editor position]
  (let [buffer (atom/ed->buffer editor)]
    (get-text-btwn editor
                   position
                   (move-position-forward editor position))))

(defn at-beginning?
  [editor front-cursor-position]
  (> 0 front-cursor-position))

(defn at-end?
  [editor front-cursor-position]
  (let [buffer (atom/ed->buffer editor)]
    (>= front-cursor-position
        (atom/point->offset editor (.getEndPosition buffer)))))

(defn same-position?
  "Retruns true/false if two positions are equal"
  [editor position-1 position-2]
  (.isEqual
   (atom/offset->point editor position-1)
   (atom/offset->point editor position-2)))

(defn delete-char-at-position
  "returns the deleted character"
  [editor cursor-position]
  (let [buffer (atom/ed->buffer editor)
        old-char (get-char-at-position editor cursor-position)]
    (.setTextInRange
     buffer
     (atom/->range
      (atom/offset->point editor cursor-position)
      (atom/offset->point editor (move-position-forward editor cursor-position)))
     "")
    old-char))

(defn replace-char-at-position [editor cursor-position character]
  (let [buffer (atom/ed->buffer editor)]
    (.setTextInRange
     buffer
     (atom/->range
      (atom/offset->point editor cursor-position)
      (atom/offset->point editor (move-position-forward editor cursor-position)))
     character)))

(defn insert-char-at-position [editor cursor-position character]
  (let [buffer (atom/ed->buffer editor)]
    (.insert
     buffer
     (atom/offset->point editor cursor-position)
     character)))

(defn s-exp [editor front-cursor-position end-cursor-position tag-stack move-forward?
             last-sexp-start last-sexp-end]
  ;; Iterate end cursor forward until we get a closing paren
  ;; when we get a closing paren we'll move the front cursor
  ;; backwards until we balance.
  ;; Repeat until we reach the end of the text

  (when (and (at-beginning? editor front-cursor-position)
             (at-end? editor end-cursor-position)
             (seq tag-stack))
    (throw (js/Error "Unbalanced parens yo")))

  (if (and
       (at-beginning? editor front-cursor-position)
       (at-end? editor end-cursor-position))
    (get-text-btwn editor last-sexp-start last-sexp-end)
    (if move-forward?
      (let [c (get-char-at-position editor end-cursor-position)
            target-tag (matching-tags (peek tag-stack))
            closing-sexp? (and
                           (not= c target-tag)
                           (closing-tags c))]
        (recur
         editor
         front-cursor-position
         (move-position-forward editor end-cursor-position)
         (if (tags c)
           (if (= target-tag c)
             (pop tag-stack)
             (conj tag-stack c))
           tag-stack)
         (if (or
              closing-sexp?
              (at-end? editor end-cursor-position))
           false true)
         last-sexp-start
         (if closing-sexp? end-cursor-position last-sexp-end)))
      (let [c (get-char-at-position editor front-cursor-position)
            target-tag (matching-tags (peek tag-stack))]
        (recur
         editor
         (move-position-backward editor front-cursor-position)
         end-cursor-position
         (if (= c target-tag) (pop tag-stack)
             (if (tags c)
               (conj tag-stack c)
               tag-stack)
             tag-stack)
         (if (or
              (at-beginning? editor front-cursor-position)
              (= c target-tag)) true false)
         (if (= c target-tag) front-cursor-position last-sexp-start)
         last-sexp-end)))))

(defn delete-range [editor start-position end-position]
  (when-not (same-position? editor start-position end-position)
    ;; Delete backwards since deleting will change the end position
    (delete-char-at-position editor end-position)
    (recur editor start-position (move-position-backward editor end-position))))

(defn previous-space [editor p]
  (loop [cursor p]
    (if (at-beginning? editor cursor)
      cursor
      (if (whitespace (get-char-at-position editor cursor))
        cursor
        (recur (move-position-backward editor cursor))))))

(defn forward-space [editor p]
  (loop [cursor p]
    (if (at-end? editor cursor)
      cursor
      (if (whitespace (get-char-at-position editor cursor))
        cursor
        (recur (move-position-forward editor cursor))))))

(defn next-closing-paren [editor cursor-position]
  (loop [cursor-position (move-position-forward editor cursor-position)
         tag-stack '()]
    (let [top-tag (peek tag-stack)
          c (get-char-at-position editor cursor-position)
          forward-cursor (move-position-forward editor cursor-position)]
      (if (at-end? editor cursor-position)
        cursor-position
        (if (opening-tags c)
          (recur forward-cursor
                 (conj tag-stack c))
          (if (closing-tags c)
            (if-not top-tag
              cursor-position
              (if (= c (matching-tags top-tag))
                (recur forward-cursor
                       (pop tag-stack))
                (do
                  (println "error")
                  cursor-position)))
            (recur forward-cursor tag-stack)))))))

(defn prev-opening-paren [editor cursor-position]
  (loop [cursor-position (move-position-backward editor cursor-position)
         tag-stack '()]
    (let [top-tag (peek tag-stack)
          c (get-char-at-position editor cursor-position)
          backward-cursor (move-position-backward editor cursor-position)]
      (if (at-beginning? editor cursor-position)
        cursor-position
        (if (closing-tags c)
          (recur backward-cursor
                 (conj tag-stack c))
          (if (opening-tags c)
            (if-not top-tag
              cursor-position
              (if (= c (matching-tags top-tag))
                (recur backward-cursor
                       (pop tag-stack))
                (do
                  (println "error")
                  cursor-position)))
            (recur backward-cursor tag-stack)))))))


(defn next-nonspace [editor p]
  (loop [cursor p]
    (if (at-end? editor cursor)
      cursor
      (if (whitespace (get-char-at-position editor cursor))
        (recur (move-position-forward editor cursor))
        cursor))))

(defn prev-nonspace [editor p]
  (loop [cursor p]
    (if (at-beginning? editor cursor)
      cursor
      (if (whitespace (get-char-at-position editor cursor))
        (recur (move-position-backward editor cursor))
        cursor))))

(defn prev-pred [editor pred p]
  (loop [cursor p]
    (if (at-beginning? editor cursor)
      cursor
      (if (pred (get-char-at-position editor cursor))
        (recur (move-position-backward editor cursor))
        cursor))))

(defn previous-form [editor cursor]
  (let [c (get-char-at-position editor cursor)]
    (if (closing-tags c)
      (->> (prev-opening-paren editor cursor)
           (move-position-backward editor)
           (prev-nonspace editor)
           (prev-pred editor (complement (apply conj whitespace opening-tags)))
           (move-position-forward editor))
      (if (whitespace c)
        (if (closing-tags (get-char-at-position editor (prev-nonspace editor cursor)))
          (->> (prev-nonspace editor cursor)
               (prev-opening-paren editor))
          (->> (prev-nonspace editor cursor)
               (prev-pred editor (complement (apply conj whitespace opening-tags)))
               (move-position-forward editor)))
        (recur editor (previous-space editor cursor))))))

(defn next-form [editor cursor]
  (let [c (get-char-at-position editor cursor)]
    (if (opening-tags c)
      (->> (next-closing-paren editor cursor)
           (move-position-forward editor)
           (next-nonspace editor))
      (if (whitespace c)
        (next-nonspace editor cursor)
        (->> (forward-space editor cursor)
             (next-nonspace editor))))))

(defn forward-slurp [editor cursor-position]

  (when (at-end? editor cursor-position)
    (throw (js/Error "No paren found to slurp forward")))

  (let [next-form (->> cursor-position
                       (prev-opening-paren editor)
                       (next-closing-paren editor)
                       (next-form editor))
        end-of-next-word (if (opening-tags (get-char-at-position editor next-form))
                           (next-closing-paren editor next-form)
                           (->> (forward-space editor next-form)
                                (move-position-backward editor)))]
    (->> cursor-position
         (prev-opening-paren editor)
         (next-closing-paren editor)
         (delete-char-at-position editor)
         (insert-char-at-position editor end-of-next-word))))

(defn backward-slurp [editor cursor-position]
  (when (at-beginning? editor cursor-position)
    (throw (js/Error "No paren found to slurp backward")))

  (let [prev-paren (prev-opening-paren editor cursor-position)
        spot-to-place (->>
                       (move-position-backward editor prev-paren)
                       (previous-form editor))]
    (->>
     (delete-char-at-position editor prev-paren)
     (insert-char-at-position editor spot-to-place))))


(defn forward-barf [editor cursor-position]

  (when (at-end? editor cursor-position)
    (throw (js/Error "No paren found")))

  (let [cursor-position (next-closing-paren editor cursor-position)
        prev-word (->> (move-position-backward editor cursor-position)
                       (previous-form editor))
        end-of-word (if (opening-tags (get-char-at-position editor prev-word))
                      (next-closing-paren editor prev-word)
                      (forward-space editor prev-word))]
    (->> cursor-position
         (delete-char-at-position editor)
         (insert-char-at-position editor end-of-word))))

(defn backward-barf [editor cursor-position]

  (when (at-beginning? editor cursor-position)
    (throw (js/Error "No paren found")))

  (let [cursor-position (prev-opening-paren editor cursor-position)
        next-word (->>
                   (move-position-forward editor cursor-position)
                   (next-form editor)
                   (move-position-backward editor))]

    (->> cursor-position
         (delete-char-at-position editor)
         (insert-char-at-position editor next-word))))

(defn raise-sexp [editor cursor-position]
  (let [c (get-char-at-position editor cursor-position)
        child-start (if (opening-tags c)
                      cursor-position
                      (prev-opening-paren editor cursor-position))
        child-end (next-closing-paren editor child-start)
        parent-start (->> child-start
                          (move-position-backward editor)
                          (prev-opening-paren editor))
        parent-end (next-closing-paren editor parent-start)]

    (delete-range editor child-end parent-end)
    (delete-range editor
                  (move-position-backward editor parent-start)
                  (previous-space editor child-start))))

(defn at-end-of-line? [editor cursor-position]
  (= cursor-position (get-end-of-line editor cursor-position)))

(defn kill-line-after-cursor [editor cursor-position]
  (when-not (at-end-of-line? editor cursor-position)
    (let [c (get-char-at-position editor cursor-position)]
      (if (opening-tags c)
        (do
          (delete-range editor
                        (move-position-backward editor cursor-position)
                        (next-closing-paren editor cursor-position))

          (recur editor cursor-position))
        (if (closing-tags c)
          (do
            (recur editor (move-position-forward editor cursor-position)))
          (do
            (delete-char-at-position editor cursor-position)
            (recur editor cursor-position)))))))

(defn fix-indent []
  (let [editor (.-activePaneItem js/atom.workspace)
        cursor (-> (.-cursors editor) first)
        position (->> (.getScreenPosition cursor) (atom/point->offset editor))]
    (println "Yo indent's broke yo")
    (println (get-char-at-position editor position))))

(defn generic-cmd-no-trans [f]
  (fn []
    (let [editor (.-activePaneItem js/atom.workspace)
          position (->> (.getCursorBufferPosition editor) (atom/point->offset editor))]
      (f editor position))))

(defn generic-cmd [f]
  (fn []
    (let [editor (.-activePaneItem js/atom.workspace)
          position (->> (.getCursorBufferPosition editor) (atom/point->offset editor))]
      (atom/transact editor #(f editor position)))))

(defn generic-cmd-movement [f]
  (fn []
    (let [editor (.-activePaneItem js/atom.workspace)
          position (->> (.getCursorBufferPosition editor) (atom/point->offset editor))]
          (.setCursorBufferPosition
           editor
           (atom/offset->point
            editor
            (f editor position))))))

(def commands->fix-indent
  {"paredit:forward-barf" (generic-cmd forward-barf)
   "paredit:forward-slurp" (generic-cmd forward-slurp)
   "paredit:backward-barf" (generic-cmd backward-barf)
   "paredit:backward-slurp" (generic-cmd backward-slurp)
   "paredit:raise-sexp" (generic-cmd raise-sexp)
   "paredit:kill-line" (generic-cmd kill-line-after-cursor)
   "paredit:next-form" (generic-cmd-movement next-form)
   "paredit:prev-form" (generic-cmd-movement previous-form)
   "paredit:next-closing-paren" (generic-cmd-movement next-closing-paren)})

(defn deactivate [& args]
  (.log js/console "bye world from clojurescript!!"))

(defn activate [state]

  ;; Register commands
  (doall
   (for [[command f] commands->fix-indent]
     (js/atom.workspaceView.command command f)))

  (.log js/console "Hello world from clojurescript!!!"))


(set! js/module.exports js/paredit.core)

(set! *main-cli-fn* (fn [& args] (.log js/console "hello from maincli " (pr-str args))))
(set! *print-fn* (fn [& args] (.log js/console (apply str args))))
