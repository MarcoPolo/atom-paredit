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

(defn previous-form [editor p]
  (loop [cursor (previous-space editor p)
         passed-nonspace? false
         tag-stack '()]
    (let [c (get-char-at-position editor cursor)
          top-tag (peek tag-stack)]
      (println "cursor: " cursor)
      (println "c: " (get-char-at-position editor cursor))
      (println "tt: " top-tag)
      (println "pn: " passed-nonspace?)
      (if (at-beginning? editor cursor)
        cursor
        (if (or (whitespace c) (opening-tags c))
          (if passed-nonspace?
            (move-position-forward editor cursor)
            (recur
             (move-position-backward editor cursor)
             passed-nonspace?
             tag-stack))

          (if (tags c)
            (if (and top-tag (= (closing-tag-pair c) top-tag))
              (recur
               (move-position-backward editor cursor)
               true
               (pop tag-stack))
              (recur
               (move-position-backward editor cursor)
               true
               (conj tag-stack c)))
            (recur
             (move-position-backward editor cursor)
             true
             tag-stack)))))))

(defn next-form [editor cursor-position]
  (let [c (get-char-at-position editor cursor-position) ]
    (loop [cursor (->> (forward-space editor cursor-position)
                       (next-nonspace editor))
           tag-stack (if (opening-tags c)
                       '(c)
                       '())]
      (let [c (get-char-at-position editor cursor)
            top-tag (peek tag-stack)]

        (println "cursor: " cursor)
        (println "c: " (get-char-at-position editor cursor))
        (println "tt: " top-tag)

        (if (at-end? editor cursor)
          cursor
          (if (whitespace c)
            (recur (move-position-forward editor cursor)
                   tag-stack)
            (if (= c (matching-tags top-tag))
              (recur (move-position-forward editor cursor)
                     (pop tag-stack))
              (if top-tag
                (recur (move-position-forward editor cursor)
                       tag-stack)
                ;; tag stack is clear and this isn't whitespace
                cursor))))))))

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

(defn forward-slurp [editor cursor-position]
  ;; to forward slurp we find the first closing tag
  ;; and insert it after the first token

  (when (at-end? editor cursor-position)
    (throw (js/Error "No paren found to slurp forward")))

  (println cursor-position)
  (let [end-of-next-word (->> cursor-position
                              (prev-opening-paren editor)
                              (next-closing-paren editor)
                              (next-form editor)
                              (forward-space editor)
                              (move-position-backward editor))]
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

  (println "Cursor " cursor-position)

  (let [cursor-position (next-closing-paren editor cursor-position)
        prev-cursor (move-position-backward editor cursor-position)
        prev-word (->> (previous-form editor prev-cursor)
                       (forward-space editor))]
    (println "Prev-cursor is: "(get-char-at-position editor prev-cursor))
    (println "Prev-word is: "(get-char-at-position editor prev-word))
    (->> cursor-position
         (delete-char-at-position editor)
         (insert-char-at-position editor prev-word))))

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
    (println "foo")

    (delete-range editor child-end parent-end)
    (delete-range editor parent-start child-start)))

(defn fix-indent []
  (let [editor (.-activePaneItem js/atom.workspace)
        cursor (-> (.-cursors editor) first)
        position (->> (.getScreenPosition cursor) (atom/point->offset editor))]
    (println "Yo indent's broke yo")
    (println (get-char-at-position editor position))))

(defn generic-cmd [f]
  (fn []
    (let [editor (.-activePaneItem js/atom.workspace)
          cursor (-> (.-cursors editor) first)
          position (->> (.getScreenPosition cursor) (atom/point->offset editor))]
      (atom/transact editor #(f editor position)))))

(def commands->fix-indent
  {"paredit:fix-indent" fix-indent
   "paredit:forward-barf" (generic-cmd forward-barf)
   "paredit:forward-slurp" (generic-cmd forward-slurp)
   "paredit:backward-barf" (generic-cmd backward-barf)
   "paredit:backward-slurp" (generic-cmd backward-slurp)})

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
