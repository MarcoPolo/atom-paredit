(ns paredit.core
  (:require [clojure.string :as str]))

(defn deactivate [& args]
  (.log js/console "bye world from clojurescript!!"))

(defn get-char-at-position [editor position]
  "_")

(defn move-position-forward [editor position])

(defn move-position-backward [editor position])

(defn at-beginning?
  [editor front-cursor-position])

(defn at-end?
  [editor front-cursor-position])

(defn get-text-btwn [editor position1 position2])

(def matching-tags
  {"[" "]"
   "(" ")"
   "{" "}"
   "]" "["
   ")" "("
   "}" "{"})

(def closing-tags
  #{"}" ")" "]"})

(def opening-tags
  #{"{" "(" "["})

(def tags (apply hash-set (keys matching-tags)))

(conj '(1 2 3) 4)
(peek [3 4] )

(throw "a ball")

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


(defn next-word
  "Returns a new position at the next word"
  [editor cursor-position])

(defn end-of-word
  "Returns a new position at the end of the current word"
  [editor cursor-position])

(defn delete-char-at-position
  "returns the deleted character"
  [editor cursor-position])

(defn replace-char-at-position [editor cursor-position character])

(defn insert-char-at-position [editor cursor-position character])

(defn previous-word
  "Takes you to the start of the previous word"
  [editor cursor-position])
  
(defn forward-slurp [editor cursor-position]
  ;; to forward slurp we find the first closing tag
  ;; and insert it after the first token

  (when (at-end? editor cursor-position)
    (throw (js/Error "No paren found to slurp forward")))

  (println cursor-position)
  (let [end-of-next-word (->> cursor-position
                           (next-word editor)
                           (end-of-word editor))]
    (if (closing-tags (get-char-at-position editor cursor-position))
      (->>
        (delete-char-at-position editor cursor-position)
        (replace-char-at-position editor end-of-next-word))
      (recur editor (move-position-forward editor cursor-position)))))

(defn forward-barf [editor cursor-position]

  (when (at-beginning? editor cursor-position)
    (throw (js/Error "No paren found to slurp forward")))

  (println "Cursor " cursor-position)

  (let [prev-word (previous-word editor cursor-position)]
    (if (closing-tags (get-char-at-position editor cursor-position))
      (do
        (println "Prev-word is: "(get-char-at-position editor prev-word))
        (when (= " " (get-char-at-position editor prev-word))
          (println "Nice")
          (insert-char-at-position editor prev-word " "))
        (->> cursor-position
             (delete-char-at-position editor)
             (replace-char-at-position editor prev-word)))
      (recur editor (move-position-forward editor cursor-position)))
    )
  )

(defn mock-next-word [text]
  (fn [_ p]
    (let [s (subs text p)
          index (.indexOf s (re-find #"\s\w+" s))])
    (if (>= index 0)
      (+ 1 p index)
      (inc (count text)))))

(defn mock-previous-word [text]
  (fn foo [_ p]
    (let [s (str/reverse (subs text 0 p))
          prev-space-position (inc (.indexOf s (re-find #"\s" s)))
          s-pre (subs s prev-space-position)
          index (.indexOf s-pre (first (re-find #"\w(\s|$)" s-pre)))]
      (- p prev-space-position index 1)
      #_(- p index))))

(re-find #"\w$" "foo bar")
(let [t "foo bar baz"]
  (->>
   ((mock-previous-word t) nil 9)
   (subs t)))


(defn mock-end-of-word [text]
  (fn [_ p]
    (let [s (subs text p)
          index (.indexOf s (re-find #"\w\s" s))])
    (if (>= index 0)
      (+ p index)
      p)))

(defn mock-delete-char-at-position [text]
  (fn [_ p]
    (let [t @text]
      (reset! text
              (str (subs t 0 p) (subs t (inc p))))
      (nth t p))))

(defn mock-insert-char-at-position [text]
  (fn [_ p c]
    (str (subs @text 0 p) c (subs @text p))))

(defn mock-replace-char-at-position [text]
  (fn [_ p c]
    (str (subs @text 0 p) c (subs @text (inc p)))))

(let [text (atom "(bitches (get some) stiches)")]
  (with-redefs [delete-char-at-position (mock-delete-char-at-position text)
                insert-char-at-position (mock-insert-char-at-position text)
                replace-char-at-position (mock-replace-char-at-position text)
                move-position-forward  (fn [_ p] (inc p))
                previous-word (mock-previous-word @text)
                end-of-word (mock-end-of-word @text)
                next-word (mock-next-word @text)
                get-char-at-position (fn get-char [_ p] (nth @text p))
                at-end? (fn [_ p] (> p (count @text)))]
    (forward-barf nil 11)))

(subs "(bitches (get) stiches)" 11)

(let [text "lolz dance lolz"]
  (->> 
   ((mock-delete-char-at-position text) nil 1)
   ((mock-insert-char-at-position text) nil 1))
)

(let [text "lolz dance lolz"]
  (->>
   14
   ((mock-next-word text) nil )
   ((mock-end-of-word text) nil)
  
)
  )


(let [text "(this is a simple test)"
      text "
(something [a bit] {more complicated})
(and other stuff too!)"]
  (with-redefs [move-position-forward  (fn [_ p] (inc p))
                move-position-backward (fn [_ p] (dec p))
                get-char-at-position (fn [_ p] (nth text p))
                at-beginning? (fn [_ p] (< p 0))
                at-end? (fn [_ p] (> p (count text)))
                get-text-btwn (fn [_ p1 p2] (subs text p1 (inc p2)))]
    (s-exp nil 11 12 '() true 0 0)))

(println "hey")



(defn fix-indent []
  (let [editor (js/atom.workspace.activePaneItem)]
    (println "Yo indent's broke yo")))

(def commands->fix-indent
  {"paredit:fix-indent" fix-indent})

(defn activate [state]

  ;; Register commands
  (doall
   (for [[command f] commands->fix-indent]
     (js/atom.workspaceView.command command f)))

  (.log js/console "Hello world from clojurescript!!"))


(set! js/module.exports js/paredit.core)

(set! *main-cli-fn* (fn [& args] (.log js/console "hello from maincli " (pr-str args))))
(set! *print-fn* (fn [& args] (.log js/console (apply str args))))
