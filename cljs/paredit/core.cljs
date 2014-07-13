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

(defn same-position?
  "Retruns true/false if two positions are equal"
  [editor position-1 position-2]
  (println "not implemented"))

(defn delete-char-at-position
  "returns the deleted character"
  [editor cursor-position])

(defn delete-range [editor start-position end-position]
  (when-not (same-position? editor start-position end-position)
    ;; Delete backwards since deleting will change the end position
    (delete-char-at-position editor end-position)
    (recur editor start-position (move-position-backward editor end-position))))

(defn replace-char-at-position [editor cursor-position character])

(defn insert-char-at-position [editor cursor-position character])

(defn previous-word
  "Takes you to the start of the previous word"
  [editor cursor-position])

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
    (let [c (get-char-at-position nil cursor)
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
        (println "pn: " passed-nonspace?)
        
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
                           (next-word editor)
                           (end-of-word editor))]
    (if (closing-tags (get-char-at-position editor cursor-position))
      (->>
        (delete-char-at-position editor cursor-position)
        (replace-char-at-position editor end-of-next-word))
      (recur editor (move-position-forward editor cursor-position)))))

(defn backward-slurp [editor cursor-position]
  (when (at-beginning? editor cursor-position)
    (throw (js/Error "No paren found to slurp backward")))

  (let [prev-paren (prev-closing-paren editor cursor-position)
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

  (println "Cursor " cursor-position)

  (let [cursor-position (prev-opening-paren editor cursor-position)
        next-cursor (move-position-forward editor cursor-position)
        next-word (next-form editor next-cursor)]
    (println "next-cursor is: "(get-char-at-position editor next-cursor))
    (println "next-word is: "(get-char-at-position editor next-word))
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
(apply conj #{"foo" "bar"} #{"a" "b"})


(let [text (atom "(bitches (get some) stiches)")]
  (with-redefs [delete-char-at-position (mock-delete-char-at-position text)
                insert-char-at-position (mock-insert-char-at-position text)
                replace-char-at-position (mock-replace-char-at-position text)
                move-position-forward  (fn [_ p] (inc p))
                move-position-forward  (fn [_ p] (inc p))
                previous-word (mock-previous-word @text)
                end-of-word (mock-end-of-word @text)
                next-word (mock-next-word @text)
                get-char-at-position (fn get-char [_ p] (nth @text p))
                move-position-backward (fn [_ p] (dec p))
                at-beginning? (fn [_ p] (< p 0))
                at-end? (fn [_ p] (> p (count @text)))]
    (println (subs @text 11))
    (->>
     (previous-form nil 17)
     (forward-space nil)
     (move-position-forward nil)
     (subs @text))
   ))

(let [text (atom "(bitches (get some) stiches)")]
  (with-redefs [delete-char-at-position (mock-delete-char-at-position text)
                insert-char-at-position (mock-insert-char-at-position text)
                replace-char-at-position (mock-replace-char-at-position text)
                move-position-forward  (fn [_ p] (inc p))
                move-position-forward  (fn [_ p] (inc p))
                previous-word (mock-previous-word @text)
                end-of-word (mock-end-of-word @text)
                next-word (mock-next-word @text)
                get-char-at-position (fn get-char [_ p] (nth @text p))
                move-position-backward (fn [_ p] (dec p))
                at-beginning? (fn [_ p] (< p 0))
                at-end? (fn [_ p] (> p (count @text)))]
    (println (subs @text 17))
    (->>
     (next-form nil 17)
     (subs @text))
   ))


(subs  "(bitches (get some) stiches)" 17)
(nth "(bitches (get some) stiches)" 18)

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

(defn mock-same-position? []
  (fn [_ p1 p2]
    (= p1 p2)))

(defn mock-insert-char-at-position [text]
  (fn [_ p c]
    (reset! text
            (str (subs @text 0 p) c (subs @text p)))))

(defn mock-replace-char-at-position [text]
  (fn [_ p c]
    (str (subs @text 0 p) c (subs @text (inc p)))))

(let [text (atom "(bitches (get (some) fd) stiches)")]
  (with-redefs [delete-char-at-position (mock-delete-char-at-position text)
                insert-char-at-position (mock-insert-char-at-position text)
                replace-char-at-position (mock-replace-char-at-position text)
                move-position-forward  (fn [_ p] (inc p))
                previous-word (mock-previous-word @text)
                end-of-word (mock-end-of-word @text)
                next-word (mock-next-word @text)
                get-char-at-position (fn get-char [_ p] (nth @text p))
                move-position-backward (fn [_ p] (dec p))
                at-beginning? (fn [_ p] (< p 0))
                at-end? (fn [_ p] (> p (count @text)))]
    ;(next-closing-paren nil 14)
    (forward-barf nil 14)
    ))

(let [text (atom "(bitches (get (some) fd) stiches)")]
  (with-redefs [delete-char-at-position (mock-delete-char-at-position text)
                insert-char-at-position (mock-insert-char-at-position text)
                replace-char-at-position (mock-replace-char-at-position text)
                move-position-forward  (fn [_ p] (inc p))
                previous-word (mock-previous-word @text)
                end-of-word (mock-end-of-word @text)
                next-word (mock-next-word @text)
                get-char-at-position (fn get-char [_ p] (nth @text p))
                move-position-backward (fn [_ p] (dec p))
                at-beginning? (fn [_ p] (< p 0))
                at-end? (fn [_ p] (> p (count @text)))
                same-position? (mock-same-position?)]
    ;(next-closing-paren nil 14)
    ;(prev-opening-paren nil 14)
    ;(backward-barf nil 14)
    ;(backward-slurp nil 14)
    #_(->>
     (next-closing-paren nil 9)
     (subs @text))

    (raise-sexp editor 9)
    @text
    ))

(bitches (get (some) fd) stiches)
(subs "(bitches (get (some) fd) stiches)" 0 9 )
(nth "(bitches (get (some) fd) stiches)" 9)

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



(re-find #"(\s\w|\([\w\W]\))" "foo bar" )


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
