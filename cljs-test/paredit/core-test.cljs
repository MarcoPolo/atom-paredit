(ns paredit.core-test
  "Test the core functionality of paredity with a mock editor")

(defn mock-next-word [text]
  (fn [_ p]
    (let [s (subs text p)
          index (.indexOf s (re-find #"\s\w+" s))])
    (if (>= index 0)
      (+ 1 p index)
      (inc (count text)))))

(comment
  "This is here so we can test the actual paredit inside the editor"

  (foo (bar (bam baz) lol wtf) rofl)

  (foo (lolz (baz bar))

)

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
      text " (something [a bit] {more complicated}) (and other stuff too!)"]
  (with-redefs [move-position-forward  (fn [_ p] (inc p))
                move-position-backward (fn [_ p] (dec p))
                get-char-at-position (fn [_ p] (nth text p))
                at-beginning? (fn [_ p] (< p 0))
                at-end? (fn [_ p] (> p (count text)))
                get-text-btwn (fn [_ p1 p2] (subs text p1 (inc p2)))]
    (s-exp nil 11 12 '() true 0 0)))

(println "hey")

(set! js/module.exports js/paredit.core)

(set! *main-cli-fn* (fn [& args] (.log js/console "hello from maincli " (pr-str args))))
(set! *print-fn* (fn [& args] (.log js/console (apply str args)))))
