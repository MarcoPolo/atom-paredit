(ns paredit.atom
  "Wrapper around atom primitives")

(def text-buffer (js/require "text-buffer"))

(defn ->point [[r c]]
  (text-buffer.Point. r c))

(defn ->range [p1 p2]
  (text-buffer.Range. p1 p2))

(defn ed->buffer [editor]
  (.-buffer editor))

(defn offset->point [editor offset]
  (let [buffer (ed->buffer editor)] 
    (.positionForCharacterIndex buffer offset)))

(defn point->offset [editor point]
  (let [buffer (ed->buffer editor)] 
    (.characterIndexForPosition buffer point)))

(defn transact [editor f]
  (let [buffer (ed->buffer editor)] 
    (.transact buffer f)))
