(ns bloxorz.core
  (:require [clojure.math.combinatorics :as combo])
  (:gen-class))

(def terrain
  (to-array-2d
    ["VVV       "
     "VVVVVV    "
     "VVVVVVVVV "
     " VVVVVVVVV"
     "     VVTVV"
     "      VVV "]))
(def ^:const all-possible-directions ["right" "left" "up" "down"])
(def start {:x1 1 :y1 1 :x2 1 :y2 1})
(def target {:x1 4 :y1 7 :x2 4 :y2 7})

(defn move
  [{:keys [x1 y1 x2 y2] :as block} direction]
  (let [is-standing? (and (== x1 x2) (== y1 y2))
        is-in-same-row? (== x1 x2)]
    (cond
      (= direction "right") 
      (if is-standing? (assoc block :y1 (inc y1) :y2 (+ y2 2))
        (if is-in-same-row? (assoc block :y1 (+ y1 2) :y2 (inc y2))
          (assoc block :y1 (inc y1) :y2 (inc y2))))
      (= direction "left") 
      (if is-standing? (assoc block :y1 (- y1 2) :y2 (dec y2))
        (if is-in-same-row? (assoc block :y1 (dec y1) :y2 (- y2 2))
          (assoc block :y1 (dec y1) :y2 (dec y2))))
      (= direction "up") 
      (if is-standing? (assoc block :x1 (- x1 2) :x2 (dec x2))
        (if is-in-same-row? (assoc block :x1 (dec x1) :x2 (dec x2))
          (assoc block :x1 (dec x1) :x2 (- x2 2))))
      (= direction "down") 
      (if is-standing? (assoc block :x1 (inc x1) :x2 (+ x2 2))
        (if is-in-same-row? (assoc block :x1 (inc x1) :x2 (inc x2))
          (assoc block :x1 (+ x1 2) :x2 (inc x2))))
      :else block)))

(defn is-block-valid?
  [{:keys [x1 y1 x2 y2]}]
  (and (> x1 0) (> y1 0) 
       (> x2 0) (> y2 0) 
       (< x1 6) (< x2 6)
       (< y1 10) (< y2 10)
       (not= (aget terrain x1 y1) \space)
       (not= (aget terrain x2 y2) \space)))

(defn get-next-moves
  [{:keys [moves blocks]}]
  (for [direction all-possible-directions
        :let [moved-block (move (first blocks) direction)]
        :when 
        (and (is-block-valid? moved-block) 
             (not (contains? (set blocks) moved-block)))]
    {:moves (cons direction moves) :blocks (cons moved-block blocks)}))

(mapcat get-next-moves (mapcat get-next-moves (list {:moves () :blocks #{start}})))
