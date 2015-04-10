(ns bloxorz.core
  (:require [clojure.math.combinatorics :as combo])
  (:gen-class))

(def terrain
  (to-array-2d
    ["TTT       "
     "TTTTTT    "
     "TTTTTTTTT "
     " TTTTTTTTT"
     "     TT TT"
     "      TTT "]))

(defrecord cell [x y])

(defrecord block [moves cell1 cell2])

(def c1 (cell. 1 -1))
(def start-position (block. () (cell. 1 1) (cell. 1 1)))
(def target-location (cell. 4 7))

(defn move
  [direction & [{:keys [moves cell1 cell2]} the-block]]
  (let [{x1 :x y1 :y} cell1 
        {x2 :x y2 :y} cell2
        is-standing? (and (== x1 x2) (== y1 y2))
  new-coordinates (cond
        (= direction "right") 
          (if is-standing? {:x1 x1 :y1 (inc y1) :x2 x2 :y2 (+ y2 2)} 
            {:x1 x1 :y1 (+ y1 2) :x2 x2 :y2 (inc y2)})
        (= direction "left") 
          (if is-standing? {:x1 x1 :y1 (dec y1) :x2 x2 :y2 (- y2 2)}
            {:x1 x1 :y1 (dec y1) :x2 x2 :y2 (- y2 2)})
        (= direction "up") 
          (if is-standing? {:x1 (dec x1) :y1 y1 :x2 (- x2 2) :y2 y2}
            {:x1 (dec x1) :y1 y1 :x2 (dec x2) :y2 y2})
        (= direction "down") 
          (if is-standing? {:x1 (inc x1) :y1 y1 :x2 (+ x2 2) :y2 y2}
            {:x1 (inc x1) :y1 y1 :x2 (inc x2) :y2 y2})
        :else the-block)]
   (block. (cons direction moves) 
           (cell. (:x1 new-coordinates) (:y1 new-coordinates)) 
           (cell. (:x2 new-coordinates) (:y2 new-coordinates)))
    ))
(map #(move % start-position) ["right" "left" "up" "down"])
(map #(move % (block. (list "right") (cell. 1 2) (cell. 1 3))) ["right" "left" "up" "down"])

(move "right" start-position)
(move "up" (block. (list "down") (cell. 2 1) (cell. 3 1)))
(map #(move % (block. (list "down") (cell. 2 1) (cell. 3 1))) ["right" "left" "up" "down"])

(defn is-block-valid?
  [{:keys [cell1 cell2]}]
  (let [x1 (.x cell1) y1 (.y cell1)
        x2 (.x cell2) y2 (.y cell2)]
    (and (> x1 0) (> y1 0) (> x2 0) (> y2 0) 
         (= (aget terrain x1 y1) \T)
         (= (aget terrain x2 y2) \T))))

(defn get-moves
  [blocks-till-now]
  (map #(cons % blocks-till-now) 
       (filter #(and (is-block-valid? %) (not (contains? (set blocks-till-now) %)))
               (map #(move % (first blocks-till-now)) ["right" "left" "up" "down"]))))
(map get-moves (get-moves (list start-position)))
(map #(move % start-position) ["right" "left" "up" "down"])
