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

(defrecord block [cell1 cell2])

(def c1 (cell. 1 -1))
(def start-position (block. (cell. 1 1) (cell. 1 1)))
(def target (block. (cell. 4 7) (cell. 4 7)))

(defn move
  [direction & [{:keys [cell1 cell2]} the-block]]
  (let [{x1 :x y1 :y} cell1 
        {x2 :x y2 :y} cell2
        is-standing? (and (== x1 x2) (== y1 y2))]
  (cond
        (= direction "right") 
          (if is-standing? (block. (cell. x1 (inc y1)) (cell. x2 (+ y2 2)))
            (block. (cell. x1 (+ y1 2)) (cell. x2 (inc y2))))
        (= direction "left") 
          (if is-standing? (block. (cell. x1 (dec y1)) (cell. x2 (- y2 2)))
            (block. (cell. x1 (dec y1)) (cell. x2 (- y2 2))))
        (= direction "down") 
          (if is-standing? (block.  (cell. (inc x1) y1) (cell. (+ x2 2) y2))
            (block. (cell. (inc x1) y1) (cell. (inc x2) y2)))
        (= direction "up") 
          (if is-standing? (block. (cell. (dec x1) y1) (cell. (- x2 2) y2))
            (block. (cell. (dec x1) y1) (cell. (dec x2) y2)))
        :else the-block)))

(move "right" start-position)

(defn is-block-valid?
  [block]
  (let [cell1 (.cell1 block)
        cell2 (.cell2 block)
        x1 (.x cell1) y1 (.y cell1)
        x2 (.x cell2) y2 (.y cell2)]
    (and (> x1 0) (> y1 0) (> x2 0) (> y2 0) 
         (= (aget terrain x1 y1) \T)
         (= (aget terrain x2 y2) \T))))

;(defn get-all-moves
;[start-position accumulator]
;(if (= start-position target) accumulator


;(defn get-moves
;[current-position visited-blocks]
;(if (= current-position target) visited-blocks

(defn get-moves
  [blocks-till-now]
  (map #(cons % blocks-till-now) 
       (filter #(and (is-block-valid? %) (not (contains? (set blocks-till-now) %)))
               (map #(move % (first blocks-till-now)) ["right" "left" "up" "down"]))))
(map get-moves (get-moves (list start-position)))
(get-moves (list  (block. (cell. 1 2) (cell. 1 3)) (block. (cell. 1 1) (cell. 1 1))))
(map #(move % (block. (cell. 1 2) (cell. 1 3))) ["right" "left" "up" "down"])

;(get-moves (list (block. 1 4 1 4) (block. 1 2 1 3) (block. 1 1 1 1)))

;(get-moves (list (block. 1 3 1 2) (block. 1 4 1 4) (block. 1 2 1 3) (block. 1 1 1 1)))
