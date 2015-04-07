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

(defrecord block 
  [x1 y1 x2 y2])

(def current-position (block. 1 1 1 1))
(def target (block. 4 7 4 7))

(defn move
  [{:keys [x1 y1 x2 y2]} direction]
  (let [is-standing? (and (== x1 x2) (== y1 y2))]
  (cond
        (= direction "right") 
          (if is-standing? (block. x1 (inc y1) x2 (+ y2 2))
            (block. x1 (+ y1 2) x2 (inc y2)))
        (= direction "left") 
          (if is-standing? (block. x1 (dec y1) x2 (- y2 2))
            (block. x1 (- y1 2) x2 (dec y2)))
        (= direction "down") 
          (if is-standing? (block. (inc x1) y1 (+ x2 2) y2)
            (block. (+ x1 2) y1 (inc x2) y2))
        (= direction "up") 
          (if is-standing? (block. (dec x1) y1 (- x2 2) y2)
            (block. (- x1 2) y1 (dec x2) y2))
        :else block)))

(defn is-valid?
  [visited-blocks & [{:keys [x1 y1 x2 y2]} current-block]]
  (and (> x1 0) (> y1 0) (> x2 0) (> y2 0)
       (= (aget terrain x1 y1) \T) (= (aget terrain x2 y2) \T)
       (not (contains? visited-blocks current-block))))

(is-valid? #{current-position} current-position)

;(defn get-all-moves
;[start-position accumulator]
;(if (= start-position target) accumulator


;(defn get-moves
;[current-position visited-blocks]
;(if (= current-position target) visited-blocks

(defn get-moves
  [blocks]
  (map #(cons % blocks) 
       (filter #(is-valid? #{%} %) 
               (map #(move (first blocks) %) ["right" "left" "up" "down"]))))

(get-moves (list current-position))
