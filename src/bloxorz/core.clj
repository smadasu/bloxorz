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
(def target-location (cell. 4 7))

(defn move
  [{{x1 :x y1 :y} :cell1 {x2 :x y2 :y} :cell2 :as block} direction]
  (let [is-standing? (and (== x1 x2) (== y1 y2))]
  (cond
        (= direction "right") 
          (if is-standing? (block. (cell. x1 (inc y1)) (cell. x2 (+ y2 2)))
            (block. (cell. x1 (+ y1 2)) (cell. x2 (inc y2))))
        (= direction "left") 
          (if is-standing? (block. (cell. x1 (dec y1)) (cell. x2 (- y2 2)))
            (block. (cell. x1 (dec y1)) (cell. x2 (- y2 2))))
        (= direction "up") 
          (if is-standing? (block. (cell. (dec x1) y1) (cell. (- x2 2) y2))
            (block. (cell. (dec x1) y1) (cell. (dec x2) y2)))
        (= direction "down") 
          (if is-standing? (block. (cell. (inc x1) y1) (cell. (+ x2 2) y2))
            (block. (cell. (inc x1) y1) (cell. (inc x2) y2)))
        :else block)))

(defn is-block-valid?
  [{{x1 :x y1 :y} :cell1 {x2 :x y2 :y} :cell2}]
    (and (> x1 0) (> y1 0) (> x2 0) (> y2 0) 
         (= (aget terrain x1 y1) \T)
         (= (aget terrain x2 y2) \T)))

(defn get-valid-moves
  [block]
  (filter #(is-block-valid? (second %))
          (reduce (fn [the-map direction]
                    (assoc the-map direction (move block direction))) {} ["right" "left" "up" "down"])))

(get-valid-moves start-position)

(defn get-moves
  [blocks-till-now]
  (map #(cons % blocks-till-now) 
       (filter #(and (is-block-valid? %) (not (contains? (set blocks-till-now) %)))
               (map #(move % (first blocks-till-now)) ["right" "left" "up" "down"]))))
(map get-moves (get-moves (list start-position)))
(map #((assoc {} % (move % start-position))) ["right" "left" "up" "down"])


