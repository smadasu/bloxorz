(ns bloxorz.core
  (:gen-class))

(def terrain
  (to-array-2d
    ["SSSEEEEEEE"
     "SSSSSSEEEE"
     "SSSSSSSSSE"
     "ESSSSSSSSS"
     "EEEEESSESS"
     "EEEEEESSSE"]))

(def block [[1 1] [1 1]])

(defn is-block-standing?
  [[[x1 y1] [x2 y2]]]
  (and (== x1 y1) (== x2 y2)))

(defn move-right
  [[[x1 y1] [x2 y2] :as block]]
  (into []
        (if (== x1 x2)
          (sort (if (is-block-standing? block)
                  [[x1 (inc y1)] [x2 (+ y2 2)]]
                  [[x1 (+ y1 2)] [x2 (inc y2)]]))
          [[x1 (inc y1)] [x2 (inc y2)]])))

(defn move-left
  [[[x1 y1] [x2 y2] :as block]]
  (into []
        (if (== x1 x2)
          (sort [[x1 (dec y1)] [x2 (- y2 2)]])
          [[x1 (dec y1)] [x2 (dec y2)]])))

(defn move-down
  [[[x1 y1] [x2 y2] :as block]]
  (into []
        (if (== y1 y2)
          (sort (if (is-block-standing? block)
                  [[(inc x1) y1] [(+ x2 2) y2]]
                  [[(+ x1 2) y1] [(inc x2) y2]]))
          [[(inc x1) y1] [(inc x2) y2]])))

(defn move-up
  [[[x1 y1] [x2 y2] :as block]]
  (into []
        (if (== y1 y2)
          (sort [[(dec x1) y1] [(- x2 2) y2]])
          [[(dec x1) y1] [(dec x2) y2]])))

(defn is-valid?
  [[[x1 y1] [x2 y2]]]
  (and (>= x1 0) (>= y1 0) (>= x2 0) (>= y2 0)
       (not= (aget terrain x1 y1) \E)
       (not= (aget terrain x2 y2) \E)))

(defn get-all-valid-moves
  [[[x1 y1] [x2 y2] :as block]]
  (filterv is-valid?
          (list (move-right block) (move-left block)
           (move-down block) (move-up block))))

(map #(cons % [block]) (get-all-valid-moves block))
