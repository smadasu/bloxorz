(ns bloxorz.core
  (:require [clojure.pprint :refer :all])
  (:gen-class))

(def terrain
  (to-array-2d
    ["SSSEEEEEEE"
     "SSSSSSEEEE"
     "SSSSSSSSSE"
     "ESSSSSSSSS"
     "EEEEESSTSS"
     "EEEEEESSSE"]))

(def block [[1 1] [1 1]])

(defn is-block-standing?
  [[[x1 y1] [x2 y2]]]
  (and (== x1 y1) (== x2 y2)))

(defn move-right
  [[[x1 y1] [x2 y2] :as block]]
  (if (or (> x1 5) (> x2 5) (> y1 10) (> y2 10))
    (println "right x1=" x1 "y1=" y1 "x2=" x2 "y2=" y2))
  {'("right")
   (vec
     (if (== x1 x2)
       (sort (if (is-block-standing? block)
               [[x1 (inc y1)] [x2 (+ y2 2)]]
               [[x1 (+ y1 2)] [x2 (inc y2)]]))
       [[x1 (inc y1)] [x2 (inc y2)]]))})

(defn move-left
  [[[x1 y1] [x2 y2] :as block]]
  (if (or (> x1 5) (> x2 5) (> y1 10) (> y2 10))
    (println "left x1=" x1 "y1=" y1 "x2=" x2 "y2=" y2))
  {'("left")
   (vec
     (if (== x1 x2)
       (sort [[x1 (dec y1)] [x2 (- y2 2)]])
       [[x1 (dec y1)] [x2 (dec y2)]]))})

(defn move-down
  [[[x1 y1] [x2 y2] :as block]]
  (if (or (> x1 5) (> x2 5) (> y1 10) (> y2 10))
    (println "down x1=" x1 "y1=" y1 "x2=" x2 "y2=" y2))
  {'("down")
   (vec
     (if (== y1 y2)
       (sort (if (is-block-standing? block)
               [[(inc x1) y1] [(+ x2 2) y2]]
               [[(+ x1 2) y1] [(inc x2) y2]]))
       [[(inc x1) y1] [(inc x2) y2]]))})

(defn move-up
  [[[x1 y1] [x2 y2] :as block]]
  (if (or (> x1 5) (> x2 5) (> y1 10) (> y2 10))
    (println "up x1=" x1 "y1=" y1 "x2=" x2 "y2=" y2))
  {'("up")
   (vec
     (if (== y1 y2)
       (sort [[(dec x1) y1] [(- x2 2) y2]])
       [[(dec x1) y1] [(dec x2) y2]]))})

(defn is-valid?
  [previous-blocks [[x1 y1] [x2 y2] :as block]]
  (and (not (contains? previous-blocks block))
       (>= x1 0) (>= y1 0) (>= x2 0) (>= y2 0)
       (< x1 5) (< y1 9) (< x2 5) (< y2 9)
       (not= (aget terrain x1 y1) \E)
       (not= (aget terrain x2 y2) \E)))

(defn target-reached?
  [[[x1 y1] [x2 y2]]]
  (and (= (aget terrain x1 y1) \T)
       (= (aget terrain x2 y2) \T)))

(defn get-all-valid-moves
  [{previous-blocks :previous-blocks current-moves :current-moves current-block :current-block}]
  (map #(zipmap [:previous-blocks :current-moves :current-block]
                [(conj previous-blocks current-block)
                 (concat current-moves (first %)) 
                 (second %)])
       (filter #(is-valid? previous-blocks (second %))
               (merge (move-right current-block) 
                      (move-left current-block)
                      (move-down current-block) 
                      (move-up current-block)))))

(defn -main
  [& args]
  (pprint
  (loop [current-status (list {:previous-blocks #{} :current-moves '() :current-block block})]
    (if (some target-reached? (map :current-block current-status))
      (map :current-moves (filter 
                            (fn [status]
                              (target-reached? (:current-block status)))
                            current-status))
      (recur (mapcat get-all-valid-moves current-status))))))
