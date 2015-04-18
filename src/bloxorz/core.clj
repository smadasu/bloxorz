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

(defrecord block [x1 y1 x2 y2])

(def start-position (block. 1 1 1 1))
(def target-location (block. 4 7 4 7))

(defn move
  [{:keys [x1 y1 x2 y2]} direction]
  (let [is-standing? (and (== x1 x2) (== y1 y2))
        is-in-same-row? (== x1 x2)]
  (cond
        (= direction "right") 
          (if is-standing? (block. x1 (inc y1) x2 (+ y2 2))
            (if is-in-same-row? (block. x1 (+ y1 2) x2 (inc y2))
              (block. x1 (inc y1) x2 (inc y2))))
        (= direction "left") 
          (if is-standing? (block. x1 (- y1 2) x2 (dec y2))
            (if is-in-same-row? (block. x1 (dec y1) x2 (- y2 2))
              (block. x1 (dec y1) x2 (dec y2))))
        (= direction "up") 
          (if is-standing? (block. (- x1 2) y1 (dec x2) y2)
            (if is-in-same-row? (block. (dec x1) y1 (dec x2) y2)
              (block. (dec x1) y1 (- x2 2) y2)))
        (= direction "down") 
          (if is-standing? (block. (inc x1) y1 (+ x2 2) y2)
            (if is-in-same-row? (block. (inc x1) y1 (inc x2) y2)
              (block. (+ x1 2) y1 (inc x2) y2)))
        :else block)))
(map #(move start-position %)["right" "left" "up" "down"])
(map #(move start-position %)["right" "right" "right" "down"])
(map #(move (block. (cell. 2 1) (cell. 3 1)) %)["right" "left" "up" "down"])
(move (block. (cell. 1 2) (cell. 1 3)) "right")


(defn is-block-valid?
  [{:keys [x1 y1 x2 y2]}]
  (try
    (and (> x1 0) (> y1 0) (> x2 0) (> y2 0) 
         (= (aget terrain x1 y1) \T)
         (= (aget terrain x2 y2) \T))
 (catch Exception e (str "caught exception: " (.printStackTrace e)))))

(defn get-next-moves
  [moves-till-now]
  (mapcat
    (fn [{:keys [moves blocks]}]
      (for [direction ["right" "left" "up" "down"]
            :let [moved-block (move (first blocks) direction)]
            :when 
            (and (is-block-valid? moved-block) 
                 (not (contains? (set blocks) moved-block)))]
        {:moves (cons direction moves) :blocks (cons moved-block blocks)})) moves-till-now))

(defn get-valid-moves
  [moves-till-now]
  (let [next-moves (get-next-moves moves-till-now)]
    next-moves
    ;(if (empty? next-moves) moves-till-now
      ;(get-valid-moves next-moves))
    ))

(map #(reverse (:moves %)) (get-valid-moves (get-valid-moves (get-valid-moves (get-valid-moves (list {:moves [""] :blocks [start-position]}))))))

(reduce move start-position  ["down" "up"])
(reduce move start-position  ["right" "right" "right"])
(move (move (block. (cell. 2 2) (cell. 3 2)) "up") "down")
