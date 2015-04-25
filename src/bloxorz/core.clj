(ns bloxorz.core
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
(def start {:moves () :x1 1 :y1 1 :x2 1 :y2 1})
(def target {:x1 4 :y1 7 :x2 4 :y2 7})

(defn move
  [{:keys [moves x1 y1 x2 y2] :as block} direction]
  (let [is-standing? (and (== x1 x2) (== y1 y2))
        is-in-same-row? (== x1 x2)]
    (cond
      (= direction "right") 
      (if is-standing? (assoc block :moves (cons direction moves) :y1 (inc y1) :y2 (+ y2 2))
        (if is-in-same-row? (assoc block :moves (cons direction moves) :y1 (+ y1 2) :y2 (inc y2))
          (assoc block :moves (cons direction moves) :y1 (inc y1) :y2 (inc y2))))
      (= direction "left") 
      (if is-standing? (assoc block :moves (cons direction moves) :y1 (- y1 2) :y2 (dec y2))
        (if is-in-same-row? (assoc block :moves (cons direction moves) :y1 (dec y1) :y2 (- y2 2))
          (assoc block :moves (cons direction moves) :y1 (dec y1) :y2 (dec y2))))
      (= direction "up") 
      (if is-standing? (assoc block :moves (cons direction moves) :x1 (- x1 2) :x2 (dec x2))
        (if is-in-same-row? (assoc block :moves (cons direction moves) :x1 (dec x1) :x2 (dec x2))
          (assoc block :moves (cons direction moves) :x1 (dec x1) :x2 (- x2 2))))
      (= direction "down") 
      (if is-standing? (assoc block :moves (cons direction moves) :x1 (inc x1) :x2 (+ x2 2))
        (if is-in-same-row? (assoc block :moves (cons direction moves) :x1 (inc x1) :x2 (inc x2))
          (assoc block :moves (cons direction moves) :x1 (+ x1 2) :x2 (inc x2))))
      :else block)))

(defn is-block-valid?
  [{:keys [x1 y1 x2 y2]}]
  (and (> x1 0) (> y1 0) 
       (> x2 0) (> y2 0) 
       (< x1 6) (< x2 6)
       (< y1 10) (< y2 10)
       (not= (aget terrain x1 y1) \space)
       (not= (aget terrain x2 y2) \space)))

(defn are-blocks-equal?
  [block1 block2]
  (and (== (:x1 block1) (:x1 block2))
       (== (:y1 block1) (:y1 block2))
       (== (:x2 block1) (:x2 block2))
       (== (:y2 block1) (:y2 block2))))

(defn blocks-contain?
  [blocks block]
  (not-empty 
    (filter #(are-blocks-equal? % block) blocks)))

(defn get-next-moves
  [moves-and-blocks]
  (let [seperated-blocks ((juxt filter remove) 
                          #(are-blocks-equal? (first %) target)
                          (mapcat
                            (fn [blocks]
                              (let [first-block (first blocks)]
                                (map #(cons % blocks) 
                                     (filter #(and (is-block-valid? %) (not (blocks-contain? blocks %))) 
                                             (map #(move first-block %) all-possible-directions))) ))
                            moves-and-blocks))
        target-blocks (first seperated-blocks)]
    (if (not-empty target-blocks) (map #(reverse (:moves (first %))) target-blocks)
      (get-next-moves (second seperated-blocks))
      )))

(defn -main 
  [& args]
  (println (get-next-moves [[start]])))
