(ns day11
  (:require [intcode]
            [clojure.java.io :as io]
            [clojure.core.async :as async]))


(def prog (read-string (str "[" (slurp (io/resource "day11.txt")) "]")))

(def input-ch (async/chan 100))
(def output-ch (async/chan 100))

(def running-prog
  (future
    (intcode/intcode 
                     {:ch-in input-ch
                      :ch-out output-ch}
                     prog)
    (async/close! output-ch)))

(defn get-instructions [color]
  (async/put! input-ch color)
  [(async/<!! output-ch) (async/<!! output-ch)])


(def robot {:pos [0 0] :direction [0 1]})

(def panel {[0 0] 1})

(defn get-color [panel pos]
  (get panel pos 0))


(defn turn [[x y] cmd]
  (case cmd
    0 [(- y) x]
    1 [y (- x)]))

(defn move-robot [r* direction-cmd]
  (let [dir* (turn (:direction r*) direction-cmd)]
    {:direction dir*
     :pos (mapv + (:pos r*) dir*)}))

(move-robot {:pos [1 1] :direction [0 1]} 1)  

(defn run []
  (loop [p* panel r* robot c 0]
    (let [[color direction] (get-instructions (get-color p* (:pos r*)))]
      (if (and color direction (< c 1000000))
        (recur
         (assoc p* (:pos r*) color)
         (move-robot r* direction)
         (inc c))
        {:panel p*
         :robot r*
         :count c}))))

(def res (run))
(:count res)
(count (:panel res))

(def panel (:panel res))

(def minx (apply min (map first (keys panel))))
(def maxx (apply max (map first (keys panel))))
(def miny (apply min (map second (keys panel))))
(def maxy (apply max (map second (keys panel))))

(for [y (range miny (inc maxy))]
  (->>
   (for [x (range minx (inc maxx))]
     (case (get panel [x y] 0)
       0 \space
       1 \x))
   (apply str)))

(clojure.pprint/pprint *1)  
