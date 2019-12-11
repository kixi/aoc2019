(ns day10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn fmt [str]
  (->>
   (str/split-lines str)
   (map str/trim)
   (str/join "\n")))

(defn asteroid-ch? [ch]
  (case ch
    \# true
    false))

(defn create-model [model-str]
  (->>
   model-str
   (str/split-lines)
   (map-indexed (fn [y line]
                  (map-indexed (fn [x ast]
                                 [[x y] (asteroid-ch? ast)])
                               line)))
   (apply concat)
   (filter second)
   (map first)
   set))

(def asteroids (create-model (slurp (io/resource "day10.txt"))))

(defn add-coord [c0 c1]
  (mapv + c0 c1))

(defn move [shape point]
  (map (partial add-coord point)
       shape))


(defn sort-points-by-distance [points]
  (sort (fn [[[_ r0] _]
             [[_ r1] _]]
          (compare r0 r1))
        points))


(defn mod2pi [angle]
  (if (>= angle (* 2 Math/PI))
    (- angle (* 2 Math/PI))
    angle))

(defn aocp2 [[x y]]
  [(mod2pi (+ Math/PI (Math/atan2 (* -1 x) y)))
   (Math/sqrt (+ (* x x)
                 (* y y)))])

(defn part1 []
  (->>
   (for [station asteriods]
     [station
      (->
       (for [asteriod asteriods
             :when (not= station asteriod)]
         (->>
          (map - asteriod station)
          aocp2
          first))
       distinct
       count)])
   (apply max-key second))) 
(def r-part1 (part1))

(def station (first r-part1))

(defn part2 []
  (->>
   (move asteriods (mapv (partial - 0) station))
   (filter (complement #{[0 0]}))
   (map (fn [p] [(aocp2 p) (move [p] station)]))
   (group-by (fn [[[angle _] _]] angle))
   (map (fn [[k v]] [k (sort-points-by-distance v)]))
   (sort (fn [[angle _] [angle1 _]]
           (compare angle angle1)))))

(def polar-model (part2))

(defn loop-step [ast-mod old-model] 
  (reduce (fn [{:keys [asteroids model]} [angle coords]]
            (let [next-asteroid (first coords)
                  model* (if (seq (rest coords))
                           (conj model [angle (rest coords)])
                           model)]
              {:asteroids (conj asteroids (last  next-asteroid))
               :model model*}))
          ast-mod
          old-model))
(first
 (drop 199
       (loop [m* polar-model a* [] m** []]
         (let [{:keys [asteroids model]}
               (loop-step {:asteroids a*
                           :model m**}
                          m*)]
           (if (seq model)
             (recur model asteroids [])
             asteroids)))))

