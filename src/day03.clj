
(ns day03
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))


(defn parse-cmd [cmd]
  (let [direction (first cmd)
        amt (Integer/parseInt (apply str (rest cmd)))]
    [(keyword (str direction)) amt]))

(= [:R 76] (parse-cmd "R76"))

(defn line [[dir length]]
  (case dir
    :R (for [l (range 1 (inc length))] [l 0])
    :L (for [l (range 1 (inc length))] [(- 0 l) 0])
    :U (for [l (range 1 (inc length))] [0 l])
    :D (for [l (range 1 (inc length))] [0 (- 0 l)])))


(defn parse-line [line]
  (let [sl (-> line
             (str/split #","))]
     (map parse-cmd sl)))

(defn line-on-pos [line [x y]]
  (map (fn [[xl yl]] [(+ xl x) (+ yl y)]) line))


(defn draw-path [cmds start]
  (reduce (fn [points cmd]
            (concat points
                    (line-on-pos
                     (line cmd)
                     (or (last points) start))))
          []
          cmds))

(defn common-points [path1 path2]
  (set/intersection (into #{} path1)
                    (into #{} path2)))

(defn manhatten-distance [[x0 y0] [x1 y1]]
  (+
   (Math/abs (- x0 x1))
   (Math/abs (- y0 y1))))


(def input (str/split-lines (slurp (io/resource "day03.txt"))))

(def wire-1 (parse-line (get input 0)))
(def wire-2 (parse-line (get input 1)))

(defn run-part1 []
  (let [p0 (draw-path wire-1 [0 0])
        p1 (draw-path wire-2 [0 0])]
    (->>
     (common-points p0 p1)
    ;; (filter (fn [v] (not= v [0 0])))
     (map #(manhatten-distance % [0 0]))
     (apply min))))
    

(run-part1)


(defn run-part2 []
  (let [p0 (draw-path wire-1 [0 0])
        p1 (draw-path wire-2 [0 0])
        p0-idx (map-indexed (fn [idx v] [(inc idx) v]) p0)
        p1-idx (map-indexed (fn [idx v] [(inc idx) v]) p1)
        inter (common-points p0 p1)
        l0s  (sort (fn [[x0 p0] [x1 p1]] (compare p0 p1)) (filter (fn [[idx p]] (inter p)) p0-idx))
        l1s  (sort (fn [[x0 p0] [x1 p1]] (compare p0 p1)) (filter (fn [[idx p]] (inter p)) p1-idx))]

    ;;  [inter l0s l1s]
    (->>
     (map (fn [[l0 p0] [l1 p1]] [(+ l1 l0) p0] ) l0s l1s)
     (map (fn [[l p]] l))
     (apply min))))

(run-part2)
