(ns day08
  (:require [clojure.java.io :as io]
            [clojure.core.matrix :as mat]
            [clojure.string :as str]))

(def input (slurp (io/resource "day08.txt")))

(defn decode [str]
  (->> str
       (map #(- (int %) (int \0)))
       (partition (* 25 6))))

(defn m12 [x]
  (* (get x 1) (get x 2)))

(def part1
  (->> input
       decode
       (map frequencies)
       (sort (fn [a b] (compare (get a 0) (get b 0))))
       first
       m12))

(defn visibility [channels]
  (first (drop-while (partial = 2) channels)))

(defn draw-pixel [p]
  (get {0 " " 1 "*"} p))

(defn draw-line [l]
  (map draw-pixel l))

(defn draw [pic]
  (->>
   pic
   (map #(apply str %))
   (str/join "\n")))

(def part2
  (->> input
       decode
       mat/transpose
       (map visibility)
       (map draw-pixel)
       (partition 25)
       draw
       println))
