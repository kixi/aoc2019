(ns day06
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as set]
            [clojure.test :refer [deftest is]]))

(defn read-program []
  (->> (slurp (io/resource "day06.txt"))
       string/split-lines
       (map (fn [s] (string/split s #"\)")))
       (map (fn [[c o]] [o c]))
       (into {})))

(def orbits (read-program))

(defn get-path [obj path]
  (if-let [orb (get orbits obj)]
    (recur orb (conj path orb))
    path))

(defn part1 []
  (->>
   (for [[obj c] orbits]
     (count (get-path obj [])))
   (apply +)))

(part1)

(defn part2 []
  (let [santa (into #{} (get-path "SAN" []))
        you (into #{} (get-path "YOU" []))]
    (-> (set/union (set/difference santa you)
                   (set/difference you santa))
        count)))

(part2)
