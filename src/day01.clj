(ns day01
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [clojure.string :as str]))



(defn module-masses []
  (->>
   (slurp (io/resource "day01.txt"))
   (str/split-lines)
   (map read-string)))

(module-masses)

(defn fuel [mass]
  (- (quot mass 3) 2))


(defn day01-part01 []
  (->> (module-masses)
       (map fuel)
       (apply +)))

(day01-part01)

(deftest examples
  (is (= 2 (fuel 12)))
  (is (= 2 (fuel 14)))
  (is (= 654 (fuel 1969)))
  (is (= 33583 (fuel 100756))))

(defn total-fuel [mass]
  (- (apply + (take-while pos? (iterate fuel mass)))
     mass))

#_(take-while pos?  (iterate fuel 1969))

(deftest examples2
  (is (= 2 (total-fuel 14)))
  (is (= 966 (total-fuel 1969)))
  (is (= 50346 (total-fuel 100756))))

(defn day01-part02 []
  (->> (module-masses)
       (map total-fuel)
       (apply +)))


(day01-part02)

