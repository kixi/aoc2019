(ns day04
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn ordered? [pwd]
  (->>
   pwd
   (map int)
   (apply <=)))

(ordered? "1233")

(defn repeating-digits? [pwd]
  (->>
   pwd
   frequencies
   vals
   (filter #(>= % 2))
   seq))


(def valid-password? #(and (ordered? %) (repeating-digits? %)))

(defn run-part1 []
  (->> (range 158126 624575)
       (map str)
       (filter valid-password?)
       count))

(run-part1)


(defn double-digits? [pwd]
  (->>
   pwd
   frequencies
   vals
   (filter #(= % 2))
   seq))

(def valid-password2? #(and (ordered? %) (double-digits? %)))

(defn run-part2 []
  (->> (range 158126 624575)
       (map str)
       (filter valid-password2?)
       count))

(run-part2)
;; 1131

