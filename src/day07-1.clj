(ns day07-1
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [clojure.math.combinatorics :as comb]))


(def input (atom []))

(def output (atom []))

(defn apop! [a]
  (let [i (peek @a)]
    (println "pop: " i " from " a)
    (swap! a pop)
    i))

(defn apush! [a v]
  (println "push: " v " to " a)
  (swap! a conj v))

(defn read-program []
  (let [p-str (slurp (io/resource "day07.txt"))]
    (read-string (str "[" p-str "]"))))

(def prog (read-program))
(defn opcode [op]
  (mod op 100))

(def def-op nil)
(defmulti do-op (fn [op _] (opcode op)))

(defmethod do-op 1 [_  [arg0 arg1]]
  {:store (+ arg0 arg1)
   :ip #(+ 4 %)})

(defmethod do-op 2 [_ [arg0 arg1]]
  {:store (* arg0 arg1)
   :ip #(+ 4 %)})

(defmethod do-op 3 [_ _]
  (let [i (apop! input)]
    {:store i
     :ip #(+ 2 %)}))

(defmethod do-op 4 [_ [arg0]]
  (apush! output arg0)
  {:ip #(+ 2 %)})

(defmethod do-op 5 [_ [cond ip]]
  (if (zero? cond)
    {:ip #(+ 3 %)}
    {:ip (fn [_] ip)}))

(defmethod do-op 6 [_ [cond ip]]
  (if (zero? cond)
    {:ip (fn [_] ip)}
    {:ip #(+ 3 %)}))

(defmethod do-op 7 [_ [arg0 arg1]]
  (if (< arg0 arg1)
    {:store 1
     :ip #(+ 4 %)}
    {:store 0
     :ip #(+ 4 %)}))

(defmethod do-op 8 [_ [arg0 arg1]]
  (if (= arg0 arg1)
    {:store 1
     :ip #(+ 4 %)}
    {:store 0
     :ip #(+ 4 %)}))

(defmethod do-op 99 [_ _]
  {:ip (fn [_] nil)})

(defn arg-count [op]
  ({1 2
    2 2
    3 0
    4 1
    5 2
    6 2
    7 2
    8 2
    99 0} (opcode op)))

(defn param-mode [op pos]
  (let [mode (Integer/parseInt (str (quot op 100)) 2)]
    (pos? (bit-and mode (int (java.lang.Math/pow 2 pos))))))

(defn get-args [op prog ip]
  (let [arg-count (arg-count op)
        args (->> prog
                  (split-at (inc ip))
                  second
                  (split-at arg-count)
                  first)]
    [(for [[a i] (map vector args (range))]
       (if (param-mode op i)
           a
           (get prog a)))
     (get prog (+ ip arg-count 1))]))

(defn exec-prog-step [{:keys [ip prog]}]
  (let [op (get prog ip)
        [args store] (get-args op prog ip)
        state (do-op op args)
        res (:store state)]

    {:prog (if res (assoc prog store res) prog)
     :ip ((:ip state) ip)}))

(defn intcode []
  (let [pr  {:prog (read-program)
             :ip 0}]
    (loop [pr-step pr]
      (let [pr-step* (exec-prog-step pr-step)]
        (if (:ip pr-step*)
          (recur pr-step*)
          pr-step*)))))

(defn run-amplifier [phase signal]
  (apush! input phase)
  (apush! input signal)
  (intcode)
  (apop! output))

(run-amplifier 4 0)

(defn run-amp-seq [phase-setting-sequence]
  (loop [sig 0
         pss phase-setting-sequence
         aggr 0]
    (if (seq pss)
      (let [out (run-amplifier sig (first pss))]
         (println sig (first pss) " -> " out)
         (recur out (rest pss) out))
      aggr)))

(run-amp-seq [4 3 2 1 0])

(->>
 (comb/permutations [0 1 2 3 4])
 (map run-amp-seq)
 (apply max))

