(ns day07-2
  (:require [clojure.java.io :as io]
            [clojure.core.async :as async]
            [clojure.math.combinatorics :as comb]))

(defn read-program []
  (let [p-str (slurp (io/resource "day07-2.txt"))]
    (read-string (str "[" p-str "]"))))

(def prog (read-program))
(defn opcode [op]
  (mod op 100))

(def def-op nil)
(defmulti do-op (fn [op _ _] (opcode op)))

(defmethod do-op 1 [_  [arg0 arg1] _]
  {:store (+ arg0 arg1)
   :ip #(+ 4 %)})

(defmethod do-op 2 [_ [arg0 arg1] _]
  {:store (* arg0 arg1)
   :ip #(+ 4 %)})

(defmethod do-op 3 [_ _ {:keys [ch-in]}]
  (let [i (async/<!! ch-in)]
    {:store i
     :ip #(+ 2 %)}))

(defmethod do-op 4 [_ [arg0] {:keys [ch-out]}]
  (async/put! ch-out arg0)
  {:ip #(+ 2 %)})

(defmethod do-op 5 [_ [cond ip] _]
  (if (zero? cond)
    {:ip #(+ 3 %)}
    {:ip (fn [_] ip)}))

(defmethod do-op 6 [_ [cond ip] _]
  (if (zero? cond)
    {:ip (fn [_] ip)}
    {:ip #(+ 3 %)}))

(defmethod do-op 7 [_ [arg0 arg1] _]
  (if (< arg0 arg1)
    {:store 1
     :ip #(+ 4 %)}
    {:store 0
     :ip #(+ 4 %)}))

(defmethod do-op 8 [_ [arg0 arg1] _]
  (if (= arg0 arg1)
    {:store 1
     :ip #(+ 4 %)}
    {:store 0
     :ip #(+ 4 %)}))

(defmethod do-op 99 [_ _ _]
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

(defn exec-prog-step [{:keys [ip prog ctx]}]
  (let [op (get prog ip)
        [args store] (get-args op prog ip)
        state (do-op op args ctx)
        res (:store state)]

    {:prog (if res (assoc prog store res) prog)
     :ip ((:ip state) ip)
     :ctx ctx}))

(defn intcode [ctx]
  (let [pr  {:prog prog
             :ip 0
             :ctx ctx}]
    (loop [pr-step pr]
      (let [pr-step* (exec-prog-step pr-step)]
        (if (:ip pr-step*)
          (recur pr-step*)
          pr-step*)))))

(defn amplifier [ctx]
  (future
    (intcode ctx)
    (async/<!! (:ch-out ctx))))

(defn run [phases]
  (let [ch-a (async/chan)
        ch-b (async/chan)
        ch-c (async/chan)
        ch-d (async/chan)
        ch-e (async/chan)
        a (amplifier {:name "A" :ch-in ch-a :ch-out ch-b})
        b (amplifier {:name "B" :ch-in ch-b :ch-out ch-c})
        c (amplifier {:name "C" :ch-in ch-c :ch-out ch-d})
        d (amplifier {:name "D" :ch-in ch-d :ch-out ch-e})
        e (amplifier {:name "E" :ch-in ch-e :ch-out ch-a})]
    (async/put! ch-a (get phases 0))
    (async/put! ch-b (get phases 1))
    (async/put! ch-c (get phases 2))
    (async/put! ch-d (get phases 3))
    (async/put! ch-e (get phases 4))
    (async/put! ch-a 0)
    @e))

(->>
 (comb/permutations [5 6 7 8 9])
 (map run)
 (apply max))
