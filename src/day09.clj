(ns day09
  (:require [clojure.java.io :as io]
            [clojure.core.async :as async]))

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

(defmethod do-op 9 [_ [arg0] _]
  {:memory #(+ % arg0)
   :ip #(+ 2 %)})

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
    9 1
    99 0} (opcode op)))

(defn param-mode [op pos]
  (let [mode (Integer/parseInt (str (quot op 100)) 2)]
    (pos? (bit-and mode (int (java.lang.Math/pow 2 pos))))))

(defn mode [op pos]
  (let [mode (into [] (reverse (.substring (format "%5d" op) 0 3)))]
    (case (get mode pos)
      \1 :value
      \2 :relative
      :position)))

(defn get-args [op prog ip memory]
  (let [arg-count (arg-count op)
        args (map (fn [idx]
                    (get prog idx))
                  (range (inc ip)
                         (+ ip arg-count 1)))
        res-args
        (for [[a i] (map vector args (range))]
          (case (mode op i)
            :value a
            :position (or (get prog a) 0)
            :relative (or (get prog (+ a memory)))))]
    [res-args
     (case (mode op arg-count)
       :position (get prog (+ 1 ip arg-count))
       :relative (+ memory (get prog (+ 1 ip arg-count))))]))

(defn print-prog [prog]
  (->>
   (into [] prog)
   (sort (fn [[idx v] [idx2 v2]] (compare idx idx2)))
   (map (fn [[idx v]] v))))

(defn exec-prog-step [{:keys [ip prog memory ctx]}]
  (let [op (get prog ip)
        [args store] (get-args op prog ip memory)
        state (do-op op args ctx)
        res (:store state)]

    {:prog (if res (assoc prog store res) prog)
     :ip ((:ip state) ip)
     :memory ((or (:memory state) identity) memory)
     :ctx ctx}))

(defn intcode [ctx prog]
  (let [pr  {:prog prog
             :ip 0
             :memory 0
             :ctx ctx}]
    (loop [pr-step pr]
      (let [pr-step* (exec-prog-step pr-step)]
        (if (:ip pr-step*)
          (recur pr-step*)
          pr-step*)))))

(defn amplifier [ctx prog]
  (future
    (println "Starting amplifier")
    (intcode ctx prog)
    (println "Stopping amplifier")))
;;(async/<!! (:ch-out ctx))))

(defn run [prog]
  (let [ch-a (async/chan 1000)
        ch-b (async/chan 1000)
        a (amplifier {:name "A" :ch-in ch-a :ch-out ch-b} prog)]
    [ch-a ch-b]))

(defn read-program []
  (let [p-str (slurp (io/resource "day09.txt"))]
    (read-string (str "[" p-str "]"))))

(def p4 (read-program))

(def prog
  (into {}
        (map-indexed
         (fn [idx v] [idx v])
         p4)))

(def cc (run prog))
(def ci (get cc 0))
(def co (get cc 1))

(async/put! ci 2)

(def output-queue (atom []))

(async/go-loop []
  (let [o (async/<! co)]
    (swap! output-queue conj o)
    (when o
      (recur))))

