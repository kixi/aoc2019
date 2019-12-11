(ns intcode
  (:require [clojure.core.async :as async]))

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
;;  (println "intcode waiting for input")
  (let [i (async/<!! ch-in)]
;;    (println "intcode << " i)
    {:store i
     :ip #(+ 2 %)}))

(defmethod do-op 4 [_ [arg0] {:keys [ch-out]}]
;;  (println "intcode >> " arg0)
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
 ;;       _ (println "Exec: ip:" ip "op:" op "args:" args "store:" store)
        state (do-op op args ctx)
        res (:store state)]
    {:prog (if res (assoc prog store res) prog)
     :ip ((:ip state) ip)
     :memory ((or (:memory state) identity) memory)
     :ctx ctx}))

(defn parse-prog [prog]
  (into {}
        (map-indexed
         (fn [idx v] [idx v])
         prog)))

(defn intcode [ctx prog]
  (let [pr  {:prog (parse-prog prog)
             :ip 0
             :memory 0
             :ctx ctx}]
    (println "Starting programm" ctx)
    (println "Initial prog" prog)
    (loop [pr-step pr]
      (let [pr-step* (exec-prog-step pr-step)]
        (if (:ip pr-step*)
          (recur pr-step*)
          pr-step*)))))

(defn run-intcode [ctx prog]
  (future
    (intcode ctx prog)))
