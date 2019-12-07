(ns day05
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]))


(def input (atom [5]))

(def output (atom []))

(defn read-program []
  (let [p-str (slurp (io/resource "day05.txt"))]
    (read-string (str "[" p-str "]"))))

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
  (let [i (first @input)]
    (swap! input rest)
    (println "input: " i " new input " @input)
    {:store i
     :ip #(+ 2 %)}))

(defmethod do-op 4 [_ [arg0]]
  (swap! output conj arg0)
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
 ;;   (println "Args" op ip arg-count args)
    [(for [[a i] (map vector args (range))]
       (if (param-mode op i)
           a
           (get prog a)))
     (get prog (+ ip arg-count 1))]))

(defn exec-prog-step [{:keys [ip prog]}]
  (let [op (get prog ip)
        _ (println "EXEC: " op ip)
        [args store] (get-args op prog ip)
        _ (println "step 0: ip: " ip "op: " op "args: " args "store:" store)
        state (do-op op args)
        res (:store state)]

    (println "step 1:" state)
    {:prog (if res (assoc prog store res) prog)
     :ip ((:ip state) ip)}))

(deftest t0
  (is (= {:ip 4 :prog [2 0 0 0 99]} (exec-prog-step {:ip 0 :prog [1, 0, 0, 0, 99]})))
  (is (= {:ip 4 :prog [2 3 0 6 99]} (exec-prog-step {:ip 0 :prog [2, 3, 0, 3, 99]}))))

(defn restore-state [prog]
  (-> prog
      (assoc 1 12)
      (assoc 2 2)))

(defn run-part-1 []
  (let [pr  {:prog (read-program)
             :ip 0}]
    (loop [pr-step pr]
      (let [pr-step* (exec-prog-step pr-step)]
        (if (:ip pr-step*)
          (recur pr-step*)
          pr-step*)))))

(run-part-1)

#_(run-part-1)

;; (defn set-state [prog noun verb]
;;   (-> prog
;;       (assoc 1 noun)
;;       (assoc 2 verb)))


;; (defn run-program [noun verb]
;;   (let [pr  {:prog (set-state (read-program) noun verb)
;;              :ip 0}]
;;     (loop [pr-step pr]
;;       (let [pr-step* (exec-prog-step pr-step)]
;;         (if (:ip pr-step*)
;;           (recur pr-step*)
;;           pr-step*)))))


;; (defn find-input  []
;;   (for [noun (range 100)
;;         verb (range 100)]
;;     [noun verb (get-in (run-program noun verb) [:prog 0])]))

;; (filter (fn [[noun verb result]] (= result 19690720)) (find-input))
