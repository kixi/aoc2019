(ns day02
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]))

(defn read-program []
  (let [p-str (slurp (io/resource "day02k.txt"))]
    (read-string (str "[" p-str "]"))))

(defmulti do-op (fn [op _ _] op))

(defmethod do-op 1 [_ arg0 arg1]
  (+ arg0 arg1))

(defmethod do-op 2 [_ arg0 arg1]
  (* arg0 arg1))

(defmethod do-op 99 [_ _ _]
  nil)

(defn exec-prog-step [{:keys [ip prog]}]
  (let [op (get prog ip)
        arg0 (get prog (get prog (inc ip)))
        arg1 (get prog (get prog (+ 2 ip)))
        residx (get prog (+ 3 ip))

        res (do-op op arg0 arg1)]
    (if res
      {:prog (assoc prog residx (do-op op arg0 arg1))
       :ip (+ 4 ip)}
      {:prog prog
       :ip nil})))

(deftest t0
  (is (= {:ip 4 :prog [2 0 0 0 99]} (exec-prog-step {:ip 0 :prog [1, 0, 0, 0, 99]})))
  (is (= {:ip 4 :prog [2 3 0 6 99]} (exec-prog-step {:ip 0 :prog [2, 3, 0, 3, 99]}))))

(defn restore-state [prog]
  (-> prog
      (assoc 1 12)
      (assoc 2 2)))

(defn run-part-1 []
  (let [pr  {:prog (restore-state (read-program))
             :ip 0}]
    (loop [pr-step pr]
      (let [pr-step* (exec-prog-step pr-step)]
        (if (:ip pr-step*)
          (recur pr-step*)
          pr-step*)))))

(run-part-1)

(defn set-state [prog noun verb]
  (-> prog
      (assoc 1 noun)
      (assoc 2 verb)))


(defn run-program [noun verb]
  (let [pr  {:prog (set-state (read-program) noun verb)
             :ip 0}]
    (loop [pr-step pr]
      (let [pr-step* (exec-prog-step pr-step)]
        (if (:ip pr-step*)
          (recur pr-step*)
          pr-step*)))))


(defn find-input  []
  (for [noun (range 100)
        verb (range 100)]
    [noun verb (get-in (run-program noun verb) [:prog 0])]))

(filter (fn [[noun verb result]] (= result 19690720)) (find-input))
