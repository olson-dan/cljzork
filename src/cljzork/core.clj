(ns cljzork.core
  (:require [clojure.java.io]
            [cljzork.instructions :as instructions])
  (:gen-class))

(defn read-u16 [memory offset]
  (bit-and (bit-or
            (int (bit-shift-left (nth memory offset) 8))
            (int (nth memory (inc offset))))
           0xffff))

(defn read-u8 [memory offset]
  (bit-and (int (nth memory offset)) 0xff))

(defn create-header [memory]
  {:dynamic_start 0
   :globals (read-u16 memory 0xc)})

(defn slurp-bytes
  "Slurp the bytes from a slurpable thing (https://stackoverflow.com/a/26372677)"
  [x]
  (with-open [out (java.io.ByteArrayOutputStream.)]
    (clojure.java.io/copy (clojure.java.io/input-stream x) out)
    (.toByteArray out)))

(defn create-machine [filename]
  (let [memory (slurp-bytes filename)]
    {:memory memory
     :header (create-header memory)
     :finished false
     :ip (read-u16 memory 6)}))

(defn decode-var-args [optypes index size args]
  (let [shift (* (- 3 index) 2)
        mask (bit-shift-left 3 shift)
        optype (bit-shift-right (bit-and optypes mask) shift)]
    (if (= index 3)
      (conj args optype)
      (recur optypes (inc index) size (conj args optype)))))

(defn decode-var [memory offset op]
  (let [opcode (bit-and op 0x1f)
        optype (if (zero? (bit-and op 0x20)) :op2 :var)
        name (instructions/get-name optype opcode)
        optypes (read-u8 memory (inc offset))
        args (decode-var-args optypes 0 2 [])
        args (filterv (comp (partial not= 3)) args)]
    (println (format "[%08X]" offset) name args)
    {:offset offset
     :name name
     :args args
     :ret :omitted}))

(defn decode-short [memory offset op]
  (println "decode-short" op))

(defn decode-long [memory offset op]
  (println "decode-long" op))

(defn decode [memory offset]
  (let [op (read-u8 memory offset)
        type (unsigned-bit-shift-right (bit-and op 0xc0) 6)]
    (cond
      (= type 3) (decode-var memory offset op)
      (= type 2) (decode-short memory offset op)
      :default (decode-long memory offset op))))

(defn execute [instruction machine]
  (assoc machine :finished true))

(defn step-machine [machine]
  (println (:ip machine))
  (execute (decode (:memory machine) (:ip machine)) machine))

(defn run-machine [machine]
  (loop [machine machine]
    (if-not (:finished machine)
      (recur (step-machine machine))
      nil)))

(defn -main [& args]
  (run-machine (create-machine (first args))))