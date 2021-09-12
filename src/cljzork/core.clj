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

(defn var-arg-size [optype]
  (cond
    (= optype 3) 0
    (= optype 2) 1
    (= optype 1) 1
    :default 0))

(defn decode-arg-value [optype memory offset size]
  (let [offset (+ offset size)]
    (cond
      (= optype 3) nil
      (= optype 2) {:type :variable, :value (read-u8 memory offset)}
      (= optype 1) {:type :small, :value (read-u8 memory offset)}
      :default {:type :large, :value (read-u16 memory offset)})))

(defn decode-var-args [optypes memory offset index size args]
  (let [shift (* (- 3 index) 2)
        mask (bit-shift-left 3 shift)
        optype (bit-shift-right (bit-and optypes mask) shift)
        value (decode-arg-value optype memory offset size)
        size (+ size (var-arg-size optype))]
    (if (= index 3)
      {:args (conj args value), :size size}
      (recur optypes memory offset (inc index) size (conj args value)))))

(defn decode-var [memory offset op]
  (let [opcode (bit-and op 0x1f)
        encoding (if (zero? (bit-and op 0x20)) :op2 :var)
        name (instructions/get-name encoding opcode)
        optypes (read-u8 memory (inc offset))
        args (decode-var-args optypes memory offset 0 2 [])
        size (:size args)
        args (filterv (comp not nil?) (:args args))]
    (println (format "[%08X]" offset) name args)
    {:offset offset
     :name name
     :args args
     :size size}))

(defn decode-short [memory offset op]
  (let [optype (bit-shift-right (bit-and op 0x30) 4)
        opcode (bit-and op 0xf)
        encoding (if (= optype 3) :op0 :op1)
        name (instructions/get-name encoding opcode)
        value (decode-arg-value optype memory offset 1)
        size (if (= (:type value) :large) 3 2)
        args (filterv (comp not nil?) (conj [] value))]
    (println (format "[%08X]" offset) name args)
    {:offset offset
     :name name
     :args args
     :size size}))

(defn decode-long [memory offset op]
  (let [opcode (bit-and op 0x1f)
        name (instructions/get-name :op2 opcode)
        x (decode-arg-value (if (zero? (bit-and op 0x40)) 1 2) memory offset 1)
        y (decode-arg-value (if (zero? (bit-and op 0x20)) 1 2) memory offset 2)
        args (conj [] x y)]
    (println (format "[%08X]" offset) name args)
    {:offset offset
     :name name
     :args args
     :size 3}))

(defn decode-return [instruction])

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