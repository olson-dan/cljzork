(ns cljzork.core
  (:require [clojure.java.io]
            [cljzork.decode :as decode]
            [cljzork.memory :as mem])
  (:gen-class))


(defn create-header [memory]
  {:dynamic_start 0
   :globals (mem/read-u16 memory 0xc)})

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
     :ip (mem/read-u16 memory 6)}))

(defn print-instruction [i]
  (println (format "[%08X]" (:offset i)) (:name i) (:args i) (:ret i)))

(defn execute [instruction machine]
  (assoc machine :finished true))

(defn step-machine [machine]
  (let [instruction (decode/decode (:memory machine) (:ip machine))]
    (print-instruction instruction)
    (execute instruction machine)))

(defn run-machine [machine]
  (loop [machine machine]
    (if-not (:finished machine)
      (recur (step-machine machine))
      nil)))

(defn -main [& args]
  (run-machine (create-machine (first args))))