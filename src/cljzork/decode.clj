(ns cljzork.decode
  (:require [cljzork.memory :as mem]
            [cljzork.instructions :as instructions]))

(defn var-arg-size [optype]
  (cond
    (= optype 3) 0
    (= optype 2) 1
    (= optype 1) 1
    :else 2))

(defn decode-arg-value [optype memory offset size]
  (let [offset (+ offset size)]
    (cond
      (= optype 3) nil
      (= optype 2) {:type :variable, :value (mem/read-u8 memory offset)}
      (= optype 1) {:type :small, :value (mem/read-u8 memory offset)}
      :else {:type :large, :value (mem/read-u16 memory offset)})))

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
        optypes (mem/read-u8 memory (inc offset))
        args (decode-var-args optypes memory offset 0 2 [])
        size (:size args)
        args (filterv (comp not nil?) (:args args))]
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
    {:offset offset
     :name name
     :args args
     :size 3}))

(defn decode-return [i memory]
  (if-not (instructions/returns? (:name i))
    i
    (let [ret (mem/read-u8 memory (+ (:offset i) (:size i)))
          size (inc (:size i))]
      (assoc i :ret ret :size size))))

(defn decode-branch [i memory]
  (if-not (instructions/branches? (:name i))
    i
    (let []
      i)))

(declare decode-zstring)

(defn decode-zstring-bytes
  ([memory bytes] (decode-zstring-bytes memory bytes {} 0 ::zero))
  ([memory bytes string offset shift]
   (if (>= offset (count bytes))
     string
     (let [c (nth bytes offset)]
       (cond
         (= c 0) (recur memory bytes
                        (update string :contents str \space)
                        (inc offset)
                        shift)
         (and (> c 0) (< c 4))
         (let [table (mem/read-u16 memory 0x18)
               abbrev (nth bytes (inc offset))
               index (+ (* 32 (dec c)) abbrev)
               str-offset (mem/read-u16 memory (+ (* index 2) table))
               abbrev (decode-zstring memory (* str-offset 2))]
           (recur memory bytes
                  (update string :contents str (:contents abbrev))
                  (+ offset (:size abbrev))
                  shift))
         (= c 4) (recur memory bytes string (inc offset) ::one)
         (= c 5) (recur memory bytes string (inc offset) ::two)
         :else
         (cond
           (= shift ::two)
           (let [utf_char (bit-shift-left (nth bytes (inc offset)) 5)
                 utf_char (bit-or (bit-and (nth bytes (+ offset 2)) 0x1f) utf_char)
                 s (-> (char utf_char) str (.getBytes "UTF-8"))]
             (recur memory bytes (update string :contents str s) (+ offset 3) :zero))
           :else
           (recur memory bytes
                  (update string :contents str
                          (nth
                           (cond
                             (= shift ::zero) "______abcdefghijklmnopqrstuvwxyz"
                             (= shift ::one) "______ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                             (= shift ::two) "______^\n0123456789.,!?_#'\"/\\-:()") c)) (inc offset) ::zero)))))))

(defn decode-zstring
  ([memory offset] (decode-zstring memory offset [] 0))
  ([memory offset bytes index]
   (let [x (mem/read-u16 memory (+ offset index))
         continue (zero? (bit-and x 0x8000))
         a (bit-and (bit-shift-right x 10) 0x1f)
         b (bit-and (bit-shift-right x 5) 0x1f)
         c (bit-and x 0x1f)]
     (println continue (format "%04x" x) a b c)
     (if-not continue
       (decode-zstring-bytes memory bytes)
       (decode-zstring memory offset (conj bytes a b c) (+ index 2))))))

(defn decode-print [i memory]
  (if-not (instructions/prints? (:name i))
    i
    (let [zstring (decode-zstring memory (+ (:offset i) (:size i)))]
      (assoc i :string zstring :size (+ (:size i) (:size zstring))))))

(defn decode-instruction [memory offset]
  (let [op (mem/read-u8 memory offset)
        type (unsigned-bit-shift-right (bit-and op 0xc0) 6)]
    (cond
      (= type 3) (decode-var memory offset op)
      (= type 2) (decode-short memory offset op)
      :else (decode-long memory offset op))))

(defn decode [memory offset]
  (decode-print
   (decode-branch
    (decode-return
     (decode-instruction memory offset)
     memory) memory) memory))