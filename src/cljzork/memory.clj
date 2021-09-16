(ns cljzork.memory)

(defn read-u16 [memory offset]
  (bit-and (bit-or
            (int (bit-shift-left (nth memory offset) 8))
            (int (nth memory (inc offset))))
           0xffff))

(defn read-u8 [memory offset]
  (bit-and (int (nth memory offset)) 0xff))

(defn write-u16 [memory offset value]
  (let [x (unchecked-byte (bit-shift-right value 8))
        y (unchecked-byte (bit-and value 0xff))]
    (assoc memory offset x (inc offset) y)))

(defn write-u8 [memory offset value]
  (assoc memory offset (unchecked-byte value)))