(ns cljzork.memory)

(defn read-u16 [memory offset]
  (bit-and (bit-or
            (int (bit-shift-left (nth memory offset) 8))
            (int (nth memory (inc offset))))
           0xffff))

(defn read-u8 [memory offset]
  (bit-and (int (nth memory offset)) 0xff))