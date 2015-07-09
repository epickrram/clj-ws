(ns
  ^{:author mark}
  clj-ws.bytes)

(defn copy-array
  [existing new-length existing-offset]
  (let [new-array (byte-array new-length)]
    (System/arraycopy existing 0 new-array 0 existing-offset)
    new-array
    )
  )

(defn get-suitable-array
  [existing existing-offset additional-length]
  (if (> additional-length (- (alength existing) existing-offset))
    (copy-array existing (byte-array (* 2 (alength existing))) existing-offset)
    existing
    )
  )

(defn append-array
  [current current-offset addition addition-offset]
  (if (< addition-offset 0)
    current
    (let [updated (get-suitable-array current current-offset addition-offset)]
      (System/arraycopy addition 0 updated current-offset addition-offset)
      updated
      )
    )

  )