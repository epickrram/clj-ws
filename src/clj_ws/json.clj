(ns
  ^{:author mark}
  clj-ws.json)

(def object-start-token-char (Character/valueOf (.charAt "{" 0)))
(def object-end-token-char (Character/valueOf (.charAt "}" 0)))
(def array-start-token-char (Character/valueOf (.charAt "[" 0)))
(def array-end-token-char (Character/valueOf (.charAt "]" 0)))
(def identifier-delimiter-token-char (Character/valueOf (.charAt ":" 0)))
(def quote-char (Character/valueOf (.charAt "\"" 0)))
(def backslash (Character/valueOf (.charAt "\\" 0)))
(def array-element-delimiter-char (Character/valueOf (.charAt "," 0)))
(def null-char (Character/valueOf (.charAt "\u0001" 0)))
(def integer-pattern (re-pattern "^[0-9\\-]+$"))
(def double-pattern (re-pattern "^[0-9\\.\\-]+$"))
(def whitespace-pattern (re-pattern "[\\s]+"))
(def string-pattern (re-pattern "^\".*\"$"))

(defn read-char
  [input]
  (def int-value (.read input))
  (if (== -1 int-value)
    null-char
    (Character/valueOf (char int-value)))
  )


(defn typed-json-value
  [string-rep]
  (if (re-matches integer-pattern string-rep)
    (Long/parseLong string-rep)
    (if (re-matches double-pattern string-rep)
      (Double/parseDouble string-rep)
      (if (re-matches string-pattern string-rep)
        (.substring string-rep 1 (- (.length string-rep) 1))
        string-rep
        )
      )
    )
  )


(defn match-char
  [expected actual]
  (.equals expected actual)
  )


(defn is-delimiter-char
  [char-to-test]
  (or
    (match-char char-to-test array-element-delimiter-char)
    (match-char char-to-test array-end-token-char)
    (match-char char-to-test object-end-token-char)
    (match-char char-to-test identifier-delimiter-token-char)
    (match-char char-to-test null-char))
  )

'; exits on delimiters ,:], etc
(defn identifier-terminator
  [input]
  )

(defn last-char
  [value]
  (def value-length (.length (.trim value)))
  (if (= value-length 0)
    null-char
    (do
      (prn (str "substring(" (- value-length 1) ") of " value))
      (prn (str "last-char: '" (.substring value (- value-length 1)) "'"))

      (Character/valueOf (.charAt (.substring value (- value-length 1)) 0))
      )
    ))

(defn is-currently-quoted
  [next-char is-quoted]
  (if (and is-quoted (not (= next-char quote-char)))
    is-quoted
    (if (and (not is-quoted) (= next-char quote-char))
      true
      false
      )
    )
  )

(defn consume-scalar
  [input accumulator previous-char-was-backslash is-quoted]
  (def next-char (read-char input))
  (prn (str "consume-scalar, next-char: '" next-char "', prev backslash: "
         previous-char-was-backslash ", accumulator: '" accumulator "', is-quoted: " is-quoted))
  (if (and (is-delimiter-char next-char) (not is-quoted))
    accumulator
    (if (= 0 (.length (.trim accumulator)))
      (do
        (def char-for-next
          (if (= backslash next-char)
            ""
            next-char
            )
          )
        (str char-for-next (consume-scalar input "" (= backslash next-char) (is-currently-quoted next-char is-quoted)))
        )

      (do
        (def char-to-pass
          (if (= next-char backslash)
            ""
            next-char
            ))
        (prn (str "char-to-pass: '" char-to-pass "'"))
        (str accumulator char-to-pass (consume-scalar input "" (= backslash next-char) (is-currently-quoted next-char is-quoted)))
        )
      )
    )
  )


(defn read-value
  [input accumulator]
  (prn (str "read-value accumulator: '" accumulator "'"))
  (def is-quoted (= (Character/valueOf (.charAt accumulator 0)) quote-char))
  (consume-scalar input accumulator (= backslash (last-char accumulator)) is-quoted)
  )

; TODO case statement for state

(defn pj
  [input-source state accumulator]
  (def input (clojure.java.io/reader input-source))
  (def next-char (read-char input))
  (if (= state nil)
    (if (match-char next-char array-start-token-char)
      (pj input "array" [])
      (if (match-char next-char object-start-token-char)
        (pj input "object" {})
        (if (re-matches whitespace-pattern (.toString next-char))
          (pj input-source, state, accumulator)
          (typed-json-value (read-value input (.toString next-char)))
          )

        )
      )
    (if (= state "array")
      (if (is-delimiter-char next-char)

        accumulator

        (if (re-matches whitespace-pattern (.toString next-char))
          (pj input "array" accumulator)
          (if (match-char next-char array-element-delimiter-char)
            (concat accumulator [(pj input "array" accumulator)])
            (if (match-char next-char object-start-token-char)
              (do
                (concat accumulator [(pj input "object" {})] (pj input "array" []))
                )
              (do
                (concat accumulator [(typed-json-value (read-value input (.toString next-char)))] (pj input "array" [])))
              )
            )
          )
        )
      (if (= state "object")
        (if (is-delimiter-char next-char)
          accumulator

          (do
            (def map-key (typed-json-value (read-value input (.toString next-char))))
            (def updated-map (merge accumulator {map-key (pj input nil nil)}))
            (pj input "object" updated-map)
            )
          )
        )
      )
    )
  )