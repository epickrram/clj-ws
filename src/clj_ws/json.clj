(ns
  ^{:author mark}
  clj-ws.json)

(def object-start-token-char (Character/valueOf (.charAt "{" 0)))
(def object-end-token-char (Character/valueOf (.charAt "}" 0)))
(def array-start-token-char (Character/valueOf (.charAt "[" 0)))
(def array-end-token-char (Character/valueOf (.charAt "]" 0)))
(def identifier-delimiter-token-char (Character/valueOf (.charAt ":" 0)))
(def quote-char (Character/valueOf (.charAt "\"" 0)))
(def array-element-delimiter-char (Character/valueOf (.charAt "," 0)))
(def null-char (Character/valueOf (.charAt "\u0001" 0)))
(def integer-pattern (re-pattern "^[0-9\\-]+$"))
(def double-pattern (re-pattern "^[0-9\\.\\-]+$"))
(def whitespace-pattern (re-pattern "[\\s]+"))
(def string-pattern (re-pattern "^\"[^\"]+\"$"))

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

(defn consume-scalar
  [input accumulator]
  (def next-char (read-char input))
  (if (is-delimiter-char next-char)
    accumulator
    (if (= 0 (.length (.trim accumulator)))
      (str next-char (consume-scalar input ""))
      (str accumulator next-char (consume-scalar input ""))
      )
    )
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
          (typed-json-value (consume-scalar input (.toString next-char)))
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
                (concat accumulator [(typed-json-value (consume-scalar input (.toString next-char)))] (pj input "array" [])))
              )
            )
          )
        )
      (if (= state "object")
        (if (is-delimiter-char next-char)
          accumulator

          (do
            (def map-key (typed-json-value (consume-scalar input (.toString next-char))))
            (def updated-map (merge accumulator {map-key (pj input nil nil)}))
            (pj input "object" updated-map)
            )
          )
        )
      )
    )
  )