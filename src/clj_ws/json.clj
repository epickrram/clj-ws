(ns
  ^{:author mark}
  clj-ws.json)

(defn str-to-char
  [str]
  (Character/valueOf (.charAt str 0))
  )

(def object-start-token-char (str-to-char "{"))
(def object-end-token-char (str-to-char "}"))
(def array-start-token-char (str-to-char "["))
(def array-end-token-char (str-to-char "]"))
(def identifier-delimiter-token-char (str-to-char ":"))
(def quote-char (str-to-char "\""))
(def backslash (str-to-char "\\"))
(def array-element-delimiter-char (str-to-char ","))
(def null-char (str-to-char "\u0001"))
(def integer-pattern (re-pattern "^[0-9\\-]+$"))
(def double-pattern (re-pattern "^[0-9\\.\\-]+$"))
(def whitespace-pattern (re-pattern "[\\s]+"))
(def string-pattern (re-pattern "^\".*\"$"))

(keyword :object)
(keyword :array)

(defn read-char
  [input]
  (def int-value (.read input))
  (if (= -1 int-value)
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

(defn last-char
  [value]
  (def value-length (.length (.trim value)))
  (if (= value-length 0)
    null-char
    (str-to-char (.substring value (- value-length 1)))
    )
  )

(defn is-currently-quoted
  [next-char is-quoted]
  (if (and is-quoted (not (= next-char quote-char)))
    is-quoted
    (and (not is-quoted) (= next-char quote-char))
    )
  )

(defn consume-scalar
  [input accumulator previous-char-was-backslash is-quoted]
  (def next-char (read-char input))
  (if (and (is-delimiter-char next-char) (not is-quoted))
    accumulator
    (do
      (def value-to-prepend
        (if (= 0 (.length (.trim accumulator)))
          (if (= backslash next-char)
            ""
            next-char
            )
          (if (= backslash next-char)
            accumulator
            (str accumulator next-char)
            ))
        )
      (str value-to-prepend (consume-scalar input "" (= backslash next-char) (is-currently-quoted next-char is-quoted)))
      )
    )
  )

(defn read-value
  [input accumulator]
  (def is-quoted (= (str-to-char accumulator) quote-char))
  (consume-scalar input accumulator (= backslash (last-char accumulator)) is-quoted)
  )

(defn pj
  [input-source state accumulator]
  (def input (clojure.java.io/reader input-source))
  (def next-char (read-char input))

  (case state

    nil
    (if (match-char next-char array-start-token-char)
      (pj input :array [])
      (if (match-char next-char object-start-token-char)
        (pj input :object {})
        (if (re-matches whitespace-pattern (.toString next-char))
          (pj input-source, state, accumulator)
          (typed-json-value (read-value input (.toString next-char)))
          )

        )
      )

    :array
    (if (is-delimiter-char next-char)

      accumulator

      (if (re-matches whitespace-pattern (.toString next-char))
        (pj input :array accumulator)
        (if (match-char next-char array-element-delimiter-char)
          (concat accumulator [(pj input :array accumulator)])
          (if (match-char next-char object-start-token-char)
            (concat accumulator [(pj input :object {})] (pj input :array []))
            (concat accumulator [(typed-json-value (read-value input (.toString next-char)))] (pj input :array [])))
          )
        )
      )

    :object
    (if (is-delimiter-char next-char)
      accumulator

      (do
        (def map-key (typed-json-value (read-value input (.toString next-char))))
        (def updated-map (merge accumulator {map-key (pj input nil nil)}))
        (pj input :object updated-map)
        )
      )
    )
  )
