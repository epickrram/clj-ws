(ns
  ^{:author mark}
  clj-ws.json)

(def init "init")
(def object-start "object-start")
(def key-start "key-start")
(def value-start "value-start")
(def array-start "array-start")
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

; {"key": "value"}

; read first token
; { -> start of object
; [ -> start of array
; _ -> start of identifier/simple value

(defn read-char
  [input]
  (def int-value (.read input))
  (if (== -1 int-value)
    null-char
    (Character/valueOf (char int-value)))

  )

(defn append-char
  [value-builder char-value]
  (.append value-builder char-value)
  ;  (prn (.toString value-builder))
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

(defn consume-token
  [input depth state json-value value-builder]
  (def next-char (read-char input))
  (prn (str "json-value: " json-value ", in state: " state ", next-char: " next-char ", value-builder: " (.toString value-builder)))
  (case state
    "init" (if (.equals next-char object-start-token-char)
             (consume-token input (+ 1 depth) key-start {} value-builder)

             (if (.equals next-char array-start-token-char)
               (consume-token input depth array-start [] value-builder)
               (throw (RuntimeException. (str "invalid json: " (.toString next-char))))))
    "array-start"
    (if-not (.equals next-char array-end-token-char)
      (do
        (if-not (.equals next-char array-element-delimiter-char)
          (do
            (append-char value-builder next-char)
            (consume-token input depth array-start json-value value-builder)
            )

          (do
            (def array-element (typed-json-value (.toString value-builder)))
            (.setLength value-builder 0)
            (def updated-vector (concat json-value [array-element]))
            (consume-token input depth array-start updated-vector value-builder)
            updated-vector
            )
          )
        )
      (do
        (def array-element (.toString value-builder))
        (.setLength value-builder 0)
        (def updated-vector (concat json-value [(typed-json-value array-element)]))
        (prn "returning json array value: " updated-vector)
        updated-vector
        )

      )
    "key-start"
    (if-not (.equals next-char identifier-delimiter-token-char)

      ; keep building key
      (do
        ; ignore quote chars
        (if-not (.equals next-char quote-char) (append-char value-builder next-char))

        (consume-token input depth key-start json-value value-builder)
        )

      ; start building value
      (do
        (def identifier (.toString value-builder))
        (.setLength value-builder 0)
        ; todo - should pass new map?
        (def latest-view (merge json-value {identifier
                                            (consume-token input depth init json-value value-builder)}))
        ;        (consume-token input depth key-start latest-view value-builder)

        latest-view

        )
      )
    "value-start"
    (if (.equals next-char object-end-token-char)
      (do
        (def value (.toString value-builder))
        (.setLength value-builder 0)
        (typed-json-value value)

        )
      (if (.equals next-char array-element-delimiter-char)
        (do
          (prn "deal with end of value")
          (def value (.toString value-builder))
          (.setLength value-builder 0)
          (def typed-val (typed-json-value value))
          ;          (consume-token input depth key-start (merge json-value {}))
          typed-val
          )
        (do
          (append-char value-builder next-char)
          (consume-token input depth value-start json-value value-builder)
          ))

      )

    ))

;

;(prn (str "got me a token: " value-builder))

(defn parse-json
  [input-source handler-func]
  (let [input (clojure.java.io/reader input-source)]
    (def json-result (consume-token input 0 init nil (StringBuilder. "")))
    (prn (str "output from parser: " json-result))
    (handler-func json-result)
    ))

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

(defn debug-consume-scalar
  [input accumulator]
  (def value (consume-scalar input accumulator))
  (prn (str "scalar: " value))

  value
  )

(defn consume-end-of-object-token
  [input]
  (def next-char (read-char input))
  (if (not (or (match-char next-char object-end-token-char) (match-char next-char null-char) (match-char next-char array-element-delimiter-char)))
    (consume-end-of-object-token input)
    )
  )

(defn pj
  [input-source state accumulator]
  (def input (clojure.java.io/reader input-source))
  (def next-char (read-char input))

  (prn (str "state: " state ", char: " next-char " - " accumulator))

  (if (= state nil)
    (if (match-char next-char array-start-token-char)
      (pj input "array" [])
      (if (match-char next-char object-start-token-char)
        (pj input "object" {})
        (typed-json-value (debug-consume-scalar input (.toString next-char)))
        )
      )
    (if (= state "array")
      (if (match-char next-char null-char)

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
                (concat accumulator [(typed-json-value (debug-consume-scalar input (.toString next-char)))] (pj input "array" [])))
              )
            )
          )
        )
      (if (= state "object")
        (if (or (match-char next-char null-char) (match-char next-char array-element-delimiter-char) (match-char next-char array-end-token-char))
            accumulator

          (do
            (def map-key (typed-json-value (debug-consume-scalar input (.toString next-char))))
            (def updated-map (merge accumulator {map-key (pj input nil nil)}))
            (pj input "object" updated-map)
            )
          )
        )
      )
    )
  )