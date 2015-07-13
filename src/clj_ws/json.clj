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

; {"key": "value"}

; read first token
; { -> start of object
; [ -> start of array
; _ -> start of identifier/simple value

(defn read-char
  [input]
  (Character/valueOf (char (.read input)))
  )

(defn append-char
  [value-builder char-value]
  (.append value-builder char-value)
  (prn (.toString value-builder))
  )

(defn consume-token
  [input depth state json-value value-builder]
  (def next-char (read-char input))
  (prn (str "json-value: " json-value ", in state: " state))
  (case state
    "init" (if (.equals next-char object-start-token-char)
           (consume-token input (+ 1 depth) key-start {} value-builder)

             (if (.equals next-char array-start-token-char)
               (consume-token input depth array-start [] value-builder)



           (throw (RuntimeException. (str "invalid json: " (.toString next-char))))))
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
        (merge json-value {identifier
                           (consume-token input depth value-start {} value-builder)})
        )
      )
    "value-start"
    (if-not (.equals next-char object-end-token-char)

      (do
        (append-char value-builder next-char)
        (consume-token input depth value-start json-value value-builder)
        )

      (do
        (prn (str "value is " (.toString value-builder)))
        (def value (.toString value-builder))
        (.setLength value-builder 0)
        value

        )
      )

  ))

;(prn (str "got me a token: " value-builder))

(defn parse-json
  [input-source handler-func]
  (let [input (clojure.java.io/reader input-source)]
    (def json-result (consume-token input 0 init nil (StringBuilder. "")))
    (prn (str "output from parser: " json-result))
    (handler-func json-result)
    ))