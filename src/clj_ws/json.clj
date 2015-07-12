(ns
  ^{:author mark}
  clj-ws.json)

(def init "init")
(def object-start "object-start")
(def key-start "key-start")
(def value-start "value-start")
(def object-start-token-char (Character/valueOf (.charAt "{" 0)))
(def object-end-token-char (Character/valueOf (.charAt "}" 0)))
(def identifier-delimiter-token-char (Character/valueOf (.charAt ":" 0)))

; {"key"; "value"}

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
  [input depth state json-value value-builder handler-func]
  (def next-char (read-char input))
  (prn (str "json-value: " json-value ", in state: " state))
  (case state
    "init" (if (.equals next-char object-start-token-char)
           (consume-token input (+ 1 depth) key-start {} value-builder handler-func)
           (throw (RuntimeException. (str "invalid json: " (.toString next-char)))))
    "key-start"
    (if-not (.equals next-char identifier-delimiter-token-char)

      ; keep building key
      (do
        (append-char value-builder next-char)
        (consume-token input depth key-start json-value value-builder handler-func)
        )

      ; start building value
      ; TODO - this should call a function to return the value, then put it in the map
      (do
        (def updated-value (merge json-value {(.toString value-builder) nil}))
        (.setLength value-builder 0)
        (consume-token input depth value-start updated-value value-builder handler-func)

        )
      )
    "value-start"
    (if-not (.equals next-char object-end-token-char)

      (do
        (append-char value-builder next-char)
        (consume-token input depth value-start json-value value-builder handler-func)
        )

      (do
;        (def updated-value (merge json-value {(.toString value-builder) nil}))
        (prn (str "value is " (.toString value-builder)))
        (.setLength value-builder 0)
        (consume-token input depth value-start json-value value-builder handler-func)

        )
      )

  ))

;(prn (str "got me a token: " value-builder))

(defn parse-json
  [input-source handler-func]
  (let [input (clojure.java.io/reader input-source)]
    (consume-token input 0 init nil (StringBuilder. "") handler-func)
    ))