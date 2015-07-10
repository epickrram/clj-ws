(ns
  ^{:author mark}
  clj-ws.json)

(def init "init")
(def object-start "object-start")
(def key-start "key-start")
(def value-start "value-start")

; {"key"; "value"}

(defn parse-json
  [input-source handler-func]
  (let [input (clojure.java.io/reader input-source)]
    (handler-func {:1 "foo"})

    ))