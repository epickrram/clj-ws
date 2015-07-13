(ns
  ^{:author mark}
  clj-ws.json-test
  (:require [clojure.test :refer :all]
            [clj-ws.json :refer :all]))

(defn get-json-result-handler
  [expected]
  (fn [json-result]
    (println "json result keys")
    (doseq [keyval json-result]
      (prn (get keyval 0))
      (prn (get keyval 1))
      )
    (println "expected keys")
    (doseq [keyval expected]
      (prn (get keyval 0))
      (prn (get keyval 1))
      (is
        (and
          (not (nil? (get expected (get keyval 0))))
          (is (== (get json-result (get keyval 1)) (get expected (get keyval 1))))
          )
        )
      )

    )
  )

(defn validate-json-parsing
  [serialised-json expected-output]
  (parse-json (new java.io.StringReader serialised-json) (get-json-result-handler expected-output))
  )

(deftest parse-simple-json-object
  (testing "Parse single json object"
    (validate-json-parsing "{\"1\":2}" {:1 2})
    ))

;(deftest parse-simple-json-array
;  (testing "Parse single json array"
;    (validate-json-parsing "[1,2,3,4]" [1,2,3,4])
;    ))
;
;(deftest parse-complex-single-depth-json-object
;  (testing "Parse complex single-depth json object"
;    (validate-json-parsing "{\"foo\": 1234, \"bar\": [1,23,4,6]" {:foo "bar" :bar [1, 23, 4, 6]})
;    ))
;
;(deftest parse-complex-nested-json-object
;  (testing "Parse complex nested json object"
;    (validate-json-parsing "{\"foo\": 1234, \"bar\": {\"nest\": 1.23456}" {:foo "bar" :bar {:nest 1.23456}})
;    ))
;
;(deftest whitespace
;  (testing "Strip whitespace"
;    (validate-json-parsing "{\"foo\"   :    1234  ,     \"bar\"    :    {    \"nest\"   :  1.23456   }"
;      {:foo "bar" :bar {:nest 1.23456}})
;    ))
;
;(deftest handle-unquoted-identifiers
;  (testing "Parse single json object"
;    (validate-json-parsing "{\"1\":2}" {:1 2})
;    ))