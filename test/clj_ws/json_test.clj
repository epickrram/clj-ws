(ns
  ^{:author mark}
  clj-ws.json-test
  (:require [clojure.test :refer :all]
            [clj-ws.json :refer :all]))

(defn validate-json-array
  [json-result expected]
  ;  (prn "expected: " expected (type expected) " as array " (into-array expected))
  ;  (prn "actual: " json-result (type json-result) " as array " (into-array json-result))
  ;  (prn "arrays equal? " (java.util.Arrays/equals (into-array json-result) (into-array expected)))
  (is
    (true? (java.util.Arrays/equals (into-array json-result) (into-array expected)))
    )
  )

(defn validate-json-object
  [json-result expected]
  (println "json result key/vals")
  (doseq [keyval json-result]
    (prn (get keyval 0))
    (prn (get keyval 1))
    )
  (println "expected key/vals")
  (doseq [keyval expected]
    (prn (get keyval 0))
    (prn (get keyval 1))
    (is
      (and
        (not (nil? (get json-result (get keyval 0))))
        (is (true? (.equals (get json-result (get keyval 0)) (get expected (get keyval 0)))))
        )
      )
    )
  )

(defn get-json-result-handler
  [expected]
  (fn [json-result]
    (if (instance? clojure.lang.PersistentArrayMap json-result)
      (validate-json-object json-result expected)
      (validate-json-array json-result expected)
      )

    )
  )

(defn validate-json-parsing
  [serialised-json expected-output]
  (parse-json (new java.io.StringReader serialised-json) (get-json-result-handler expected-output))
  )

(deftest parse-single-element-integer-array
  (testing "single element integer array"
    (is
      (=
        (pj (new java.io.StringReader "[1234]") nil nil)
        [1234]
        ))))

(deftest parse-multi-element-integer-array
  (testing "multi element integer array"
    (is
      (=
        (pj (new java.io.StringReader "[1234, 5678, 0, -1387, 3743847]") nil nil)
        [1234, 5678, 0, -1387, 3743847]
        ))))

(deftest parse-single-element-string-array
  (testing "single element string array"
    (is
      (=
        (pj (new java.io.StringReader "[\"foobar\"]") nil nil)
        ["foobar"]
        ))))

(deftest parse-mixed-type-multi-element-array
  (testing "multi element mixed type array"
    (is
      (=
        (pj (new java.io.StringReader "[1234, \"5678\", 0.3421, \"FOOBAR\", \"-3743.847\"]") nil nil)
        [1234, "5678", 0.3421, "FOOBAR", "-3743.847"]
        ))))
;
;; TODO test that delimiter chars can be present in string values (ie. "foo,bar" is a valid string value)
;(deftest string-value-containing-delimiter-chars
;  (testing "string value containing delimiter chars"
;      (is
;        (=
;          (pj (new java.io.StringReader "[\"a,b]c d\"]"))
;          ["a,b]c d"]
;          ))))


;(deftest parse-simple-json-object
;  (testing "simple-json-object"
;    (is
;      (=
;        (pj (new java.io.StringReader "{\"a\": 1}"))
;        {"a" 1}
;        ))))

















; OLD TESTS

;(deftest parse-simple-json-object
;  (testing "Parse single json object"
;    (validate-json-parsing "{\"1\":2}" {"1" 2})
;    ))
;
;(deftest parse-simple-json-array
;  (testing "Parse single json array"
;    (validate-json-parsing "[1,2,3,4]" [1,2,3,4])
;    ))

;(deftest parse-complex-single-depth-json-object
;  (testing "Parse complex single-depth json object"
;    (validate-json-parsing "{\"foo\":1234,\"bar\":[1,23,4,6]" {"foo" "bar" "bar" [1, 23, 4, 6]})
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