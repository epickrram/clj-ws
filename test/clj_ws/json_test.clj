(ns
  ^{:author mark}
  clj-ws.json-test
  (:require [clojure.test :refer :all]
            [clj-ws.json :refer :all]))


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

;; TODO test that delimiter chars can be present in string values (ie. "foo,bar" is a valid string value)
;(deftest string-value-containing-delimiter-chars
;  (testing "string value containing delimiter chars"
;      (is
;        (=
;          (pj (new java.io.StringReader "[\"a,b]c d\"]"))
;          ["a,b]c d"]
;          ))))


(deftest parse-simple-single-key-json-object
  (testing "simple-single-key-json-object"
    (is
      (=
        (pj (new java.io.StringReader "{\"a\": 1}") nil nil)
        {"a" 1}
        ))))


(deftest parse-simple-multiple-key-json-object
  (testing "simple-multiple-key-json-object"
    (is
      (=
        (pj (new java.io.StringReader "{\"a\": 1, \"b\": 3.14, \"c\": \"foobar\"}") nil nil)
        {"a" 1 "b" 3.14 "c" "foobar"}
        ))))


(deftest parse-array-of-json-object
  (testing "array-of-json-object"
    (is
      (=
        (pj (new java.io.StringReader "[{\"a\": 1, \"b\": 3.14}, {\"c\": \"foobar\"}]") nil nil)
        [{"a" 1 "b" 3.14} {"c" "foobar"}]
        ))))

(deftest parse-nested-json-object
  (testing "nested-json-object"
    (is
      (=
        (pj (new java.io.StringReader "{\"a\": {\"foo\": 1234}}") nil nil)
        {"a" {"foo" 1234}}
        ))))


(deftest parse-complex-object
  (testing "complex-object"
    (is
      (=
        (pj (new java.io.StringReader "{\"a\": [{\"foo\": 1234}, {\"arr\": [1, 2, \"str\", {\"c\": \"recur\"}]}]}") nil nil)
        {"a" [{"foo" 1234} {"arr" [1 2 "str" {"c" "recur"}]}]}
        ))))






