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

(deftest a-test
  (testing "Parse single json object"
    (parse-json (new java.io.StringReader "{1: 2}") (get-json-result-handler {:1 2}))
    ))
