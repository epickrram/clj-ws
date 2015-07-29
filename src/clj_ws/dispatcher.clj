(ns
  ^{:author mark}
  clj-ws.dispatcher)

(keyword :dispatched)
(keyword :unknown-target)
(keyword :handler-error)

(defn dispatch-request
  [request handler-map]
  (def handler-function (get handler-map (get request "id")))
  (if (not= nil handler-function)
    (try
      (do
        (handler-function request)
        :dispatched
        )
      (catch java.lang.RuntimeException e
        (do
          (prn (str e))
          :handler-error
          )
        )
      )
    :unknown-target
    )
  )