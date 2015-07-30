(ns
  ^{:author mark}
  clj-ws.dispatcher)

(keyword :dispatched)
(keyword :unknown-target)
(keyword :handler-error)
(keyword :invalid-request)

(defn dispatch-request
  [request handler-map]
  (def request-id (get request "id"))
  (if (not= nil request-id)
    (do
      (def handler-function (get handler-map request-id))
      (if (not= nil handler-function)
        (try
          (do
            (handler-function request)
            :dispatched
            )
          (catch java.lang.Throwable e
            (do
              (prn (str e))
              :handler-error
              )
            )
          )
        :unknown-target
        ))
    :invalid-request
    )
  )