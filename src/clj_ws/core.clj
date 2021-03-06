(ns clj-ws.core
  (:gen-class))

(require '[clojure.core.async :as async :refer :all])
(require '[clj-ws.bytes :refer :all])

(defn client-data-handler
  [data-buffer total-length]
  (println (new String data-buffer 0 total-length "UTF-8"))
  )

(defn read-fully
  [input-stream data handler total-read]
  (let
    [buffer (byte-array 16) bytes-read (.read input-stream buffer) updated-total (+ total-read bytes-read)]
    (def updated-array (append-array data total-read buffer bytes-read))
    (if (== bytes-read -1) (handler data updated-total) (read-fully input-stream
                                                          updated-array
                                                          handler updated-total))
    )
  )

(defn handle-client-connection
  [client-socket]
  (with-open [socket client-socket]
    (let [input (clojure.java.io/input-stream (.getInputStream client-socket)) data-buffer (byte-array 4096)]
      (read-fully input data-buffer client-data-handler 0)
      )
    )
  )

(defn server-loop
  [server-socket]
  (let [client-socket (.accept server-socket)]
    (go (handle-client-connection client-socket))
    (server-loop server-socket)))


(defn start-server
  [port]
  (let [server-socket (new java.net.ServerSocket port)]
    (server-loop server-socket)))

(defn -main
  [& args]
  (start-server 8080)
  )


