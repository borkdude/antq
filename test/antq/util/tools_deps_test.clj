(ns antq.util.tools-deps-test
  (:require
   [antq.util.bb :refer [if-bb]]
   [antq.util.env :as u.env]
   [antq.util.maven :as u.mvn]
   [antq.util.tools-deps :as sut]
   [clojure.edn :as edn]
   [clojure.string :as str]
   [clojure.test :as t])
  (:import
   (java.util
    Base64
    UUID)))

(def ^:private current-clojure-version
  (get-in (edn/read-string (slurp "deps.edn"))
          [:deps 'org.clojure/clojure :mvn/version]))

(t/deftest find-versions-test
  (let [vers (sut/find-versions 'org.clojure/clojure u.mvn/default-repos)]
    (t/is (contains? (set vers) current-clojure-version))
    (t/testing "snapshot versions are left out"
      (t/is (not-any? u.mvn/snapshot? vers)))))

(def ^:private metadata
  "<metadata><groupId>acme</groupId><artifactId>lib</artifactId><versioning><versions><version>1.0.0</version><version>2.0.0</version></versions></versioning></metadata>")

(defn- respond
  "Returns [status body] for a request path and its Authorization header."
  [path authorization]
  (cond
    (not= authorization (str "Basic " (.encodeToString (Base64/getEncoder) (.getBytes "nexus-user:nexus-pass"))))
    [401 ""]

    (str/ends-with? path "/maven-metadata.xml")
    [200 metadata]

    :else
    [404 ""]))

(def ^:private challenge
  {"WWW-Authenticate" "Basic realm=\"test\""})

(defn- start-server!
  "Starts a repository on a free port. Returns [port stop-fn]."
  []
  (if-bb
   (let [run-server (requiring-resolve 'org.httpkit.server/run-server)
         server (run-server (fn [{:keys [uri headers]}]
                              (let [[status body] (respond uri (get headers "authorization"))]
                                {:status status :body body :headers challenge}))
                            {:port 0 :legacy-return-value? false})]
     [((requiring-resolve 'org.httpkit.server/server-port) server)
      #((requiring-resolve 'org.httpkit.server/server-stop!) server)])
   (let [server (com.sun.net.httpserver.HttpServer/create (java.net.InetSocketAddress. "localhost" 0) 0)]
     (.createContext server "/"
                     (reify com.sun.net.httpserver.HttpHandler
                       (handle [_ exchange]
                         (let [[status ^String body] (respond (str (.getRequestURI exchange))
                                                              (.getFirst (.getRequestHeaders exchange) "Authorization"))
                               bytes (.getBytes body)]
                           (doseq [[k v] challenge]
                             (.add (.getResponseHeaders exchange) k v))
                           (.sendResponseHeaders exchange (int status) (if (zero? (alength bytes)) -1 (alength bytes)))
                           (with-open [out (.getResponseBody exchange)]
                             (.write out bytes))))))
     (.start server)
     [(.getPort (.getAddress server)) #(.stop server 0)])))

(t/deftest find-versions-with-credentials-test
  ;; tools.deps refuses an http: repository unless this is set
  (if-not (u.env/getenv "CLOJURE_CLI_ALLOW_HTTP_REPO")
    (println "Skipping find-versions-with-credentials-test: set CLOJURE_CLI_ALLOW_HTTP_REPO")
    (let [[port stop!] (start-server!)
          url (str "http://localhost:" port "/")]
      (try
        (t/testing "a repository with credentials from the project file lists versions"
          (t/is (= ["1.0.0" "2.0.0"]
                   (sut/find-versions 'acme/lib
                                      {(str "with-credentials-" (UUID/randomUUID))
                                       {:url url :username "nexus-user" :password "nexus-pass"}}))))
        (t/testing "the same repository without credentials lists none"
          (t/is (empty? (sut/find-versions 'acme/other
                                           {(str "without-credentials-" (UUID/randomUUID))
                                            {:url url}}))))
        (finally
          (stop!))))))
