(ns antq.util.tools-deps-test
  (:require
   [antq.util.bb :refer [if-bb]]
   [antq.util.env :as u.env]
   [antq.util.maven :as u.mvn]
   [antq.util.tools-deps :as sut]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :as t])
  (:import
   (java.nio.file
    Files)
   (java.nio.file.attribute
    FileAttribute)
   (java.util
    Base64)))

(def ^:private current-clojure-version
  (get-in (edn/read-string (slurp "deps.edn"))
          [:deps 'org.clojure/clojure :mvn/version]))

(t/deftest find-versions-test
  (let [vers (sut/find-versions 'org.clojure/clojure u.mvn/default-repos)]
    (t/is (contains? (set vers) current-clojure-version))
    (t/testing "snapshot versions are left out"
      (t/is (not-any? u.mvn/snapshot? vers)))))

(def ^:private metadata
  "<metadata><groupId>acme</groupId><artifactId>lib</artifactId><versioning><versions><version>1.0.0</version></versions></versioning></metadata>")

(def ^:private pom
  "<project><modelVersion>4.0.0</modelVersion><groupId>acme</groupId><artifactId>lib</artifactId><version>1.0.0</version><dependencies><dependency><groupId>acme</groupId><artifactId>dep</artifactId><version>3.0.0</version></dependency></dependencies></project>")

(def ^:private other-metadata
  "<metadata><groupId>acme</groupId><artifactId>lib</artifactId><versioning><versions><version>1.0.0</version><version>2.0.0</version></versions></versioning></metadata>")

(def ^:private authorization
  (str "Basic " (.encodeToString (Base64/getEncoder) (.getBytes "nexus-user:nexus-pass"))))

(defn- respond
  "Returns [status body] for a request path and its Authorization header.
  Paths under /open need no credentials."
  [path header]
  (cond
    (str/starts-with? path "/open/b/") [200 other-metadata]
    (str/starts-with? path "/open/a/") [200 metadata]
    (not= header authorization) [401 ""]
    (str/ends-with? path "/maven-metadata.xml") [200 metadata]
    (str/ends-with? path "/acme/lib/1.0.0/lib-1.0.0.pom") [200 pom]
    :else [404 ""]))

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

(defn- delete-tree! [dir]
  (doseq [f (reverse (file-seq (io/file dir)))]
    (io/delete-file f true)))

(defn- with-repository
  "Calls f with the URL of an authenticating repository and a local Maven
  repository of its own."
  [f]
  (let [[port stop!] (start-server!)
        url (str "http://localhost:" port "/")
        local (str (Files/createTempDirectory "antq-m2" (into-array FileAttribute [])))]
    (try
      (binding [sut/*local-repo* local]
        (f url))
      (finally
        (stop!)
        (delete-tree! local)))))

(t/deftest two-repositories-test
  (when (u.env/getenv "CLOJURE_CLI_ALLOW_HTTP_REPO")
    (with-repository
      (fn [url]
        ;; the credentials are the same, so the session is not reseeded between
        ;; the two lookups
        (t/testing "each repository has its own versions of one lib"
          (t/is (= ["1.0.0"]
                   (sut/find-versions 'acme/lib {"a" {:url (str url "open/a/")}})))
          (t/is (= ["1.0.0" "2.0.0"]
                   (sut/find-versions 'acme/lib {"b" {:url (str url "open/b/")}}))))))))

(t/deftest credentials-test
  ;; tools.deps refuses an http: repository without this
  (t/is (u.env/getenv "CLOJURE_CLI_ALLOW_HTTP_REPO")
        "set CLOJURE_CLI_ALLOW_HTTP_REPO to run this test")
  (when (u.env/getenv "CLOJURE_CLI_ALLOW_HTTP_REPO")
    (with-repository
      (fn [url]
        (let [repos (fn [credentials] {"acme" (merge {:url url} credentials)})
              credentials {:username "nexus-user" :password "nexus-pass"}]
          (t/testing "credentials from the project file list versions"
            (t/is (= ["1.0.0"]
                     (sut/find-versions 'acme/lib (repos credentials)))))

          (t/testing "the credentials of the previous lookup are gone"
            (t/is (empty? (sut/find-versions 'acme/other (repos nil)))))

          (t/testing "credentials reach the POM of a dependency"
            (t/is (= [['acme/dep {:mvn/version "3.0.0"}]]
                     (sut/coord-deps 'acme/lib "1.0.0" (repos credentials))))))))))
