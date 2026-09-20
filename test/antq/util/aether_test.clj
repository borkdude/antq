(ns antq.util.aether-test
  (:require
   [antq.util.aether :as sut]
   [antq.util.env :as u.env]
   [antq.util.leiningen :as u.lein]
   [antq.util.maven :as u.mvn]
   [clojure.test :as t]
   [clojure.tools.deps.util.maven :as deps.util.maven]
   [clojure.tools.deps.util.session :as deps.util.session])
  (:import
   java.util.concurrent.ConcurrentHashMap
   (org.apache.maven.settings
    Server
    Settings)
   (org.eclipse.aether
    RepositorySystem
    RepositorySystemSession)
   (org.eclipse.aether.repository
    AuthenticationContext
    RemoteRepository)))

(defn- dummy-settings
  "Returns fresh settings.
  seed-session! adds servers to what it is given."
  []
  (doto (Settings.)
    (.addServer (doto (Server.)
                  (.setId "serv1")))
    (.addServer (doto (Server.)
                  (.setId "serv2")
                  (.setUsername "two-user")
                  (.setPassword "two-pass")))))

(def ^:private dummy-repos
  {;; duplicated with dummy-settings
   "serv1" {:url "https://one.example.com"}
   ;; named by dummy-settings as well, settings.xml wins
   "serv2" {:url "https://two.example.com"
            :username "project-user"
            :password "project-pass"}
   ;; new to appear
   "serv3" {:url "https://three.example.com"
            :username "three-user"
            :password "three-pass"}
   ;; new to appear
   "serv4" {:url "https://four.example.com"
            :username :env
            :password :env/four}
   ;; new to appear
   "serv5" {:url "https://five.example.com"
            :creds :gpg}
   ;; should not be added because of missing username and password
   "dummy" {:url "https://dummy.example.com"}})

(def ^:private dummy-env
  {"LEIN_PASSWORD" "lein-pass"
   "FOUR" "env-four"})

(defn- seeded-credentials
  "Returns the username and password the seeded session holds for each
  repository id of dummy-repos."
  []
  (let [store ^ConcurrentHashMap deps.util.session/session
        system ^RepositorySystem (.get store :mvn/system)
        session ^RepositorySystemSession (.get store :mvn/session)]
    (into {}
          (map (fn [^RemoteRepository repo]
                 [(.getId repo)
                  ;; nil if the repository has no credentials, so no with-open
                  (when-let [ctx (AuthenticationContext/forRepository session repo)]
                    (try
                      {:username (.get ctx AuthenticationContext/USERNAME)
                       :password (.get ctx AuthenticationContext/PASSWORD)}
                      (finally
                        (.close ctx))))]))
          (deps.util.maven/remote-repos system session (update-vals dummy-repos #(select-keys % [:url]))))))

(t/deftest seed-session-test
  (with-redefs [deps.util.maven/get-settings (constantly (dummy-settings))
                u.env/getenv #(get dummy-env %)
                u.lein/get-credential (constantly {:username "gpg-user"
                                                   :password "gpg-pass"})]
    (deps.util.session/with-session
      (sut/seed-session! (u.mvn/credentials dummy-repos) nil)
      (t/is (= {"serv1" nil
                ;; settings.xml wins over the project file
                "serv2" {:username "two-user" :password "two-pass"}
                ;; from project.clj
                "serv3" {:username "three-user" :password "three-pass"}
                ;; from project.clj with an environment variable
                "serv4" {:username "lein-pass" :password "env-four"}
                ;; from profiles.clj with gpg
                "serv5" {:username "gpg-user" :password "gpg-pass"}
                "dummy" nil}
               (seeded-credentials))))))
