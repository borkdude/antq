(ns ^:no-doc antq.util.maven
  (:require
   [antq.log :as log]
   [antq.util.env :as u.env]
   [antq.util.leiningen :as u.lein]
   [antq.util.xml :as u.xml]
   [clojure.data.xml :as xml]
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def default-repos
  {"central" {:url "https://repo1.maven.org/maven2/"}
   "clojars" {:url "https://repo.clojars.org/"}})

(defn normalize-repo-url
  "c.f. https://clojure.org/reference/deps_and_cli#_maven_s3_repos"
  [url]
  (str/replace url #"^s3p://" "s3://"))

(defn normalize-repos
  [repos]
  (reduce-kv
   (fn [acc k v]
     (assoc acc k (if (contains? v :url)
                    (update v :url normalize-repo-url)
                    v)))
   {} repos))

(defn snapshot?
  [s]
  (if s
    (str/includes? (str/lower-case s) "snapshot")
    false))

(defn ensure-username-or-password
  [x]
  (if (string? x)
    x
    (or (u.lein/env x)
        (str x))))

(defn read-pom
  "Returns the url and scm url of a POM file as a map."
  [^java.io.File file]
  (let [content (:content (xml/parse-str (slurp file)))
        scm (first (u.xml/get-tags :scm content))]
    {:url (some-> (u.xml/get-value :url content) str/trim not-empty)
     :scm-url (some-> (when scm (u.xml/get-value :url (:content scm))) str/trim not-empty)}))

(defn- get-local-versions*
  [name]
  (let [sep (System/getProperty "file.separator")
        path (-> (str name)
                 (str/replace "/" sep)
                 (str/replace "." sep))
        file (io/file (u.env/getenv "HOME") ".m2" "repository" path "maven-metadata-local.xml")]
    (when (.exists file)
      (try
        (->> (slurp file)
             (xml/parse-str)
             (xml-seq)
             (u.xml/get-tags :version)
             (map (comp first :content)))
        (catch Exception ex
          (log/warning (str "Failed to get local versions for "
                            name
                            " from "
                            (.getAbsolutePath file)
                            " because: "
                            (.getMessage ex)))
          nil)))))

(def get-local-versions
  (memoize get-local-versions*))
