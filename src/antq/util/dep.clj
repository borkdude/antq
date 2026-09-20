(ns ^:no-doc antq.util.dep
  (:require
   [antq.constant :as const]
   [antq.log :as log]
   [antq.util.async :as u.async]
   [antq.util.function :as u.fn]
   [antq.util.maven :as u.mvn]
   [antq.util.tools-deps :as u.tools-deps]
   [antq.util.url :as u.url]
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import
   java.io.File))

(defn compare-deps
  [x y]
  (if (and (string? (:file x))
           (string? (:file y)))
    (let [prj (.compareTo ^String (:file x) ^String (:file y))]
      (if (zero? prj)
        (.compareTo ^String (:name x) ^String (:name y))
        prj))
    0))

(defn relative-path
  [^File target-file]
  (-> (.getPath target-file)
      (str/replace-first #"^\./" "")))

(defn name-candidates
  [^String dep-name]
  (let [[group-id artifact-id] (str/split dep-name #"/" 2)
        candidates (cond-> #{}
                     (seq dep-name) (conj (symbol dep-name)))]
    (cond-> candidates
      (= group-id artifact-id) (conj (symbol group-id)))))

(defn repository-opts
  [dep]
  {:repositories (-> u.mvn/default-repos
                     (merge (:repositories dep))
                     (u.mvn/normalize-repos))})

(defmulti normalize-version-by-name
  (fn [dep] (:name dep)))

(defmethod normalize-version-by-name :default
  [dep]
  dep)

(defn normalize-path
  [^String path]
  (let [file (io/file path)]
    (try
      (let [path' (-> file
                      (.toPath)
                      (.normalize)
                      (str))]
        (if (and (not (str/blank? path))
                 (str/blank? path'))
          "."
          path'))
      (catch Exception _
        (.getCanonicalPath file)))))

(defn- pom-file*
  "Returns the POM of dep in the local repository, or nil when it cannot be
  read. Reading a Maven coordinate's dependencies caches its POM there, which
  is the only route tools.deps offers to the POM itself."
  ^File
  [dep]
  (let [lib (symbol (:name dep))
        version (:version dep)
        repositories (:repositories (repository-opts dep))
        {:keys [base path]} (u.tools-deps/lib-location lib version repositories)
        artifact-id (first (str/split (name lib) #"\$"))
        file (io/file base path (str artifact-id "-" version ".pom"))]
    (when-not (.exists file)
      (let [reason (try
                     (u.tools-deps/coord-deps lib version repositories)
                     nil
                     (catch Exception ex
                       (->> ex (iterate ex-cause) (take-while some?) last ex-message)))]
        ;; the dependencies can fail over a parent POM while the POM itself arrived
        (when (and reason (not (.exists file)))
          (log/warning (str "Failed to read the POM of " lib " " version ": " reason)))))
    (when (.exists file)
      file)))

(def ^:private pom-file-with-timeout
  (u.async/fn-with-timeout
   pom-file*
   const/pom-timeout-msec))

(defn- get-scm-url*
  [dep]
  (try
    (let [{:keys [url scm-url]} (some-> (pom-file-with-timeout dep) (u.mvn/read-pom))]
      (some-> (or scm-url url)
              (u.url/ensure-https)
              (u.url/ensure-git-https-url)))
    ;; Skip showing the diff URL when the POM cannot be read
    (catch Exception _ nil)))
(def get-scm-url (u.fn/memoize-by get-scm-url* :name))

(defn ensure-version-list
  [x]
  (cond
    (string? x)
    [x]

    (and (sequential? x)
         (every? string? x))
    x

    :else
    []))
