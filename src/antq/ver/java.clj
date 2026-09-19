(ns ^:no-doc antq.ver.java
  (:require
   [antq.constant :as const]
   [antq.util.async :as u.async]
   [antq.util.bb :refer [if-bb]]
   [antq.util.dep :as u.dep]
   [antq.util.exception :as u.ex]
   [antq.util.maven :as u.mvn]
   [antq.ver :as ver]
   [clojure.set :as set]
   [clojure.tools.deps.extensions :as ext]
   [clojure.tools.deps.extensions.maven]
   [version-clj.core :as version])
  (:import
   clojure.lang.ExceptionInfo))

(defn- find-versions
  "Returns the release versions of a Maven artifact through tools.deps.
  Snapshot versions are left out and credentials from a project file are
  not used."
  [name opts]
  (->> (ext/find-versions (symbol name) nil :mvn {:mvn/repos (:repositories opts)})
       (map :mvn/version)))

(def ^:private get-versions-with-timeout
  (u.async/fn-with-timeout
   (if-bb find-versions (requiring-resolve 'antq.util.aether/get-versions))
   const/maven-timeout-msec))

(defn get-sorted-versions-by-name*
  [name
   {:as dep-opts :keys [snapshots?]}
   options]
  (try
    (let [maven-vers (->> (get-versions-with-timeout name dep-opts)
                          (map str))
          versions (if (:ignore-locals options)
                     (seq (set/difference (set maven-vers)
                                          (set (u.mvn/get-local-versions name))))
                     maven-vers)
          sorted-versions (->> versions
                               (sort version/version-compare)
                               (reverse))]
      (cond->> sorted-versions
        (not snapshots?) (remove ver/snapshot?)))
    (catch ExceptionInfo ex
      (if (u.ex/ex-timeout? ex)
        [ex]
        (throw ex)))))

(def get-sorted-versions-by-name
  (memoize get-sorted-versions-by-name*))

(defmethod ver/get-sorted-versions :java
  [dep options]
  (get-sorted-versions-by-name (:name dep)
                               (u.dep/repository-opts dep)
                               options))
