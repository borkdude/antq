(ns ^:no-doc antq.ver.java
  (:require
   [antq.constant :as const]
   [antq.util.async :as u.async]
   [antq.util.dep :as u.dep]
   [antq.util.exception :as u.ex]
   [antq.util.maven :as u.mvn]
   [antq.util.tools-deps :as u.tools-deps]
   [antq.ver :as ver]
   [clojure.set :as set]
   [version-clj.core :as version])
  (:import
   clojure.lang.ExceptionInfo))

(defn- get-versions
  [name opts]
  (u.tools-deps/find-versions name (:repositories opts)))

(def ^:private get-versions-with-timeout
  (u.async/fn-with-timeout
   get-versions
   const/maven-timeout-msec))

(defn get-sorted-versions-by-name*
  [name
   dep-opts
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
      (remove ver/snapshot? sorted-versions))
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
