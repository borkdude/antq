(ns antq.smoke
  "Checks that antq still runs on babashka."
  (:require
   [antq.api :as api]
   [antq.dep.pom :as dep.pom]
   [clojure.java.io :as io]))

(defn- fail!
  [& msg]
  (apply println msg)
  (System/exit 1))

(defn- check-version-lookup!
  "Reports a newer version, and a changelog URL, for a deliberately old
  dependency. Covers version lookup and reading the POM of an artifact."
  []
  (let [outdated (api/outdated-deps {'org.clojure/tools.cli {:mvn/version "0.3.5"}})
        {:keys [name latest-version changes-url]} (first outdated)]
    (when-not (= 1 (count outdated))
      (fail! "Expected one outdated dependency, got" (pr-str outdated)))
    (when-not (and (= "org.clojure/tools.cli" name) (seq latest-version) (seq changes-url))
      (fail! "Expected a version and a changelog URL for tools.cli, got"
             (pr-str (first outdated))))
    (println "version lookup:" name "0.3.5 ->" latest-version)))

(defn- check-pom-parsing!
  "Reads a project's own pom.xml, which goes through tools.deps on the JVM and
  babashka by different routes."
  []
  (let [deps (dep.pom/extract-deps "pom.xml" (io/file "test/resources/dep/test_pom.xml"))
        names (set (map :name deps))]
    (when-not (contains? names "foo/core")
      (fail! "Expected foo/core in the parsed pom, got" (pr-str names)))
    (println "pom parsing:" (count deps) "dependencies")))

(defn run
  [_]
  (check-version-lookup!)
  (check-pom-parsing!)
  (println "ok"))
