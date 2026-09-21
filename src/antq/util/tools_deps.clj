(ns ^:no-doc antq.util.tools-deps
  "tools.deps calls that honor credentials from a project file.

  On the JVM tools.deps keeps its Maven context in a session. The credentials
  go into that session, so they do not travel in a config map. The session
  holds the credentials of one project at a time. Callers that look up in
  parallel group their work by `credential-set` first, see
  `antq.core/outdated-deps`.

  On babashka the credentials are bound around each lookup."
  (:require
   [antq.util.bb :refer [if-bb]]
   [antq.util.maven :as u.mvn]
   [clojure.tools.deps :as deps]
   [clojure.tools.deps.extensions :as ext]
   [clojure.tools.deps.extensions.maven]))

(if-bb (require '[babashka.deps.mvn :as deps.mvn]) nil)

(def ^:dynamic *local-repo*
  "The local Maven repository, or nil for the default one."
  nil)

(defonce ^:private seeded (atom ::none))

(defn credential-set
  "Returns the credentials of repositories, the unit the session holds."
  [repositories]
  (u.mvn/credentials repositories))

(defn- ensure-credentials!
  "Seeds the session with the credentials of repositories, replacing those of
  another project.
  A caller that runs in parallel with another credential set reseeds between
  this and the lookup, so callers group by `credential-set` first. A lookup
  that outlives its timeout has the same race. Either loses its credentials
  and reports a 401, neither sends them elsewhere."
  [repositories]
  (let [wanted [(credential-set repositories) *local-repo*]]
    (when (not= wanted @seeded)
      (locking seeded
        ;; seeded is set after the session is, so a waiting thread sees both
        (when (not= wanted @seeded)
          ((requiring-resolve 'antq.util.aether/seed-session!) (first wanted) *local-repo*)
          (reset! seeded wanted))))))

(defn- config
  [repositories]
  (cond-> {:mvn/repos (update-vals repositories #(select-keys % [:url :releases :snapshots]))}
    *local-repo* (assoc :mvn/local-repo *local-repo*)))

(defn find-versions
  "Returns the release versions of lib in repositories, oldest first."
  [lib repositories]
  (let [lib (symbol lib)
        versions #(mapv :mvn/version (ext/find-versions lib nil :mvn (config repositories)))]
    (if-bb
     (deps.mvn/with-repository-credentials (credential-set repositories)
       (versions))
     (do (ensure-credentials! repositories)
         (versions)))))

(defn coord-deps
  "Returns the dependencies of lib at version. Fetches its POM into the local
  repository."
  [lib version repositories]
  (let [deps #(ext/coord-deps (symbol lib) {:mvn/version version} :mvn (config repositories))]
    (if-bb
     (deps.mvn/with-repository-credentials (credential-set repositories)
       (deps))
     (do (ensure-credentials! repositories)
         (deps)))))

(defn lib-location
  "Returns the :base and :path of lib at version in the local repository."
  [lib version repositories]
  (deps/lib-location (symbol lib) {:mvn/version version} (config repositories)))
