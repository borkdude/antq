(ns ^:no-doc antq.util.tools-deps
  "tools.deps calls that honor credentials from a project file. tools.deps
  keeps its Maven context in a session. The credentials go into that session,
  so they do not travel in a config map.

  The session holds the credentials of one project at a time. Callers that
  look up in parallel group their work by `credential-set` first, see
  `antq.core/outdated-deps`."
  (:require
   [antq.util.bb :refer [if-bb]]
   [antq.util.maven :as u.mvn]
   [clojure.tools.deps :as deps]
   [clojure.tools.deps.extensions :as ext]
   [clojure.tools.deps.extensions.maven]
   [clojure.tools.deps.util.session :as session])
  (:import
   java.util.concurrent.ConcurrentHashMap))

(def ^:dynamic *local-repo*
  "The local Maven repository, or nil for the default one."
  nil)

(defonce ^:private seeded (atom ::none))

(defn credential-set
  "Returns the credentials of repositories, the unit the session holds."
  [repositories]
  (u.mvn/credentials repositories))

(defn- seed!
  [credentials]
  (if-bb
   (let [read-settings (requiring-resolve 'babashka.impl.mvn.settings/read-settings)
         servers (update-vals credentials #(select-keys % [:username :password]))
         store ^ConcurrentHashMap session/session]
     ;; settings.xml wins where both name a server
     (.put store :babashka.impl.mvn/settings (update (read-settings) :servers #(merge servers %)))
     ;; cached repositories and version listings carry the previous credentials
     (doseq [k (vec (.keySet store))
             :when (and (vector? k)
                        (#{:babashka.impl.mvn/repos :babashka.impl.mvn/versions} (first k)))]
       (.remove store k)))
   ((requiring-resolve 'antq.util.aether/seed-session!) credentials *local-repo*)))

(defn- ensure-credentials!
  "Seeds the session with the credentials of repositories, replacing those of
  another project.
  A caller that runs in parallel with another credential set reseeds between
  this and the lookup, so callers group by `credential-set` first."
  [repositories]
  (let [wanted [(credential-set repositories) *local-repo*]]
    (when (not= wanted @seeded)
      (locking seeded
        ;; seeded is set after the session is, so a waiting thread sees both
        (when (not= wanted @seeded)
          (seed! (first wanted))
          (reset! seeded wanted))))))

(defn- config
  [repositories]
  (cond-> {:mvn/repos (update-vals repositories #(select-keys % [:url :releases :snapshots]))}
    *local-repo* (assoc :mvn/local-repo *local-repo*)))

(defn find-versions
  "Returns the release versions of lib in repositories, oldest first."
  [lib repositories]
  (ensure-credentials! repositories)
  (map :mvn/version (ext/find-versions (symbol lib) nil :mvn (config repositories))))

(defn coord-deps
  "Returns the dependencies of lib at version. Fetches its POM into the local
  repository."
  [lib version repositories]
  (ensure-credentials! repositories)
  (ext/coord-deps (symbol lib) {:mvn/version version} :mvn (config repositories)))

(defn lib-location
  "Returns the :base and :path of lib at version in the local repository."
  [lib version repositories]
  (deps/lib-location (symbol lib) {:mvn/version version} (config repositories)))
