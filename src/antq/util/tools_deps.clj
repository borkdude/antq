(ns ^:no-doc antq.util.tools-deps
  "tools.deps calls that honor credentials from a project file. tools.deps
  keeps its Maven context in a session. The credentials go into that session,
  so they do not travel in a config map."
  (:require
   [antq.util.bb :refer [if-bb]]
   [antq.util.maven :as u.mvn]
   [clojure.tools.deps :as deps]
   [clojure.tools.deps.extensions :as ext]
   [clojure.tools.deps.extensions.maven]
   [clojure.tools.deps.util.session :as session])
  (:import
   java.util.concurrent.ConcurrentHashMap))

(defonce ^:private seeded (atom nil))

(defn- seed!
  [credentials]
  (if-bb
   (let [read-settings (requiring-resolve 'babashka.impl.mvn.settings/read-settings)
         servers (into {} (map (fn [[id c]] [id (select-keys c [:username :password])])) credentials)
         store ^ConcurrentHashMap session/session]
     ;; settings.xml wins where both name a server
     (.put store :babashka.impl.mvn/settings (update (read-settings) :servers #(merge servers %)))
     ;; repositories resolved before this carry the old credentials
     (doseq [k (vec (.keySet store))
             :when (and (vector? k) (= :babashka.impl.mvn/repos (first k)))]
       (.remove store k)))
   ((requiring-resolve 'antq.util.aether/seed-session!) credentials)))

(defn- ensure-credentials!
  "Seeds the session with the credentials of repositories, once per set.
  The session is one per process, so credentials add up over a run. Throws
  if one repository id has credentials and two URLs."
  [repositories]
  (let [credentials (u.mvn/credentials repositories)]
    (doseq [[id {:keys [url]}] repositories
            :let [known (get-in @seeded [id :url])]
            :when (and known url (not= known url))]
      (throw (ex-info (str "Repository " id " has credentials and two URLs: " known " and " url)
                      {:repository id})))
    (when (or (nil? @seeded)
              (some (fn [[id c]] (not= c (get @seeded id))) credentials))
      (locking seeded
        (seed! (swap! seeded #(merge credentials %)))))))

(defn- config
  [repositories]
  {:mvn/repos (update-vals repositories #(select-keys % [:url :releases :snapshots]))})

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
