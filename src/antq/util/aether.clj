(ns ^:no-doc antq.util.aether
  "Code that uses Aether or Maven's settings classes. Keep it in this namespace."
  (:require
   [antq.log :as log]
   [antq.util.maven :as u.mvn]
   [clojure.tools.deps.util.maven :as deps.util.maven]
   [clojure.tools.deps.util.session :as deps.util.session])
  (:import
   eu.maveniverse.maven.mima.context.Context
   java.util.concurrent.ConcurrentHashMap
   (org.apache.maven.settings
    Server
    Settings)
   org.eclipse.aether.DefaultRepositorySystemSession
   (org.eclipse.aether.transfer
    TransferEvent
    TransferListener)))

(defn- settings-with
  "Returns the user's Maven settings with a server added for each entry of
  credentials that settings.xml does not name."
  ^Settings
  [credentials]
  (let [settings ^Settings (deps.util.maven/get-settings)
        server-ids (set (map #(.getId ^Server %) (.getServers settings)))]
    (doseq [[id {:keys [username password]}] credentials
            :when (not (contains? server-ids id))]
      (.addServer settings (doto (Server.)
                             (.setId id)
                             (.setUsername username)
                             (.setPassword password))))
    settings))

(defn get-maven-settings
  ^Settings
  [opts]
  ;; NOTE
  ;; In Leiningen, authentication information is defined in project.clj or profiles.clj instead of ~/.m2/settings.xml,
  ;; so if there is authentication information in `:repositories`, apply to `settings`
  (settings-with (u.mvn/credentials (:repositories opts))))

(def ^TransferListener custom-transfer-listener
  "Copy from clojure.tools.deps.util.maven/console-listener
  But no outputs for `transferStarted`"
  (reify TransferListener
    (transferStarted [_ _event])
    (transferCorrupted [_ event]
      (log/warning (str "Download corrupted:" (.. ^TransferEvent event getException getMessage))))
    ;; This happens when Maven can't find an artifact in a particular repo
    ;; (but still may find it in a different repo), ie this is a common event
    (transferFailed [_ _event])
    (transferInitiated [_ _event])
    (transferProgressed [_ _event])
    (transferSucceeded [_ _event])))

(defn seed-session!
  "Puts a Maven context with credentials as servers into the tools.deps
  session, where the :mvn extension looks it up."
  [credentials]
  (let [context ^Context (deps.util.maven/make-context :settings (settings-with credentials))
        session ^DefaultRepositorySystemSession (deps.util.maven/make-system-session context)
        store ^ConcurrentHashMap deps.util.session/session]
    ;; Overwrite TransferListener not to show "Downloading" messages
    (.setTransferListener session custom-transfer-listener)
    (.put store :mvn/context context)
    (.put store :mvn/system (deps.util.maven/make-system context))
    (.put store :mvn/session session)))

(defn active-proxy
  "Returns the active proxy of the user's Maven settings as a map, or nil if
  none is active."
  []
  (when-let [prxy (.getActiveProxy (settings-with nil))]
    {:host (.getHost prxy)
     :port (.getPort prxy)
     :username (.getUsername prxy)
     :password (.getPassword prxy)}))
