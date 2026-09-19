(ns ^:no-doc antq.util.aether
  "Code that uses Aether or Maven's settings classes. Keep it in this namespace."
  (:require
   [antq.log :as log]
   [antq.util.leiningen :as u.lein]
   [antq.util.maven :as u.mvn]
   [clojure.tools.deps.util.maven :as deps.util.maven]
   [clojure.tools.deps.util.session :as deps.util.session])
  (:import
   eu.maveniverse.maven.mima.context.Context
   (java.net
    Authenticator
    PasswordAuthentication)
   (org.apache.maven.settings
    Server
    Settings)
   (org.eclipse.aether
    DefaultRepositorySystemSession
    RepositorySystem)
   (org.eclipse.aether.artifact
    Artifact)
   (org.eclipse.aether.resolution
    VersionRangeRequest)
   (org.eclipse.aether.transfer
    TransferEvent
    TransferListener)))

(defn- new-repository-server
  ^Server
  [{:keys [id username password]}]
  (doto (Server.)
    (.setId id)
    (.setUsername (u.mvn/ensure-username-or-password username))
    (.setPassword (u.mvn/ensure-username-or-password password))))

(defn- get-auth-info
  [repository]
  (let [[id {:keys [url username password creds]}] repository]
    (cond
      (and username password)
      {:id id
       :username username
       :password password}

      (= :gpg creds)
      (let [credential-info (u.lein/get-credential url)]
        {:id id
         :username (:username credential-info)
         :password (:password credential-info)}))))

(defn get-maven-settings
  ^Settings
  [opts]
  (let [settings ^Settings (deps.util.maven/get-settings)
        server-ids (set (map #(.getId ^Server %) (.getServers settings)))]
    ;; NOTE
    ;; In Leiningen, authentication information is defined in project.clj or profiles.clj instead of ~/.m2/settings.xml,
    ;; so if there is authentication information in `:repositories`, apply to `settings`
    (doseq [repo (:repositories opts)]
      (let [{:keys [id username password]} (get-auth-info repo)]
        (when (and username
                   password
                   (not (contains? server-ids id)))
          (.addServer settings
                      (new-repository-server {:id id :username username :password password})))))
    settings))

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

(defn repository-system
  [name version opts]
  (let [lib (cond-> name (string? name) symbol)
        local-repo @deps.util.maven/cached-local-repo
        system ^RepositorySystem (deps.util.session/retrieve :mvn/system #(deps.util.maven/make-system))
        settings ^Settings (get-maven-settings opts)
        context ^Context (deps.util.maven/make-context :local-repo local-repo :settings settings)
        session ^DefaultRepositorySystemSession (deps.util.maven/make-system-session context)
        ;; Overwrite TransferListener not to show "Downloading" messages
        _ (.setTransferListener session custom-transfer-listener)
        ;; c.f. https://stackoverflow.com/questions/35488167/how-can-you-find-the-latest-version-of-a-maven-artifact-from-java-using-aether
        artifact (deps.util.maven/coord->artifact lib {:mvn/version version})
        remote-repos (deps.util.maven/remote-repos system session (:repositories opts))]
    {:system system
     :session session
     :artifact artifact
     :remote-repos remote-repos}))

(defn get-versions
  [name opts]
  (let [{:keys [^RepositorySystem system
                ^DefaultRepositorySystemSession  session
                ^Artifact artifact
                remote-repos]} (repository-system name "[0,)" opts)
        req (doto (VersionRangeRequest.)
              (.setArtifact artifact)
              (.setRepositories remote-repos))]
    (->> (.resolveVersionRange system session req)
         (.getVersions))))

(defn authenticator
  ^Authenticator
  [^String username ^String password]
  (proxy [Authenticator] []
    (getPasswordAuthentication []
      (PasswordAuthentication. username (char-array password)))))

(defn initialize-proxy-setting!
  []
  (when-let [prxy (some-> (get-maven-settings {})
                          (.getActiveProxy))]
    (let [host (.getHost prxy)
          port (.getPort prxy)
          username (.getUsername prxy)
          password (.getPassword prxy)]
      (System/setProperty "http.proxyHost" host)
      (System/setProperty "http.proxyPort" (str port))
      (System/setProperty "https.proxyHost" host)
      (System/setProperty "https.proxyPort" (str port))
      (when (and username password)
        (System/setProperty "jdk.http.auth.tunneling.disabledSchemes" "")
        (System/setProperty "jdk.http.auth.proxying.disabledSchemes" "")
        (Authenticator/setDefault (authenticator username password))))))
