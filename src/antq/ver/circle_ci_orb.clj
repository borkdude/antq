(ns ^:no-doc antq.ver.circle-ci-orb
  (:require
   [antq.log :as log]
   [antq.util.json :as u.json]
   [antq.ver :as ver]
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn- orb-id
  [orb-ns orb-name]
  (try
    (-> (io/as-url (str "https://internal.circleci.com/api/v2/orbs?ns=" orb-ns "&name=" orb-name))
        slurp
        (u.json/read-str)
        :items
        first
        :id)
    (catch Exception ex
      (log/error (str "Failed to fetch orb id from circleci: "
                      (.getMessage ex))))))

(defn- orb-versions
  [id]
  (try
    (-> (io/as-url (str "https://internal.circleci.com/api/v2/orbs/" id))
        slurp
        (u.json/read-str)
        :versions)
    (catch Exception ex
      (log/error (str "Failed to fetch orb versions from circleci: "
                      (.getMessage ex))))))

(defmethod ver/get-sorted-versions :circle-ci-orb
  [dep _options]
  (let [[orb-ns orb-name] (str/split (:name dep) #"/" 2)
        id (orb-id orb-ns orb-name)]
    (orb-versions id)))
