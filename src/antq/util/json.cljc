(ns ^:no-doc antq.util.json
  "JSON reading and writing. babashka ships cheshire in place of
  clojure.data.json."
  (:require
   #?(:bb [cheshire.core :as json]
      :clj [clojure.data.json :as json])))

(defn read-str
  "Returns the JSON string s parsed into Clojure data with keyword keys."
  [s]
  #?(:bb (json/parse-string s true)
     :clj (json/read-str s :key-fn keyword)))

(defn write-str
  "Returns x as a JSON string."
  [x]
  #?(:bb (json/generate-string x)
     :clj (json/write-str x)))
