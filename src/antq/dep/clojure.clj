(ns antq.dep.clojure
  "Clojure CLI"
  (:require
   [antq.record :as r]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.walk :as walk]))

(def ^:private project-file "deps.edn")

(defn extract-deps
  [deps-edn-content-str]
  (let [deps (atom {})]
    (walk/postwalk (fn [form]
                     (when (and (sequential? form)
                                (#{:deps :extra-deps} (first form)))
                       (swap! deps merge (second form)))
                     form)
                   (edn/read-string deps-edn-content-str))
    (for [[dep-name {:mvn/keys [version]}] @deps]
      (r/map->Dependency {:type :java
                          :file project-file
                          :name  (if (qualified-symbol? dep-name)
                                   (str dep-name)
                                   (str dep-name "/" dep-name))
                          :version version}))))

(defn load-deps
  ([] (load-deps "."))
  ([dir]
   (let [file (io/file dir project-file)]
     (when (.exists file)
       (extract-deps (slurp file))))))
