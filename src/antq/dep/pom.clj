(ns ^:no-doc antq.dep.pom
  "Clojure CLI"
  (:require
   [antq.constant.project-file :as const.project-file]
   [antq.record :as r]
   [antq.util.dep :as u.dep]
   [antq.util.maven :as u.mvn]
   [antq.util.xml :as u.xml]
   [clojure.data.xml :as xml]
   [clojure.java.io :as io]
   [clojure.tools.deps.extensions :as ext]
   [clojure.tools.deps.extensions.pom])
  (:import
   java.io.File
   java.nio.file.Files
   java.nio.file.attribute.FileAttribute))

(defn extract-repos-from-xml
  [xml]
  (->> xml
       (u.xml/get-tags :repository)
       (map #(u.xml/get-values [:id :url] (:content %)))
       (reduce #(assoc %1 (first %2) {:url (second %2)}) {})))

(defn extract-deps-from-xml-string
  [file-path pom-xml-content-str]
  (let [xml (->> pom-xml-content-str
                 xml/parse-str
                 xml-seq)
        repos (extract-repos-from-xml xml)]
    (->> (u.xml/get-tags :dependency xml)
         (map #(u.xml/get-values [:groupId :artifactId :version] (:content %)))
         (filter (fn [[_ _ version]] (seq version)))
         (map (fn [[group-id artifact-id version]]
                (r/map->Dependency {:project :pom
                                    :type :java
                                    :file file-path
                                    :name (str group-id "/" artifact-id)
                                    :version version
                                    :repositories repos}))))))

(defn- pom-root
  "Returns the directory tools.deps should read the pom from. It reads a file
  named pom.xml only, so content under any other name is staged in a temp
  directory."
  ^String [^File file ^String content]
  (if (= "pom.xml" (.getName file))
    (.getParent (.getAbsoluteFile file))
    (let [dir (.toFile (Files/createTempDirectory "antq-pom" (into-array FileAttribute [])))
          staged (io/file dir "pom.xml")]
      (.deleteOnExit dir)
      (.deleteOnExit staged)
      (spit staged content)
      (.getPath dir))))

(defn extract-deps
  "Reads the dependencies of a pom.xml through tools.deps, so that a version
  inherited from a parent or held in a property is resolved. Falls back to
  reading the XML when the model cannot be built."
  [^String file-path ^File file]
  (let [content (slurp file)]
    (try
      (let [root (pom-root file content)
            repos (extract-repos-from-xml (xml-seq (xml/parse-str content)))]
        (for [[dep-name attr] (ext/coord-deps 'antq/pom {:deps/root root} :pom
                                              {:mvn/repos u.mvn/default-repos})]
          (r/map->Dependency {:project :pom
                              :type :java
                              :file file-path
                              :name (str dep-name)
                              :version (:mvn/version attr)
                              :repositories repos})))
      (catch Exception _
        (extract-deps-from-xml-string file-path content)))))

(defn load-deps
  {:malli/schema [:function
                  [:=> :cat [:maybe r/?dependencies]]
                  [:=> [:cat 'string?] [:maybe r/?dependencies]]]}
  ([] (load-deps "."))
  ([dir]
   (let [file (io/file dir const.project-file/maven)]
     (when (.exists file)
       (extract-deps (u.dep/relative-path file)
                     file)))))
