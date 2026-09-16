(ns ^:no-doc antq.report.json
  (:require
   [antq.report :as report]
   [antq.util.json :as u.json]))

(defmethod report/reporter "json"
  [deps _options]
  (->> deps
       ;; NOTE Add diff-url for backward compatibility
       (map #(assoc % :diff-url (:changes-url %)))
       (u.json/write-str)
       (println)))
