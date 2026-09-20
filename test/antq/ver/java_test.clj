(ns antq.ver.java-test
  (:require
   [antq.record :as r]
   [antq.util.exception :as u.ex]
   [antq.util.maven :as u.mvn]
   [antq.ver :as ver]
   [antq.ver.java :as sut]
   [clojure.set :as set]
   [clojure.test :as t]))

(defn- dummy-versions
  [_ opts]
  (concat
   ["1"]
   (when (contains? (:repositories opts) "dummy")
     ["1.3"])
   ["1.6-SNAPSHOT" "2"]))

(defn- get-sorted-versions
  [m]
  ;; get-sorted-versions-by-name is memoized, so a test that wants its own
  ;; versions passes its own :name
  (ver/get-sorted-versions (r/map->Dependency (merge {:type :java :name "dummy"} m))
                           {}))

(t/deftest get-sorted-versions-test
  (with-redefs [sut/get-versions-with-timeout dummy-versions]
    (t/is (= ["2" "1"]
             (get-sorted-versions {:version "1.0.0"})))
    (t/testing "a snapshot dependency is offered releases only"
      (t/is (= ["2" "1"]
               (get-sorted-versions {:version "1.0.0-SNAPSHOT"})))))

  (t/testing "normalizing repository URL"
    (with-redefs [sut/get-sorted-versions-by-name (fn [_ opts _] opts)]
      (let [res (get-sorted-versions {:repositories {"foo" {:url "s3p://bar"}}})
            diff (set/difference (set (:repositories res))
                                 (set u.mvn/default-repos))]
        (t/is (= #{["foo" {:url "s3://bar"}]}
                 diff))))))

(t/deftest get-sorted-versions-letter-test
  (t/testing "a version that starts with a letter sorts below the numbered ones"
    (with-redefs [sut/get-versions-with-timeout (fn [_ _] ["r03" "1" "2"])]
      (t/is (= ["2" "1" "r03"]
               (get-sorted-versions {:name "letters" :version "1.0.0"}))))))

(t/deftest get-sorted-versions-timeout-test
  (with-redefs [sut/get-sorted-versions-by-name sut/get-sorted-versions-by-name*
                sut/get-versions-with-timeout (fn [& _] (throw (u.ex/ex-timeout "test timeout")))]
    (let [deps (get-sorted-versions {:version "1.0.0"})]
      (t/is (= 1 (count deps)))
      (t/is (u.ex/ex-timeout? (first deps))))))
