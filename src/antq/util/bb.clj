(ns ^:no-doc antq.util.bb)

(def ^:private bb-version
  (System/getProperty "babashka.version"))

(defmacro if-bb
  "Expands to then on babashka and to else on the JVM. The other branch is
  never compiled, so it may name classes that babashka lacks."
  [then else]
  (if bb-version then else))
