(defproject primitive "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha16"]
                 [net.mikera/imagez "0.12.0"]
                 [gif-clj "1.0.3"]]
  :main ^:skip-aot primitive.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})