(defproject tp "0.1.0-SNAPSHOT"
  :description "Trabajo práctico - Lenguajes Formales - FIUBA - Intérprete Racket"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]]
  :main ^:skip-aot tp.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
