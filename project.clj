(defproject com.github.johnnyjayjay/titanium-json-ld-clj "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [com.apicatalog/titanium-json-ld "1.3.1"]]
  :profiles {:dev {:dependencies [[org.glassfish/jakarta.json "2.0.1"]]}}
  :repl-options {:init-ns com.github.johnnyjayjay.titanium-json-ld-clj})
