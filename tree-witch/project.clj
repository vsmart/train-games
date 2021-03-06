(defproject sorry-dog "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [quil "2.5.0"]
                 [org.clojure/clojurescript "1.9.293"]]

  :plugins [[lein-cljsbuild "1.1.4"]
             [lein-figwheel "0.5.8"]]
  :hooks [leiningen.cljsbuild]

  :cljsbuild
  {:builds { "dev"
            {:source-paths ["src"]
             :id "dev"
             :figwheel true
             :compiler
             {:output-to "js/main.js"
              :output-dir "out"
              :main "tree_witch.core"
              :optimizations :none
              :pretty-print true}}
             "min"
            {:source-paths ["src"]
             :id "min"
             :compiler
             {:output-to "js/sleepy_dog.js"
              :output-dir "min"
              :main "tree_witch.core"
              :optimizations :advanced }}}})
