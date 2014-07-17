(defproject paredit "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2261"]
                 [com.cemerick/piggieback "0.1.3"]]

  :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}

  :plugins [[lein-cljsbuild "1.0.3"]]

  :cljsbuild {
    :builds [{:id "paredit"
              :notify-command ["terminal-notifier" "-message"]
              :source-paths ["cljs"]
              :compiler {
                :output-to "lib/paredit.js"
                :output-dir "lib/out"
                :source-map "lib/paredit.js.map"
                :target :nodejs
                :optimizations :simple}}
             {:id "dev"
              :notify-command ["terminal-notifier" "-message"]
              :source-paths ["cljs"]
              :compiler
              {:output-dir "target/devout"
                :output-to "target/paredit.js"
               :target :nodejs
               :optimizations :none}}]})
