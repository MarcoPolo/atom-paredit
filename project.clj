(defproject paredit "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-3058"]
                 [com.cemerick/piggieback "0.1.5"]]

  :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}

  :plugins [[lein-cljsbuild "1.0.5"]]

  :clean-targets ["lib/out" "lib/paredit.js" "lib/paredit.js.map"]

  :cljsbuild {
    :builds [{:id "paredit"
              :notify-command ["./notify"]
              :source-paths ["cljs"]
              :compiler {
                :output-to "lib/paredit.js"
                :output-dir "lib/out"
                :source-map "lib/paredit.js.map"
                :target :nodejs
                :optimizations :simple}}
             {:id "dev"
              :notify-command ["./notify"]
              :source-paths ["cljs"]
              :compiler
              {:output-dir "target/devout"
                :output-to "target/paredit.js"
               :target :nodejs
               :optimizations :none}}]})
