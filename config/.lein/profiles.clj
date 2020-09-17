{:user {:dependencies [[clj-kondo "RELEASE"]]
        :plugins [[com.gfredericks/lein-shorthand "0.4.1"]
                  [lein-pprint "1.1.1"]]
        :aliases {"clj-kondo" ["run" "-m" "clj-kondo.main"]}}
        :shorthand {. [clojure.repl/apropos
                       clojure.repl/doc
                       clojure.pprint/pprint
                       clojure.repl/pst
                       clojure.repl/source]}}}
