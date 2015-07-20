{:user {:plugins [[cider/cider-nrepl "0.10.0-SNAPSHOT"]
                  [com.palletops/lein-shorthand "0.4.0"]
                  [lein-cljfmt "0.1.10"]
                  [refactor-nrepl "1.2.0-SNAPSHOT"]]
        :dependencies [[org.clojure/tools.nrepl "0.2.7"]]
        :shorthand {. [clojure.repl/apropos
                       clojure.repl/doc
                       clojure.pprint/pprint
                       clojure.repl/pst
                       clojure.repl/source]}}
 :auth {:repository-auth {#"nexus" {:username "deployment"
                                    :password "deployment"}}}}
