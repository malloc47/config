{:user {:plugins [[cider/cider-nrepl "0.9.1"]
                  [com.palletops/lein-shorthand "0.4.0"]
                  [refactor-nrepl "1.1.0"]
                  [lein-pprint "1.1.1"]]
        :shorthand {. [clojure.repl/apropos
                       clojure.repl/doc
                       clojure.pprint/pprint
                       clojure.repl/pst
                       clojure.repl/source]}}
 :auth {:repository-auth {#"nexus" {:username "deployment"
                                    :password "deployment"}}}}
