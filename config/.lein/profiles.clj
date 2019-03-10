{:user {:plugins [#_[cider/cider-nrepl "0.9.1"]
                  [com.palletops/lein-shorthand "0.4.0"]
                  #_[refactor-nrepl "2.2.0"]
                  [lein-pprint "1.1.1"]]
        :shorthand {. [clojure.repl/apropos
                       clojure.repl/doc
                       clojure.pprint/pprint
                       clojure.repl/pst
                       clojure.repl/source]}}}
