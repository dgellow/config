(require 'boot.repl)
(swap! boot.repl/*default-dependencies*
       concat '[[cider/cider-nrepl "0.12.0"]
                [org.clojure/tools.nrepl "0.2.12"]])
(swap! boot.repl/*default-middleware*
       conj 'cider.nrepl/cider-middleware)
