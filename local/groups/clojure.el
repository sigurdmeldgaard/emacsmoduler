(require 'clojure-mode)

(setq swank-clojure-jar-path "/Applications/clojure-1.1.0/clojure.jar"
      swank-clojure-extra-classpaths
      (list "~/installs/swank-clojure/src/main/clojure"
            "/Applications/clojure-1.1.0/clojure-contrib.jar"))

(load "swank-clojure-autoload")