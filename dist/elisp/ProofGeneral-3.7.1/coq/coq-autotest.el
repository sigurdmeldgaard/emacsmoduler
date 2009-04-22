;; coq-autotest.el: tests of Coq Proof General (in progress).
;;
;; You can run these by issuing "make test.coq" in PG home dir.
;;
;; coq-autotest.el,v 9.0 2008/01/30 15:22:07 da Exp
;;

(require 'pg-autotest)

;; The included test files
(unless noninteractive
  (pg-autotest  message "Testing standard examples")
  (pg-autotest script-wholefile "coq/example.v")
  (pg-autotest script-wholefile "coq/example-x-symbol.v")
  (pg-autotest script-wholefile "coq/ex-module.v")

  (pg-autotest-quit-prover)
  (pg-autotest-finished))

