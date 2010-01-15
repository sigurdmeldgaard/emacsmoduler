(set-language-environment "utf-8")

(add-to-list 'load-path "~/emacsmoduler/dist/elisp/slime/")

;;; Note that if you save a heap image, the character
;;; encoding specified on the command line will be preserved,
;;; and you won't have to specify the -K utf-8 any more.
(setq inferior-lisp-program "/Applications/ccl/dx86cl64 -K utf-8")

(require 'slime)
(setq slime-net-coding-system 'utf-8-unix)
(slime-setup '(slime-fancy))
