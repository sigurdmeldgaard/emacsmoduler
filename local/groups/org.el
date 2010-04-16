(add-to-list 'load-path (concat grail-dist-elisp "org-mode/lisp/"))

(autoload 'org-export-bibtex-preprocess "contrib/lisp/org-exp-bibtex")

(require 'org-install)

(setq org-log-done t)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
;(setq org-agenda-files (quote ("~/izmir/todo.org")))
