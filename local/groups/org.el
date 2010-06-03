(org-remember-insinuate)
(setq org-directory "/home/stm/unihome/orgland")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cr" 'org-remember)

 (setq org-remember-templates
      `(("Todo" ?t "* TODO %?\n  %i\n  %a" 
         ,(concat org-directory "/TASKS.org") "Tasks")
        ("Journal" ?j "* %U %?\n\n  %i\n  %a" 
         ,(concat org-directory "/JOURNAL.org") "Journal")
        ("Idea" ?i "* %^{Title}\n  %i\n  %a" 
         ,(concat org-directory "/IDEAS.org")
         "New Ideas")))

(add-to-list 'load-path (concat grail-dist-elisp "org-mode/lisp/"))
(require 'org-install)
(setq org-log-done t)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
;(setq org-agenda-files (quote ("~/izmir/todo.org")))
