(add-to-list 'load-path "~/.emacs.d")
(require 'tex-site)
(setq org-export-with-LaTeX-fragments t)

(setq org-format-latex-header "\\documentclass{article}
\\usepackage{fullpage}         % do not remove
\\usepackage{amssymb}
\\usepackage[usenames]{color}
\\usepackage{amsmath}
\\usepackage[utf8]{inputenc}
\\usepackage{latexsym}
\\usepackage[mathscr]{eucal}
\\usepackage[T1]{fontenc}
\\pagestyle{empty}             % do not remove")

(setq TeX-PDF-mode t)
(add-to-list 'safe-local-variable-values '((TeX-master . rapport) (TeX-master . "rapport")))

(define-globalized-minor-mode always-double-mode double-mode double-mode)

;; Flymake for LaTeX
(eval-after-load 'flymake
  '(progn
     (push
      '("^\\(\.+\.tex\\):\\([0-9]+\\):\\([0-9]+\\):\\(.+\\)"
	1 2 3 4) flymake-err-line-patterns)
     (defun flymake-get-tex-args (file-name)
       (list "chktex" (list "-g0" "-r" "-l"
			    (expand-file-name "~/.chktexrc")
			    "-I" "-q" "-v0" file-name)))
     ))

(eval-after-load 'latex '(progn
                           (defvar tex-compile-commands 'nil)
                           (add-to-list 'tex-compile-commands '("kpdf %r.pdf &" "%r.pdf"))
			   (add-hook 'LaTeX-mode-hook
				     (lambda () (speck-mode 1)))
			   (add-hook 'LaTeX-mode-hook
				     (lambda () (reftex-mode 1)))
			   (add-hook 'LaTeX-mode-hook
				     '(lambda () (highlight-fixmes-mode 1)))))