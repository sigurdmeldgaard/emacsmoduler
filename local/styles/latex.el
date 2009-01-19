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

(eval-after-load 'latex '(progn
			   (add-to-list 'LaTeX-mode-hook
					(lambda () (speck-mode 1)))
			   (add-hook 'LaTeX-mode-hook
				     '(lambda () (highlight-fixmes-mode 1)))))