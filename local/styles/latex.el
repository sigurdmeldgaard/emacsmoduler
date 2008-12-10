(setq org-export-with-LaTeX-fragments t)
(eval-after-load 'latex '(add-to-list 'LaTeX-mode-hook (lambda () (speck-mode 1))))

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
