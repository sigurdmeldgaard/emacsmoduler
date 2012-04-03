;;;; Things related to different modes

;;; mmm

(add-to-list 'load-path (concat dist-elisp "mmm-mode"))
(load "mmm-auto")

;;; proofgeneral

;(load-file (concat dist-elisp "ProofGeneral/generic/proof-site.el"))

;;; small-modes - this is stuff that is initialized in a single line

(autoload 'hoogle-mode "hoogle" nil t)
(autoload 'xquery-mode "xquery-mode" nil t)
(autoload 'haml-mode "haml-mode" nil t)
(autoload 'erlang-mode "erlang" nil t)
(autoload 'peep-mode "peep" nil t)
(autoload 'speck-mode "speck" nil t)
(autoload 'russian-mode "russian-mode" nil t)
(autoload 'tp-file-contents "typist" nil t)


;;; expand-region

(add-to-list 'load-path (concat dist-elisp "expand-region.el"))
(load "expand-region")
(global-set-key [s-up] 'er/expand-region)
(global-set-key [s-down] 'er/contract-region)

;;; lua

(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))

;;; d

(autoload 'd-mode "d-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.d\\'" . d-mode))


;;; edit-server

(add-hook 'edit-server-start-hook 
          (lambda () (set-visited-file-name 
                      (concat temporary-file-directory
                              (buffer-name)))))


;;; factor

(autoload 'factor-mode "~/leg/factor/misc/fuel/fu.el"
  "Major mode for editing Factor source." t)

(setq fuel-listener-factor-binary "~/leg/factor/factor")
(setq fuel-listener-factor-image "~/leg/factor/factor.image")


;;; gdb

(setq gdb-show-main t)
(setq gdb-many-windows t)


;;; gnuplot

(autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)


;;; haskell

(load (concat dist-elisp "haskell-mode-2.3/haskell-site-file.el"))

(add-hook 'haskell-mode-hook 'my-mmm-mode)

(mmm-add-classes
 '((literate-haskell-bird
    :submode text-mode
    :front "^[^>]"
    :include-front true
    :back "^>\\|$"
    )
   (literate-haskell-latex
    :submode literate-haskell-mode
    :front "^\\\\begin{code}"
    :front-offset (end-of-line 1)
    :back "^\\\\end{code}"
    :include-back nil
    :back-offset (beginning-of-line -1)
    )))

(defun my-mmm-mode ()
  ;; go into mmm minor mode when class is given
  (make-local-variable 'mmm-global-mode)
  (setq mmm-global-mode 'true))

(setq mmm-submode-decoration-level 0)


;;; install-elisp

(require 'auto-install)

;;; agda

;(add-to-list 'load-path (concat dist-elisp "agda-mode"))
;(require 'agda2)

;;; javascript

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq js2-basic-offset 2)
(setq js2-use-font-lock-faces t)

;;; Latex
            
(add-hook 'LaTeX-mode-hook
          (lambda () (speck-mode 1)
            (visual-line-mode 1)
            (TeX-fold-mode 1)            
            (highlight-fixmes-mode 1)
            (turn-on-reftex)))

(setq org-export-with-LaTeX-fragments t)
 
(setq org-format-latex-header "\\documentclass{article}
\\usepackage{fullpage}	       % do not remove
\\usepackage{amssymb}
\\usepackage[usenames]{color}
\\usepackage{amsmath}
\\usepackage[utf8]{inputenc}
\\usepackage{latexsym}
\\usepackage[mathscr]{eucal}
\\usepackage[T1]{fontenc}
\\pagestyle{empty}	       % do not remove")
 
(setq TeX-PDF-mode t)

;;; Flymake for LaTeX
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
 
;;; makefile

(add-hook 'makefile-mode-hook (lambda () (setq indent-tabs-mode t)))


;;; micropython

(defun linux-c-mode ()
  "C mode with adjusted defaults for use with Paolo!"
  (interactive)
  (c-mode)
  (c-set-style "linux")
  (setq tab-width 8)
  (setq indent-tabs-mode t)
  (setq c-basic-offset 8)
  (yas/define 'c-mode "LABEL" "LABEL_BEGIN(${1:OPCODE_NAME});
    $0
    LABEL_END($1);" "LABEL_BEGIN(OPCODE) ...")
  (yas/define 'c-mode "pr" "printf(\"Printing %s:%d %d$0\\n\", __FILE__, __LINE__ $1);"
	      "printf(\"...\\n\");"))

(add-to-list 'auto-mode-alist
	     '(".*/micropython.*/.*\\.[ch]$"
	       . linux-c-mode))

(add-hook 'c-mode-common-hook
	  '(lambda () (highlight-fixmes-mode 1)))

(add-hook 'c-mode-common-hook
  (lambda() 
    (local-set-key  (kbd "C-c o") 'ff-find-other-file)))


;;; org

(setq org-log-done t)
(setq org-export-with-LaTeX-fragments t)

(org-remember-insinuate)
(setq org-directory "~/Dropbox/org/")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cr" 'org-remember)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t)))

; (setq org-remember-templates
;      `(("Todo" ?t "* TODO %?\n  %i\n  %a" 
;         ,(concat org-directory "/TASKS.org") "Tasks")
;        ("Journal" ?j "* %U %?\n\n  %i\n  %a" 
;         ,(concat org-directory "/JOURNAL.org") "Journal")
;        ("Idea" ?i "* %^{Title}\n  %i\n  %a" 
;         ,(concat org-directory "/IDEAS.org")
;         "New Ideas")))
; 
;(setq org-log-done t)


;;; python

;(load "pymacs")
;(load "python")
; 
;;;(eval-after-load "pymacs"
;;;  '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))
; 
;(defun load-ropemacs ()
;    "Load pymacs and ropemacs"
;    (interactive)
;    (require 'pymacs)
;    (pymacs-load "ropemacs" "rope-")
;    ;; Automatically save project python buffers before refactorings
;    (setq ropemacs-confirm-saving 'nil))
; 
;;(add-hook 'python-mode-hook 'load-ropemacs)
;;(autoload 'pysmell-mode "pysmell" "Code completion for python" t)
; 
;(when (load "flymake" t)
;  (defun flymake-pyflakes-init ()
;    (let* ((temp-file (flymake-init-create-temp-buffer-copy
;                       'flymake-create-temp-inplace))
;           (local-file (file-relative-name
;                        temp-file
;                        (file-name-directory buffer-file-name))))
;      (list "pycheck" (list local-file))))
; 
;  (add-to-list 'flymake-allowed-file-name-masks
;               '("\\.py\\'" flymake-pyflakes-init)))
; 
;;(setq python-mode-hook nil)
;;(add-hook 'python-mode-hook
;;          '(lambda () (eldoc-mode 1)))
;;(add-hook 'python-mode-hook
;;          '(lambda () (flymake-mode 1)))
; 
;;(add-hook 'find-file-hook 'flymake-find-file-hook)
; 
;(setq python-python-command "python2.6")

;;; sage

(add-to-list 'load-path (expand-file-name "~/sage/data/emacs"))
(autoload 'sage-mode "sage-mode")

(setq sage-command "sage")

;; 
;;;; If you want sage-view to typeset all your output and have plot()
;;;; commands inline, uncomment the following line and configure sage-view:
;;(require 'sage-view "sage-view")
;;(add-hook 'sage-startup-hook 'sage-view)
;; You can use commands like
;; (add-hook 'sage-startup-hook 'sage-view
;; 'sage-view-disable-inline-output 'sage-view-disable-inline-plots)
;; to have some combination of features.  In future, the customize interface
;; will make this simpler... hint, hint!


;;; xml

(require 'nxml-enc)
(autoload 'php-mode "php-mode" nil t)

(setq nxml-slash-auto-complete-flag t)
(add-to-list 'auto-mode-alist '("\\.php[34]?\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.htm[l]?\\'" . nxml-mode))


;;; snippets

(add-to-list 'load-path (expand-file-name (concat dist-elisp "yasnippet")))
(load "yasnippet")
(require 'dropdown-list)
(defun read-lines (filePath)
  "Return a list of lines in FILEPATH."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string
     (buffer-string) "\n" t)) )

(setq yas/prompt-functions '(yas/dropdown-prompt
			     yas/ido-prompt
			     yas/completing-prompt))
(yas/load-directory (concat emacsmoduler-path "/snippets"))
(yas/define-snippets 'nxhtml-mode nil 'html-mode)
(yas/global-mode 1)
(setq yas/wrap-around-region 'nil)
;(require 'snippet)

;;; html

(add-to-list 'auto-mode-alist '("\\.html" . nxhtml-mode))

;;; java

(when (featurep 'flymake)
  (setq flymake-allowed-file-name-masks
        (remove-if '(lambda (a) (equal (car a) "\\.java\\'")) flymake-allowed-file-name-masks)))

(defun java-mode-untabify ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[ \t]+$" nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    (goto-char (point-min))
    (if (search-forward "\t" nil t)
	(untabify (1- (point)) (point-max))))
  nil)

(add-hook 'java-mode-hook 
	  '(lambda ()
	     (make-local-variable 'write-contents-hooks)
	     (add-hook 'write-contents-hooks 'java-mode-untabify)))
;;; c

(when (featurep 'flymake)
  (setq flymake-allowed-file-name-masks
        (remove-if '(lambda (a) (equal (car a) "\\.c\\'")) flymake-allowed-file-name-masks))
  (setq flymake-allowed-file-name-masks
        (remove-if '(lambda (a) (equal (car a) "\\.h\\'")) flymake-allowed-file-name-masks))
  (setq flymake-allowed-file-name-masks
        (remove-if '(lambda (a) (equal (car a) "\\.cpp\\'")) flymake-allowed-file-name-masks)))

;;; sml

(autoload 'sml-mode "sml-mode" "Major mode for editing SML." t)
(autoload 'run-sml "sml-proc" "Run an inferior SML process." t)

(add-to-list (quote auto-mode-alist) (quote ("\\.s?\\(ml\\|ig\\)\\'" . sml-mode)))

(defun sml-inside-comment ()
  (eq (get-text-property (point) 'face)
		       'font-lock-comment-face))

(defun sml-fill-comment-paragraph (&optional justify)
  "Fill current comment.
If we're not in a comment, just return nil."
  (if (sml-inside-comment)
      (save-excursion
	(let ((a (comment-beginning))
	      (fill-prefix
	       (save-excursion
		 (next-line)
		 (beginning-of-line)
		 (if (sml-inside-comment)
		     (let ((p (point)))
		       (search-forward-regexp "^[* \t]*")
		       (buffer-substring-no-properties p (point)))
		   fill-prefix)))
	      (b (progn
		   (search-forward-regexp comment-end)
		   (comment-enter-backward))))
	  (fill-region a b)))
    nil))
  
(defun sml-fill-paragraph (&optional justify)
  (or (sml-fill-comment-paragraph justify)
      nil))

(add-hook 'sml-mode-hook
	  '(lambda ()
	    (setq fill-paragraph-function 'sml-fill-paragraph)))


;;; sage

(add-to-list 'load-path (expand-file-name "~/sage/data/emacs"))
(autoload 'sage-mode "sage-mode")

(setq sage-command "sage")

;; 
;;;; If you want sage-view to typeset all your output and have plot()
;;;; commands inline, uncomment the following line and configure sage-view:
;;(require 'sage-view "sage-view")
;;(add-hook 'sage-startup-hook 'sage-view)
;; You can use commands like
;; (add-hook 'sage-startup-hook 'sage-view
;; 'sage-view-disable-inline-output 'sage-view-disable-inline-plots)
;; to have some combination of features.  In future, the customize interface
;; will make this simpler... hint, hint!


;;; lisp

(set-language-environment "utf-8")

(add-to-list 'load-path "~/emacsmoduler/dist/elisp/slime/")

;;; Note that if you save a heap image, the character
;;; encoding specified on the command line will be preserved,
;;; and you won't have to specify the -K utf-8 any more.
(setq inferior-lisp-program "/Applications/ccl/dx86cl64 -K utf-8")

;;; isabelle

(eval-after-load 'proof-script
  '(progn
     (define-key proof-mode-map (kbd "C-M-8")
       (lambda () (interactive)
         (unicode-tokens-mode)))
     (add-to-list 'isar-shortcut-alist '("``" . "´"))
     (add-to-list 'isar-shortcut-alist '("/\\\\" . "⋀"))
     (add-to-list 'isar-shortcut-alist '("{|" . "⦃"))
     (add-to-list 'isar-shortcut-alist '("|}" . "⦄"))))

;;; ediff

(setq ediff-split-window-function 'split-window-horizontally)

;;; grep

(require 'repository-root) ; optional: needed for repository-wide search
(require 'grep-o-matic)
(setq grep-o-matic-repository-root-function 'repository-root)
(add-to-list 'repository-root-matchers repository-root-matcher/git)
(add-to-list 'repository-root-matchers repository-root-matcher/hg)
(add-to-list 'repository-root-matchers repository-root-matcher/darcs)
(add-to-list 'repository-root-matchers repository-root-matcher/autoconf)
(add-to-list 'repository-root-matchers repository-root-matcher/bzr)
(add-to-list 'repository-root-matchers repository-root-matcher/svn)
(add-to-list 'repository-root-matchers repository-root-matcher/cvs)


;;; compilation

(setq compilation-directory-matcher
 '("\\(?:\\(?:Entering\\|Leavin\\(g\\)\\) directory
`\\(.+\\)'$\\)\\|\\(?:[^]^[]*\\][[:space:]]*\\(\\(?:[[:alnum:]]*/\\)+\\)\\)\\|\\(?:^\\(\\[\\)\\)"
(2 . 1) (3 . 9) (4 . 4)))

(require 'ansi-color)
;(defun colorize-compilation-buffer ()
;  (toggle-read-only)
;  (ansi-color-apply-on-region (point-min) (point-max))
;  (toggle-read-only))
;(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(add-to-list 'comint-preoutput-filter-functions
             'ansi-color-apply)


;;;; yaml

(autoload 'yaml-mode "yaml-mode")
(add-to-list 'auto-mode-alist '("\\.yml\\|\\.yaml" . yaml-mode))

;;; html

(add-to-list 'auto-mode-alist '("\\.html" . nxhtml-mode))
