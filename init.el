(package-initialize)

(load "byte-code-cache")
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(dolist (package '(yasnippet yas-jit yasnippet-bundle
       expand-region undo-tree sr-speedbar
       solarized-theme smex parenface paredit
       org mic-paren color-theme-solarized
       color-theme-sanityinc-solarized color-theme
       auctex all rainbow-delimiters magit mark-multiple
       mark-more-like-this ido-ubiquitous haskell-mode))
  (unless (package-installed-p package)
    (package-install package)))


(defvar dist-elisp (concat emacsmoduler-path "/dist/elisp/"))
(defvar local-elisp (concat emacsmoduler-path "/local/elisp/"))

(add-to-list 'load-path dist-elisp)
(add-to-list 'load-path local-elisp)

(cond
 ((daemonp) ())
 ((window-system) 
    (load "gui")))

;;; System specific ====================
(when (string-equal "gnu/linux" system-type)  
    nil)
(when (string-equal "darwin"    system-type)  
    ;; Standard browser.
    (setq browse-url-generic-program "open")

  ;; Use built-in viewer for pdf's
  (eval-after-load 'latex 
    '(progn
       (add-to-list 'TeX-output-view-style
                    '("^pdf$" "." "open %o %(outpage)"))))

                                        ;(require 'growl)
  )

(when (string-equal "windows-nt"   system-type)
  (setq w32-apps-modifier 'super)
  ;; Deactivate speck on windows for now...
  (defun speck-mode (s) (interactive) nil))

;;; Settings ====================
(global-undo-tree-mode)

(when (featurep 'mule)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'latin-1)
  (set-language-environment 'utf-8)
  (server-start))

(if (locate-library "edit-server")
    (progn
      (require 'edit-server)
      (setq edit-server-new-frame nil)
      (edit-server-start)))

(defconst use-backup-dir t)
(setq backup-directory-alist (quote ((".*" . "~/backup/temp/")))
      version-control t                ; Use version numbers for backups
      kept-new-versions 5             ; Number of newest versions to keep
      kept-old-versions 2              ; Number of oldest versions to keep
      delete-old-versions t            ; Ask to delete excess backup versions?
      backup-by-copying-when-linked t) ; Copy linked files, don't rename.

(setq tramp-default-method "scp")

(setq browse-url-browser-function 'browse-url-generic)

(setq ispell-program-name "aspell")

(ido-ubiquitous-mode 1)

(defun ido-find-tag ()
  "Find a tag using ido"
  (interactive)
  (tags-completion-table)
  (let (tag-names)
    (mapc (lambda (x)
            (unless (integerp x)
              (push (prin1-to-string x t) tag-names)))
          tags-completion-table)
    (find-tag (ido-completing-read "Tag: " tag-names nil nil (thing-at-point 'symbol)))))

;Hacks to make AUCtex happy
(setq byte-compile-verbose t)
(setq byte-compile-warnings t)
(setq TeX-save-query nil) ;;autosave before compiling


(require 'dbus)

(defun th-evince-sync (file linecol)
  (message "hej")
  (let ((buf (get-buffer file))
        (line (car linecol))
        (col (cadr linecol)))
    (if (null buf)
        (message "Sorry, %s is not opened..." file)
      (switch-to-buffer buf)
      (goto-line (car linecol))
      (unless (= col -1)
        (move-to-column col)))))

(setq org-agenda-files '("~/Dropbox/org/main.org"))

(dbus-register-signal
 :session nil "/org/gnome/evince/Window/0"
 "org.gnome.evince.Window" "SyncSource"
 'th-evince-sync)


(require 'fixpath)

;;;; Commands ====================
(setq-default indent-tabs-mode nil)

(defun yes-or-no-p (prompt)
  "redirects to y-or-n-p"
  (y-or-n-p prompt))


(let ((byte-compile-warnings '())
      (byte-compile-verbose nil))
  (require 'browse-kill-ring)
  (browse-kill-ring-default-keybindings))

(load "ido")
(ido-mode 1)

;;; Stefan Monnier <foo at acm.org>.
;;; From: http://www.emacswiki.org/emacs/UnfillParagraph
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(when (or (functionp 'cua-mode) (featurep 'cua))
  (cua-mode 1)
  (setq
   cua-auto-mark-last-change t
   cua-enable-cua-keys t
   cua-enable-register-prefix (quote not-ctrl-u)
   cua-highlight-region-shift-only nil
   cua-keep-region-after-copy nil)
(define-key cua--rectangle-keymap " "     'self-insert-command)
(define-key cua--rectangle-keymap "("     'self-insert-command)
(define-key cua--rectangle-keymap ")"     'self-insert-command))


(setq completion-ignore-case t      ; ignore case when completing...
  read-file-name-completion-ignore-case t) ; ...filenames too

(setq scroll-conservatively 10000)  ; smooth scrolling


(setq search-highlight t)           ; highlight when searching... 
(setq query-replace-highlight t)    ; ...and replacing

(savehist-mode 1)
(column-number-mode 1)

(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'top)

;;;Use of time
(display-time)
(add-hook 'diary-hook 'appt-make-list)
(set-variable 'timeclock-modeline-display t)

(scroll-bar-mode 1)
(tool-bar-mode -1)
(setq inhibit-startup-message 1)
(abbrev-mode 1)

(require 'font-lock)
;(setq font-lock-support-mode 'lazy-lock-mode)
(setq font-lock-maximum-decoration t)
(global-font-lock-mode t)

(require 'highlight-fixmes-mode)
(highlight-fixmes-mode t)

;(when (eq window-system 'x)
;  (set-face-font 'default "-*-fixed-medium-r-*-*-12-*-*-*-*-*-iso8859-*");
;  ;; In "fontset-standard" use "misc fixed" for the charset
;  ;; `mule-unicode-0100-24ff'
;  (set-fontset-font "fontset-standard"
;                    'mule-unicode-0100-24ff
;                   "-*-fixed-medium-r-*-*-12-*-*-*-*-*-iso10646-1")
;  (shell-command
;   (concat "xmodmap "
;          "-e 'keycode 115 = Hyper_R' "
;           "-e 'add mod4 = Hyper_R' ")))

(setq speedbar-directory-unshown-regexp "^\\(.HG\\|.CVS\\|.RCS\\|.SCCS\\|_DARCS\\|\\..*\\)\\'")
(setq speedbar-use-images nil) 

(require 'better-registers)
(better-registers-install-save-registers-hook)
(setq better-registers-save-file "~/.registers.el")
(if (file-exists-p better-registers-save-file)
    (load better-registers-save-file))

;(require 'command-frequency)
;(command-frequency-mode 1)
;(command-frequency-table-load "~/.emacs.frequencies")
;(command-frequency-autosave-mode 1)

(load "ido")
(ido-mode 1)

(require 'vc-ediff)
(require 'find-cmd)
(require 'project-root)
(require 'mindent)

(setq custom-file (concat emacsmoduler-path "/custom.el"))
(load custom-file)

(define-key isearch-mode-map [(control shift o)]
      (lambda () (interactive)
        (let ((shk-search-string isearch-string))
          (grep-compute-defaults)
          (lgrep (if isearch-regexp shk-search-string (regexp-quote shk-search-string))
                 (format "*.%s" (file-name-extension (buffer-file-name)))
                 default-directory)
          (isearch-abort))))

(defun isearch-occur ()
      "Invoke `occur' from within isearch."
      (interactive)
      (let ((case-fold-search isearch-case-fold-search))
        (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))

(define-key isearch-mode-map (kbd "C-M-o") 'isearch-occur)

(require 'incr-at-point)

(let ((byte-compile-warnings '())
      (byte-compile-verbose nil))
  (defadvice kill-ring-save (before slick-copy activate compile)
    "When called interactively with no active region, copy a single line instead."
    (interactive
     (if mark-active (list (region-beginning) (region-end))
       (message "Copied line")
       (list (line-beginning-position)
             (line-beginning-position 2)))))

  (defadvice completion-kill-region (before slick-cut activate compile)
    "When called interactively with no active region, kill a single line instead."
    (interactive
     (if mark-active (list (region-beginning) (region-end))
       (list (line-beginning-position)
             (line-beginning-position 2))))))

(defun forward-current-word-keep-offset ()
  (interactive)
  (find-current-word 'forward))

(defun backward-current-word-keep-offset ()
  (interactive)
  (find-current-word 'backward))
  
(defun find-current-word (direction)
  (let* ((curword (thing-at-point 'symbol))
         (re-curword (concat "\\<" (thing-at-point 'symbol) "\\>"))
         (offset (point)) 
         (case-fold-search nil))
    (beginning-of-thing 'symbol)
    (setq offset (- offset (point)))	; offset from start of symbol/word
    (if (eq direction 'forward)
        (setq offset (- offset (length curword)))) ; offset from end
    (forward-char)
    (let ((search-direction
           (if (eq direction 'forward)
               (function re-search-forward)
             (function re-search-backward)))
          (startover-point
           (if (eq direction 'forward)
               (point-min)
             (point-max))))
      (if (funcall search-direction re-curword nil t)
          (forward-char offset)
        ;; else
        (progn (goto-char startover-point)
               (if (funcall search-direction re-curword nil t)
                   (progn (message "Searching from bottom. %s" (what-line))
                          (forward-char offset))
                 ;; else
                 (message "Searching from bottom: Not found"))
               )))))

(add-hook 'server-switch-hook 
            (lambda ()
              (when (current-local-map)
                (use-local-map (copy-keymap (current-local-map))))
              (local-set-key (kbd "C-c C-c") '(lambda ()
                                                (interactive)
                                                (save-buffer)
                                                (server-edit)))))

;;;; Things related to different modes ====================

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
(autoload 'mark-next-like-this "mark-more-like-this" nil t)
(autoload 'mark-previous-like-this "mark-more-like-this" nil t)
(autoload 'mark-all-like-this "mark-more-like-this" nil t)

(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)
(global-set-key (kbd "C-*") 'mark-all-like-this)

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

(defun run-latex ()
  (interactive)
  (save-buffer)
  (TeX-command "LaTeX" 'TeX-master-file nil))

(add-hook 'LaTeX-mode-hook
          (lambda () (speck-mode 1)
            (visual-line-mode 1)
            (TeX-fold-mode 1)            
            (define-key LaTeX-mode-map [f5] 'run-latex)
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

;;;; Keybindings ====================
;(setq skeleton-pair t)
;(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
;(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
;(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
;(global-set-key (kbd "<") 'skeleton-pair-insert-maybe)

(load (concat dist-elisp "drag-stuff"))
(drag-stuff-global-mode t)

(global-set-key "\C-\M-d"
                (lambda ()
                  (interactive)
                  (ispell-change-dictionary "dansk")))

(global-set-key "\C-\M-e"
                (lambda ()
                  (interactive)
                  (ispell-change-dictionary "english")))

(global-set-key "\C-p" 'isearch-backward-current-word-keep-offset)
(global-set-key "\C-n" 'isearch-forward-current-word-keep-offset)


(global-set-key "\C-ca" 'org-agenda)

;;; Redefining keys:
(global-set-key "\C-xf" 'find-file-at-point)
(global-set-key "\C-x\C-b" 'bs-show)
(global-set-key "\C-c\C-k" 'compile)
(global-set-key [f11] 'hippie-expand)
(global-set-key [f6] 'hippie-expand)
(global-set-key (kbd "C-:") 'hippie-expand-lines)
;(global-set-key (kbd "<return>") 'newline-and-indent)


(global-set-key [C-tab] 'goto-matching-paren)
(global-set-key [S-return] 'open-my-way)
(global-set-key "\C-o" 'find-file-at-point)
(global-set-key "\C-f" 'isearch-forward)

(global-set-key (kbd "C-;") 'iedit-mode)

(global-set-key "\C-s" 'save-buffer)
(define-key isearch-mode-map "\C-f" 'isearch-repeat-forward)
(define-key isearch-mode-map "\C-g" 'isearch-repeat-forward)
(define-key isearch-mode-map [backspace] 'isearch-delete-char)

(global-unset-key "\C-d")
(set-variable 'kill-whole-line t)
(global-set-key [delete] 'delete-char)
(global-set-key "\C-d" 'kill-whole-line)

(global-unset-key "\C-z")
(global-set-key "\C-z" 'advertised-undo)

(global-set-key "\C-G" 'magit-status)
(global-set-key "\M-c" 'toggle-current-word)

(global-set-key [(f10)] 'sr-speedbar-toggle)

(defun compile-using-last-command ()
  (interactive)
  (let
      ((compilation-read-command nil))
    (compile (car compile-history))))

(global-set-key [(f5)] 'compile-using-last-command)
(global-set-key [(S-f5)] 'compile)

(defun smex () (interactive) (smex-initialize) (smex))
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'smex-update-and-run)
; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(global-set-key (kbd "M-.") 'ido-find-tag)

(global-set-key "\M-\C-g" 'rgrep)

(global-set-key "\M-/" 'pop-tag-mark)

(global-set-key "\C-x\C-r" 'rename-file-and-buffer)

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (define-key c-mode-base-map (kbd "C-d") 'kill-whole-line)))


(when (not (featurep 'x))
    (global-set-key "\C-hk" 'describe-key)
    (global-set-key [backspace] 'backward-delete-char-untabify)
    (global-set-key [f4] 'help-map))


(global-unset-key "\M-a")
(global-set-key "\M-a" 'increment-number-at-point)
(global-set-key "\M-A" (lambda (&optional n)
			       (interactive "p")
			       (increment-number-at-point (or (- (abs n))
							      -1))))

 (global-set-key [C-M-down] 'forward-current-word-keep-offset)
 (global-set-key [C-M-up] 'backward-current-word-keep-offset)

(define-key comint-mode-map [C-up] 'comint-previous-matching-input-from-input)
(define-key comint-mode-map [C-down] 'comint-next-matching-input-from-input)

(eval-after-load "vc-hooks"
         '(define-key vc-prefix-map "+" 'ediff-revision))

;(require 'key-chord)
;(key-chord-define-global "cv" 'reindent-then-newline-and-indent)
;(key-chord-define-global "]\" 'reindent-then-newline-and-indent)

(global-set-key (kbd "s-q") 'fill-paragraph)

(global-set-key (kbd "s-p") 'undefined)

(global-set-key  (kbd "<kp-delete>") 'delete-char)

(global-set-key  (kbd "s-w") 'kill-ring-save)

(global-set-key  (kbd "s-:") 'eval-expression)

(global-set-key (kbd "s-f") 'ido-choose-from-recentf)

(defun char-on-key (key char)
  (global-set-key key 
                  (lexical-let ((ch char)) 
                    (lambda () (interactive) (insert-char ch 1)))))

(char-on-key (kbd "s-[") ?å)
(char-on-key (kbd "s-{") ?Å)
(char-on-key (kbd "s-'") ?ø)
(char-on-key (kbd "s-\"") ?Ø)
(char-on-key (kbd "s-;") ?æ)
(char-on-key (kbd "s-:") ?Æ)

(load (concat dist-elisp "agda-mode/agda-input"))

(load "small")

(provide 'init)
