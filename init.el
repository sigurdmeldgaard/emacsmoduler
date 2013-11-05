(package-initialize)
(setq py-load-pymacs-p nil)

(load "byte-code-cache")
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives 
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))

(dolist (package '(yasnippet expand-region undo-tree sr-speedbar
       solarized-theme smex parenface paredit org mic-paren
       color-theme-solarized color-theme-sanityinc-solarized
       color-theme auctex all rainbow-delimiters magit
       haskell-mode ghc multiple-cursors ido-ubiquitous speck
       pymacs pysmell sml-mode key-chord iedit grep-o-matic
       drag-stuff d-mode browse-kill-ring exec-path-from-shell
       fold-dwim repository-root))
  (unless (package-installed-p package)
    (package-install package)))

(exec-path-from-shell-initialize)

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
    (require 'dbus))
  
(when (string-equal "darwin"    system-type)  
    ;; Standard browser.
    (setq browse-url-generic-program "open")
    (setenv "PATH" (concat
                    "/usr/texbin" ":"
                    (getenv "PATH")))
  ;; Use built-in viewer for pdf's
  (eval-after-load 'latex 
    '(progn
       (add-to-list 'TeX-output-view-style
                    '("^pdf$" "." "open %o %(outpage)"))
       (add-to-list 'LaTeX-verbatim-environments "comment")))
  )
 
(when (string-equal "windows-nt"   system-type)
  (setq w32-apps-modifier 'super)
  ;; Deactivate speck on windows for now...
  (defun speck-mode (s) (interactive) nil))
 
;;; Settings ====================
(setq undo-tree-mode-lighter "")

(global-undo-tree-mode)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
 
;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Transparently open compressed files
(auto-compression-mode t)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

(setq fill-column 80)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)


 
(defconst use-backup-dir t)
(setq backup-directory-alist (quote ((".*" . "~/backup/temp/")))
      version-control t                ; Use version numbers for backups
      kept-new-versions 5             ; Number of newest versions to keep
      kept-old-versions 2              ; Number of oldest versions to keep
      delete-old-versions t            ; Ask to delete excess backup versions?
      backup-by-copying-when-linked t) ; Copy linked files, don't rename.
 
(setq tramp-default-method "scp")
 
(setq browse-url-browser-function 'browse-url-generic)
 
(setq ispell-program-name "/usr/bin/aspell")
 
(load "fold-dwim")
(global-set-key (kbd "<f7>") 'fold-dwim-toggle)
(global-set-key (kbd "<M-f7>") 'fold-dwim-hide-all)
(global-set-key (kbd "<S-M-f7>") 'fold-dwim-show-all)
 
(ido-mode 1)

(ido-ubiquitous-mode 1)

(setq ido-file-extensions-order '(".tex" t))
 
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
 
(add-hook 'org-mode-hook 'reftex-mode)
(add-hook 'org-mode-hook 'highlight-fixmes-mode)
 
(setq org-agenda-files '("~/Dropbox/org/main.org" "~/Dropbox/org/habits.org"))
(setq org-refile-allow-creating-parent-nodes t)
(setq org-refile-targets `((nil :maxlevel . 9)
                           (,org-agenda-files :maxlevel . 9)))
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)
(setq org-default-notes-file "~/Dropbox/org/notes.org")
(define-key global-map [f3] 'org-capture)
(setq org-capture-templates
      '(("t" "todo" entry (file "~/Dropbox/org/refile.org")
         "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
        ("r" "respond" entry (file "~/Dropbox/org/refile.org")
         "* TODO Respond to %:from on %:subject\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
        ("n" "note" entry (file "~/Dropbox/refile.org")
         "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
        ("j" "Journal" entry (file+datetree "~/Dropbox/org/diary.org")
         "* %?\n%U\n" :clock-in t :clock-resume t)
        ("w" "org-protocol" entry (file "~/Dropbox/org/refile.org")
         "* TODO Review %c\n%U\n" :immediate-finish t)
        ("p" "Phone call" entry (file "~/Dropbox/org/refile.org")
         "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
        ("h" "Habit" entry (file+headline "~/Dropbox/org/habits.org" "Habits")
         "* NEXT %?\n%U\n%a\nSCHEDULED: %t .+1d/3d\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")))
(require 'fixpath)
 
(setq-default indent-tabs-mode nil)
 
(defun yes-or-no-p (prompt)
  "redirects to y-or-n-p"
  (y-or-n-p prompt))
 
 
(let ((byte-compile-warnings '())
      (byte-compile-verbose nil))
  (autoload 'browse-kill-ring "browse-kill-ring" "" t))
  
;;; By Stefan Monnier <foo at acm.org>.
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
 
;; Allow recursive minibuffers
(setq enable-recursive-minibuffers t)

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
 
;;; Visual simplification
(scroll-bar-mode 1)
(tool-bar-mode -1)
(setq inhibit-startup-message 1)
(abbrev-mode 1)
 
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
 
(autoload 'vc-ediff "vc-ediff" "" t)
 
;; (require 'find-cmd) ;; Deactivated for now
;; (autoload  "project-root") ;; Deactivated for now
 
(autoload 'mindent-mode "mindent" "" t) ; Doesn't seem to work
 
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
 
(require 'edit-server)
(setq edit-server-new-frame nil)
(edit-server-start)
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
	  (lambda () ;(speck-mode 1)
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
 
(setq org-replace-disputed-keys t)

;; Fontify org-mode code blocks
(setq org-src-fontify-natively t)

(setq org-log-done t)
(setq org-export-with-LaTeX-fragments t)
 
;Org capture
(setq org-directory "~/Dropbox/org/")
(setq org-default-notes-file (concat org-directory "notes.org"))
(define-key global-map "\C-cr" 'org-capture)
 
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t)
   (python . t)))
 
;(setq org-log-done t)
 
;;; python
 
;; Things we would like:
;; Rope
;; pyflakes // flymake
;; Pymacs for some sucky reason doesn't work
(add-hook 'python-mode-hook
	  '(lambda () (flymake-mode 1)))
 
;; Enables folding
(add-hook 'python-mode-hook 'hs-minor-mode)
 
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

(require 'dropdown-list)
(defun read-lines (filePath)
  "Return a list of lines in FILEPATH."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string
     (buffer-string) "\n" t)) )
 
(yas-global-mode 1)
(setq yas-prompt-functions '(yas/dropdown-prompt
			     yas/ido-prompt
			     yas/completing-prompt))
(yas-load-directory (concat emacsmoduler-path "/snippets"))
 
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

(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;; grep
 
(setq grep-o-matic-repository-root-function 'repository-root)
(repository-root "") ;; to activate load
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
 
;;; Hippie expand
(defvar he-search-loc-backward (make-marker))
(defvar he-search-loc-forward (make-marker))

(defun try-expand-dabbrev-closest-first (old)
  "Try to expand word \"dynamically\", searching the current buffer.
The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible expansions of the same
string).  It returns t if a new expansion is found, nil otherwise."
  (let (expansion)
    (unless old
      (he-init-string (he-dabbrev-beg) (point))
      (set-marker he-search-loc-backward he-string-beg)
      (set-marker he-search-loc-forward he-string-end))

    (if (not (equal he-search-string ""))
        (save-excursion
          (save-restriction
            (if hippie-expand-no-restriction
                (widen))

            (let (forward-point
                  backward-point
                  forward-distance
                  backward-distance
                  forward-expansion
                  backward-expansion
                  chosen)

              ;; search backward
              (goto-char he-search-loc-backward)
              (setq expansion (he-dabbrev-search he-search-string t))

              (when expansion
                (setq backward-expansion expansion)
                (setq backward-point (point))
                (setq backward-distance (- he-string-beg backward-point)))

              ;; search forward
              (goto-char he-search-loc-forward)
              (setq expansion (he-dabbrev-search he-search-string nil))

              (when expansion
                (setq forward-expansion expansion)
                (setq forward-point (point))
                (setq forward-distance (- forward-point he-string-beg)))

              ;; choose depending on distance
              (setq chosen (cond
                            ((and forward-point backward-point)
                             (if (< forward-distance backward-distance) :forward :backward))

                            (forward-point :forward)
                            (backward-point :backward)))

              (when (equal chosen :forward)
                (setq expansion forward-expansion)
                (set-marker he-search-loc-forward forward-point))

              (when (equal chosen :backward)
                (setq expansion backward-expansion)
                (set-marker he-search-loc-backward backward-point))

              ))))

    (if (not expansion)
        (progn
          (if old (he-reset-string))
          nil)
      (progn
        (he-substitute-string expansion t)
        t))))

(defun try-expand-line-closest-first (old)
  "Try to complete the current line to an entire line in the buffer.
The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible completions of the same
string).  It returns t if a new completion is found, nil otherwise."
  (let ((expansion ())
        (strip-prompt (and (get-buffer-process (current-buffer))
                           comint-use-prompt-regexp
                           comint-prompt-regexp)))
    (unless old
      (he-init-string (he-line-beg strip-prompt) (point))
      (set-marker he-search-loc-backward he-string-beg)
      (set-marker he-search-loc-forward he-string-end))

    (if (not (equal he-search-string ""))
        (save-excursion
          (save-restriction
            (if hippie-expand-no-restriction
                (widen))

            (let (forward-point
                  backward-point
                  forward-distance
                  backward-distance
                  forward-expansion
                  backward-expansion
                  chosen)

              ;; search backward
              (goto-char he-search-loc-backward)
              (setq expansion (he-line-search he-search-string
                                              strip-prompt t))

              (when expansion
                (setq backward-expansion expansion)
                (setq backward-point (point))
                (setq backward-distance (- he-string-beg backward-point)))

              ;; search forward
              (goto-char he-search-loc-forward)
              (setq expansion (he-line-search he-search-string
                                              strip-prompt nil))

              (when expansion
                (setq forward-expansion expansion)
                (setq forward-point (point))
                (setq forward-distance (- forward-point he-string-beg)))

              ;; choose depending on distance
              (setq chosen (cond
                            ((and forward-point backward-point)
                             (if (< forward-distance backward-distance) :forward :backward))

                            (forward-point :forward)
                            (backward-point :backward)))

              (when (equal chosen :forward)
                (setq expansion forward-expansion)
                (set-marker he-search-loc-forward forward-point))

              (when (equal chosen :backward)
                (setq expansion backward-expansion)
                (set-marker he-search-loc-backward backward-point))

              ))))

    (if (not expansion)
        (progn
          (if old (he-reset-string))
          ())
      (progn
        (he-substitute-string expansion t)
        t))))

;; Hippie expand: sometimes too hip
(setq hippie-expand-try-functions-list '(try-expand-dabbrev-closest-first
                                         try-complete-file-name
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-expand-all-abbrevs
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; Create own function to expand lines (C-S-.)
(defun hippie-expand-lines ()
  (interactive)
  (let ((hippie-expand-try-functions-list '(try-expand-line-closest-first
                                            try-expand-line-all-buffers)))
    (end-of-line)
    (hippie-expand nil)))

;; Don't case-fold when expanding with hippe
(defun hippie-expand-no-case-fold ()
  (interactive)
  (let ((case-fold-search nil))
    (hippie-expand nil)))


;;; Experiment

;;; Something like Alt-Tab switching between buffers
(defun bs-start () 
  (interactive)
  (define-key bs-mode-map "\M-`" 'bs-down)
  (define-key bs-mode-map "\M-~" 'bs-up)
  (define-key bs-mode-map "`" 'bs-select)
  (bs-show 0)
  (bs-down 1))
(global-set-key "\M-`" 'bs-start)
 
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

(global-set-key (kbd "C-M-g") 'magit-status)

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
 
(global-set-key	 (kbd "<kp-delete>") 'delete-char)
 
(global-set-key	 (kbd "s-w") 'kill-ring-save)
 
(global-set-key	 (kbd "s-:") 'eval-expression)
 
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
 
 
;(load-file (let ((coding-system-for-read 'utf-8))
;	      (shell-command-to-string "agda-mode locate")))
 
(add-to-list 'load-path (concat dist-elisp "agda-mode/"))
 
(load "agda-input")
(load "agda2")
 
(global-rainbow-delimiters-mode 1)
(set-face-attribute 'rainbow-delimiters-unmatched-face
  nil
  :box
   '(:line-width 2 :color "Red" :style released-button)
  :foreground  "Red")

(load "small") ;;; Some more homebrewed commands
 
(defun okular-make-url () (concat
               "file://"
               (expand-file-name (funcall file (TeX-output-extension) t)
                         (file-name-directory (TeX-master-file)))
               "#src:"
               (TeX-current-line)
               (expand-file-name (TeX-master-directory))
               "./"
               (TeX-current-file-name-master-relative)))

(add-hook 'LaTeX-mode-hook '(lambda ()
                  (add-to-list 'TeX-expand-list
                       '("%u" okular-make-url))))

(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

;(setq TeX-view-program-selection '((output-pdf "Okular") (output-dvi "Okular")))
(setq LaTeC-command "latex --synctex=1")
(setq TeX-source-correlate-method 'synctex)

(global-set-key (kbd "M-j")
            (lambda ()
                  (interactive)
                  (join-line -1)))

(provide 'init)
