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

(load "loadfiles")
(load "keys")
(load "small")

(provide 'init)
