(setq indent-tabs-mode nil)

(defun yes-or-no-p (prompt)
  "redirects to y-or-n-p"
  (y-or-n-p prompt))

(when (or (functionp 'cua-mode) (featurep 'cua))
  (cua-mode 1)
  (setq
   cua-auto-mark-last-change t
   cua-enable-cua-keys t
   cua-enable-register-prefix (quote not-ctrl-u)
   cua-highlight-region-shift-only nil
   cua-keep-region-after-copy nil))

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

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
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

(require 'command-frequency)
(command-frequency-mode 1)
(command-frequency-table-load)
(command-frequency-autosave-mode 1)

(load "ido")
(ido-mode 1)

(require 'vc-ediff)
(require 'find-cmd)
(require 'project-root)

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