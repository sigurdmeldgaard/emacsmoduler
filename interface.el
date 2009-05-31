(setq-default indent-tabs-mode nil)

(defun yes-or-no-p (prompt)
  "redirects to y-or-n-p"
  (y-or-n-p prompt))

(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
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
           (line-beginning-position 2)))))

 (defun forward-current-word-keep-offset ()
  (interactive)
  (find-current-word 'forward))

(defun backward-current-word-keep-offset ()
  (interactive)
  (find-current-word 'backward))
  
(defun find-current-word-keep-offset (direction)
  (let ((re-curword) (curword) (offset (point)) 
        (old-case-fold-search case-fold-search) )
    (setq curword (thing-at-point 'symbol))
    (setq re-curword (concat "\\<" (thing-at-point 'symbol) "\\>") )
    (beginning-of-thing 'symbol)
    (setq offset (- offset (point)))	; offset from start of symbol/word
    (if (eq direction 'forward)
        (setq offset (- (length curword) offset))) ; offset from end
    (forward-char)
    (setq case-fold-search nil)
    (let ((search-direction (if (eq direction 'forward) (function re-search-forward)
                              (function re-search-backward))))
      (if (funcall search-direction re-curword nil t)
          (forward-char offset)
        ;; else
        (progn (goto-char (point-max))
               (if (funcalll search-direction re-curword nil t)
                   (progn (message "Searching from bottom. %s" (what-line))
                          (forward-char offset))
                 ;; else
                 (message "Searching from bottom: Not found"))
               ))
      (setq case-fold-search old-case-fold-search))))

;; (defadvice kill-buffer (around my-kill-buffer-check activate)
;;   "Prompt when a buffer is about to be killed."
;;   (let* ((buffer-file-name (buffer-file-name))
;;          backup-file)
;;     ;; see 'backup-buffer
;;     (if (and (buffer-modified-p)
;;              buffer-file-name
;;              (file-exists-p buffer-file-name)
;;              (setq backup-file (car (find-backup-file-name buffer-file-name))))
;;         (let ((answer (completing-read (format "Buffer modified %s, (d)iff, (s)ave, (k)ill? " (buffer-name))
;;                                        '("d" "s" "k") nil t)))
;;           (cond ((equal answer "d")
;;                  (set-buffer-modified-p nil)
;;                  (let ((orig-buffer (current-buffer))
;;                        (file-to-diff (if (file-newer-than-file-p buffer-file-name backup-file)
;;                                          buffer-file-name
;;                                        backup-file)))
;;                    (set-buffer (get-buffer-create (format "%s last-revision" (file-name-nondirectory file-to-diff))))
;;                    (buffer-disable-undo)
;;                    (insert-file-contents file-to-diff nil nil nil t)
;;                    (set-buffer-modified-p nil)
;;                    (setq buffer-read-only t)
;;                    (ediff-buffers (current-buffer) orig-buffer)))
;;                 ((equal answer "k")
;;                  (set-buffer-modified-p nil)
;;                  ad-do-it)
;;                 (t
;;                  (save-buffer)
;;                  ad-do-it)))
;;       ad-do-it)))
