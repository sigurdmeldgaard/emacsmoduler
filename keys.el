;(setq skeleton-pair t)
;(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
;(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
;(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
;(global-set-key (kbd "<") 'skeleton-pair-insert-maybe)

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

(global-set-key [(meta up)] 'move-line-up)
(global-set-key [(meta down)] 'move-line-down)

;;; Redefining keys:
(global-set-key "\C-xf" 'find-file-at-point)
(global-set-key "\C-x\C-b" 'bs-show)
(global-set-key "\C-c\C-k" 'compile)
(global-set-key [f11] 'hippie-expand)
(global-set-key [f6] 'hippie-expand)
(global-set-key "\r" 'newline-and-indent)
(global-set-key [C-tab] 'goto-matching-paren)
(global-set-key [S-return] 'open-my-way)
(global-set-key "\C-o" 'find-file-at-point)
(global-set-key "\C-f" 'isearch-forward)

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

(defun quick-compile ()
  (interactive)
  (let
      ((compilation-read-command nil))
    (compile (car compile-history))))

(global-set-key [(f5)] 'quick-compile)


(global-set-key "\M-\C-g" 'grep)

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

;(require 'key-chord)
;(key-chord-define-global "cv" 'reindent-then-newline-and-indent)
;(key-chord-define-global "]\" 'reindent-then-newline-and-indent)
