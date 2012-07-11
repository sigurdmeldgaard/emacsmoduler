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
