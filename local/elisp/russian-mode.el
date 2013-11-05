; -*- coding: mule-utf-8 -*-
(defgroup russian-mode nil
  "Stuff for using russian-yawerty and danish keyboards together, and other nice stuff for writing bilingual")

(defvar russian-mode-map (make-sparse-keymap))
(define-key russian-mode-map [(super shift q)] 'toggle-input-method)
(define-key russian-mode-map [(super shift z)] 'dansk)
(define-key russian-mode-map [(super shift k)] 'compile)
(define-key russian-mode-map [(super shift x)] 'russisk)
(define-key russian-mode-map [(super shift s)] 'acute-it)
(define-key russian-mode-map [(super shift a)] 'acute-a)
(define-key russian-mode-map [(super shift e)] 'acute-e)
(define-key russian-mode-map [(super shift y)] 'acute-y)
(define-key russian-mode-map [(super shift y)] 'acute-u)
(define-key russian-mode-map [(super shift o)] 'acute-o)
(define-key russian-mode-map [(super shift q)] 'acute-ja)
(define-key russian-mode-map [(super shift w)] 'insert-rid)
(define-key russian-mode-map [(super shift n)] 'insert-ri)
(define-key russian-mode-map [(super shift j)] 'insert-di)
(define-key russian-mode-map [(super shift v)] 'insert-mitem)

(define-key russian-mode-map [(super q)] 'toggle-input-method)
(define-key russian-mode-map [(super z)] 'dansk)
(define-key russian-mode-map [(super k)] 'compile)
(define-key russian-mode-map (kbd "M-1") 'acute)
(define-key russian-mode-map [(super x)] 'russisk)
;(define-key russian-mode-map [(super s)] 'acute-it)
;(define-key russian-mode-map [(super a)] 'acute-a)
;(define-key russian-mode-map [(super e)] 'acute-e)
;(define-key russian-mode-map [(super y)] 'acute-y)
;(define-key russian-mode-map [(super y)] 'acute-u)
;(define-key russian-mode-map [(super o)] 'acute-o)
;(define-key russian-mode-map [(super q)] 'acute-ja)
;(define-key russian-mode-map [(super w)] 'insert-rid)
;(define-key russian-mode-map [(super n)] 'insert-ri)
;(define-key russian-mode-map [(super j)] 'insert-di)
;(define-key russian-mode-map [(super v)] 'insert-mitem)
(defun acute-letter (letter)
    (insert "\\'{")
    (insert letter)
    (insert "}"))

(defun around-word (front back)
  (search-backward-regexp "\\(\\ \\)")
  (goto-char (match-end 1))
  (insert front)
  (search-forward-regexp "\\(\\ \\)")
  (goto-char (match-beginning 1))
  (insert back))

(defun insert-rid ()
  (interactive)
  (around-word "\\rid{}{" "}"))
(defun insert-di ()
  (interactive)
  (around-word "\\di{" "}"))

(defun insert-mitem ()
  (interactive)
  (insert "\n\t\\mitem "))

(defun insert-ri ()
  (interactive)
  (around-word "\\ri{" "}"))


(defun acute-it ()
  (interactive)
  (let ((letter (char-before)))
    (save-excursion
      (delete-backward-char 1))
    (acute-letter letter)))

(defun acute () (interactive) (insert ?\u0301))

(defun acute-a () (interactive) (acute-letter "е"))
(defun acute-je () (interactive) (acute-letter "а"))
(defun acute-o () (interactive) (acute-letter "о"))
(defun acute-y () (interactive) (acute-letter "ы"))
(defun acute-u () (interactive) (acute-letter "у"))
(defun acute-i () (interactive) (acute-letter "и"))
(defun acute-ja () (interactive) (acute-letter "я"))
(defun acute-ju () (interactive) (acute-letter "ю"))
(defun acute-e () (interactive) (acute-letter "э"))
(defun acute-jo () (interactive) (acute-letter "ё"))
(defun dansk ()
  (interactive)
  (set-input-method nil))
(defun russisk ()
  (interactive)
  (set-input-method 'cyrillic-yawerty))

(define-minor-mode russian-mode
  "Toggle R mode.
With prefix arg, turn Double mode on iff arg is positive."
  nil "R" russian-mode-map
  :group 'russian-mode
  (if russian-mode
      (progn
        (add-hook 'change-majorussian-mode-hook
                  (lambda () (russian-mode -1))
                  nil t))))



(provide 'russian-mode)
