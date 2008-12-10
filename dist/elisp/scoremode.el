
(define-derived-mode score-mode fundamental-mode "SCORES"
  (define-key score-mode-map " " 'insert-number)
  (define-key score-mode-map "a" 'insert-letter-a)
  (define-key score-mode-map "b" 'insert-letter-b)
  (define-key score-mode-map "c" 'insert-letter-c)
  (define-key score-mode-map "d" 'insert-letter-d)
  (define-key score-mode-map "e" 'insert-letter-e)
  (define-key score-mode-map "f" 'insert-letter-f)
  (define-key score-mode-map "g" 'insert-letter-g)
  )

(defun insert-letter-a ()
  (interactive)
  (insert-letter ?a))
(defun insert-letter-b ()
  (interactive)
  (insert-letter ?b))
(defun insert-letter-c ()
  (interactive)
  (insert-letter ?c))
(defun insert-letter-d ()
  (interactive)
  (insert-letter ?d))
(defun insert-letter-e ()
  (interactive)
  (insert-letter ?e))

(defun insert-letter (l)
  (when (>= (char-before (point)) l)
    (insert-number))
  (insert (format "%c" l)))


(defun insert-number ()
  (interactive)
  (let ((pos (point))
        (letters '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
        nr between match-start
        match-end do-anything-p)
    (save-excursion
      (while (not (member (char-before) letters))
        (backward-char))
      (setq match-end (point))
      (while (member (char-before) letters)
        (backward-char))
            (setq match-start (point))
            (message (buffer-substring match-start
                                       match-end))
            (setq nr (string-to-number (buffer-substring match-start
                                                         match-end)))
            (setq between (buffer-substring match-end (point))))
    (unless (member (char-after match-end) '(?a ?b ?c ? ?d ?e ?f ?g))
            (insert "-"))
    (insert (format "%d" (+ 1 nr)))))

(provide 'score-mode)
