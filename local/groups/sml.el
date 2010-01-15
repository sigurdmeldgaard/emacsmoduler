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
