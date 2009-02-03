(autoload 'sml-mode "sml-mode" "Major mode for editing SML." t)
(autoload 'run-sml "sml-proc" "Run an inferior SML process." t)

(add-to-list (quote auto-mode-alist) (quote ("\\.s\\(ml\\|ig\\)\\'" . sml-mode)))

(defun sml-fill-comment-paragraph (&optional justify)
  "Fill current comment.
If we're not in a comment, just return nil."
  (save-excursion
    (let ((a (comment-beginning))
	  (fill-prefix
	   (save-excursion
	     (next-line)
	     (beginning-of-line)
	     (if (eq (get-text-property (point) 'face)
		       'font-lock-comment-face)
		 (let ((p (point)))
		   (search-forward-regexp "^[* \t]*")
		   (buffer-substring-no-properties p (point)))
	       fill-prefix)))
	  (b (progn
	       (search-forward-regexp comment-end-skip)
	       (comment-enter-backward))))
      (fill-region a b)
      (message (format "hallo%s" (cons a b) )))))
  
(defun sml-fill-paragraph (&optional justify)
  (or (sml-fill-comment-paragraph justify)
      nil))

(add-hook 'sml-mode-hook
	  '(lambda ()
	    (setq fill-paragraph-function 'sml-fill-paragraph)))
