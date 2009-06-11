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
