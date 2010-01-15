;(require 'cc-mode)
;(require 'css-mode)
;(require 'javascript-mode)

(defun mmm-detect-style-region ()
  (let ((case-fold-search t)
	b e m s)
    (and (re-search-forward "<style\\(.*\\)>" nil t)
	 (setq s (downcase (match-string 1))
	       b (match-end 0)
	       m (if (string-match "css" s)
		     'css-mode
		   'css-mode))
	 (search-forward "</style>" nil t)
	 (setq e (match-beginning 0))
	 (cons m (cons b e))))
)

(defun mmm-detect-script-region ()
  (let ((case-fold-search t)
	b e m s)
    (and (re-search-forward "<script\\(.*\\)>" nil t)
	 (setq s (downcase (match-string 1))
	       b (match-end 0)
	       m (if (string-match "javascript" s)
		     'javascript-mode
		   'java-mode))
	 (search-forward "</script>" nil t)
	 (setq e (match-beginning 0))
	 (cons m (cons b e))))
)

;;
;; detect JSP scriptlet regions
;;
(defun mmm-detect-jsp-xml-scriptlet-region ()
  (let ((case-fold-search t) b e)
    (and 
     (re-search-forward "<jsp:scriptlet>" nil t)
     (setq b (match-end 0))
     (search-forward "</jsp:scriptlet>" nil t)
     (setq e (match-beginning 0))
     (cons 'java-mode (cons b e))))
)

;;
;; detect JSP scriptlet tags (as well as other JSP tags)
;;
(defun mmm-detect-jsp-scriptlet-region ()
  (let ((case-fold-search t) b e)
    (and 
     (re-search-forward "<%" nil t)
     (setq b (match-end 0))
     (search-forward "%>" nil t)
     (setq e (match-beginning 0))
     (cons 'java-mode (cons b e))))
)

;;
;; initalize all mmm activators
;;
(defun mmm-html-init()
  (interactive)
  (mmm-activator 'mmm-detect-script-region)
  (mmm-activator 'mmm-detect-style-region)
  (mmm-activator 'mmm-detect-jsp-scriptlet-region)
  (mmm-activator 'mmm-detect-jsp-xml-scriptlet-region)
)

;;
;; initalize and re-fontify
;;
(defun mmm-html-reload()
  (interactive)
  (mmm-html-init)
  (font-lock-fontify-buffer))

;;
;; add hooks
;;
(add-hook 'html-mode-hook
	  (function (lambda()
		      (setq line-number-mode t)
		      (setq column-number-mode t)
		      (mmm-html-init)
)))

;;
;; add modes
;;
(setq auto-mode-alist
      (append '(("\\.js$" . javascript-mode)
		("\\.css$" . css-mode)
		("\\.jsp$" . html-mode) ; no jsp-mode just yet =(
		("\\.jspi$" . html-mode) ; no jsp-mode just yet =(
		("\\.htm[l]?$" . html-mode)) auto-mode-alist))

(provide 'mmm-html)
;;;EOF