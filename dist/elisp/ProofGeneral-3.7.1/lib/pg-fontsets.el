;;; pg-fontsets.el --- Define fontsets useful for Proof General
;;
;; Copyright (C) 2008 David Aspinall / LFCS Edinburgh
;; Author:    David Aspinall <David.Aspinall@ed.ac.uk>
;; License:   GPL (GNU GENERAL PUBLIC LICENSE)
;;
;; pg-fontsets.el,v 1.2 2008/02/17 12:34:11 da Exp
;;
;;; Commentary:
;; 
;; Define some fontsets to try to select fonts that display many symbols.
;;
;; Select one of these fontsets via the menu Options -> Set Font/Fontset
;; or, with M-x set-default-font 
;;
;; Recommended & free fonts to install on your system are:
;;
;;  DejaVu LGC (Sans and Sans Mono).  See http://dejavu.sourceforge.net
;;    - missing Uplus, smile, frown, join

;;; Code:

(defcustom pg-fontsets-default-fontset nil
  "*Name of default fontset to use with Proof General."
  :type 'string
  :group 'proof-user-options)

(defun pg-fontsets-make-fontsetsizes (basefont)
  (dolist (size '(10 12 14 18 22))
    (add-to-list 'pg-fontsets-names
	(create-fontset-from-fontset-spec
	 (replace-regexp-in-string 
	  "%T" (car (split-string basefont))
	 (replace-regexp-in-string 
	  "%S" (int-to-string size)
	  (replace-regexp-in-string 
	   "%F" basefont
"-*-%F-medium-r-normal--%S-*-*-*-*-*-fontset-PG%T,
gnu-unifont:-*-%F-medium-r-normal--%S-*-*-*-*-*-iso10646-1"
;ascii:-*-%F-medium-r-normal--%S-*-*-*-*-*-mac-roman,
;latin-iso8859-1:-*-%F-medium-r-normal--%S-*-*-*-*-*-mac-roman,
;mule-unicode-0100-24ff:-*-%F-medium-r-normal--%S--*-*-*-*-*-iso10646-1,
;mule-unicode-2500-33ff:-*-%F-medium-r-normal--%S--*-*-*-*-*-iso10646-1,
;mule-unicode-e000-ffff:-*-%F-medium-r-normal--%S--*-*-*-*-*-iso10646-1"
)))))))

(defconst pg-fontsets-base-fonts 
  '("dejavu lgc sans mono"
    "liberation mono" 
    "stixregular"))

(defun pg-fontsets-make-fontsets ()
  (setq pg-fontsets-names nil)
  (mapcar 'pg-fontsets-make-fontsetsizes 
	  pg-fontsets-base-fonts))
;    (custom-initialize-default 'pg-fontsets-default-fontset 
;			       (nth 2 pg-fontsets-names))
;  (setq pg-fontsets-default-fontset (nth 2 pg-fontsets-names))
;  (set-default-font pg-fontsets-default-fontset))

(pg-fontsets-make-fontsets)



;;; pg-fontsets.el ends here
