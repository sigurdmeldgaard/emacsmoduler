;; isabelle-system.el --- Interface with Isabelle system
;;
;; Copyright (C) 2000 LFCS Edinburgh, David Aspinall.
;;
;; Author:      David Aspinall <da@dcs.ed.ac.uk>
;; Maintainer:  Proof General maintainer <proofgen@dcs.ed.ac.uk>
;;
;; isabelle-system.el,v 9.3 2008/07/16 22:37:36 da Exp
;;
;; Most of this code is taken from the final version of Isamode.
;; --------------------------------------------------------------
;;

;;; Code:
(eval-when-compile
  (require 'proof-site)			; compile for isar (defpgdefault, etc)
  (require 'span)
  (proof-ready-for-assistant 'isar))

(require 'proof)			; for proof-assistant-symbol, etc.
(require 'proof-syntax)			; for proof-string-match
(require 'cl)				; mapcan


;; The isabelle custom group won't have been defined yet.
(defgroup isabelle nil
  "Customization of user options for Isabelle and Isabelle/Isar Proof General"
  :group 'proof-general)

(defcustom isabelle-web-page
  "http://www.cl.cam.ac.uk/Research/HVG/Isabelle/"
  ;; "http://isabelle.in.tum.de"
  ;; "http://www.dcs.ed.ac.uk/home/isabelle"
  "URL of web page for Isabelle."
  :type 'string
  :group 'isabelle)


;;; ================ Extract Isabelle settings ================

(defcustom isa-isatool-command
  (or (if proof-rsh-command
	  ;; not much hope to locate executable remotely
	  (concat proof-rsh-command " isatool"))
      (getenv "ISATOOL")
      (proof-locate-executable "isatool" nil
			       (list
				;; support default unpack in home dir situation 
				(concat (getenv "HOME") "/Isabelle/bin/")))
      "path_to_isatool_is_unknown")
  "Command to invoke Isabelle tool 'isatool'.
Emacs should be able to find `isatool' if it is on the PATH when
started.  Then several standard locations are attempted.
Otherwise you should set this, using a full path name here for reliable
working."
  :type 'file
  :group 'isabelle)

(defvar isatool-not-found nil
  "Non-nil if user has been prompted for `isatool' already and it wasn't found.")

(defun isa-set-isatool-command ()
  "Make sure isa-isatool-command points to a valid executable.
If it does not, prompt the user for the proper setting.
If it appears we're running on win32 or FSF Emacs, we allow this to
remain unverified.
Returns non-nil if isa-isatool-command is surely an executable
with full path."
  (interactive)
  (unless (or noninteractive
	      proof-rsh-command 
	      isatool-not-found 
	      (file-executable-p isa-isatool-command))
    (setq isa-isatool-command
	  (read-file-name
	   "Please give the full path to `isatool' (RET if you don't have it): "
	   nil nil nil))
    (unless (file-executable-p isa-isatool-command)
      (setq isatool-not-found t)
      (beep)
      (warn "Proof General: isatool command not found; some menus will be incomplete and Isabelle may not run correctly.  Please check your Isabelle installation.")))
  (or proof-rsh-command
      (file-executable-p isa-isatool-command)))

(defun isa-shell-command-to-string (command)
  "Like shell-command-to-string except the last character is stripped."
  (let ((s (shell-command-to-string command)))
    (if (equal (length s) 0) s
      (substring s 0 -1))))

(defun isa-getenv (envvar &optional default)
  "Extract environment variable ENVVAR setting using the `isatool' program.
If the isatool command is not available, try using elisp's getenv
to extract the value from Emacs' environment.
If there is no setting for the variable, DEFAULT will be returned"
  (isa-set-isatool-command)
  (if (or proof-rsh-command
	  (file-executable-p isa-isatool-command))
      (let ((setting (isa-shell-command-to-string
		      (concat isa-isatool-command
			      " getenv -b " envvar))))
	(if (string-equal setting "")
	    default
	  setting))
    (or (getenv envvar) default)))

;;;
;;; ======= Interaction with System using Isabelle tools =======
;;;

(defcustom isabelle-program-name-override nil
  "*Name of executable program to run Isabelle.

You can set customize this in case the automatic settings
mechanism (based on isatool) does not work for you.  One reason
to do this is if you are running Isabelle remotely, or using
windows and avoiding isatool.

A possible value when running under Windows looks like this:

  C:\\sml\\bin\\.run\\run.x86-win32.exe @SMLload=C:\\Isabelle\\

This expects SML/NJ in C:\\sml and Isabelle images in C:\Isabelle.
The logic image name is tagged onto the end."
  :type 'file
  :group 'isabelle)

(defvar isabelle-prog-name nil
  "Set from `isabelle-set-prog-name', has name of logic appended sometimes.")

(defun isa-tool-list-logics ()
  "Generate a list of available object logics."
  (if (isa-set-isatool-command)
      (delete "" (split-string
		  (isa-shell-command-to-string
		   (concat isa-isatool-command " findlogics")) "[ \t]"))))

(defcustom isabelle-logics-available nil
  "*List of logics available to use with Isabelle.
If the `isatool' program is available, this is automatically
generated with the Lisp form `(isa-tool-list-logics)'."
  :type (list 'string)
  :group 'isabelle)

(unless noninteractive
  (setq isabelle-logics-available (isa-tool-list-logics)))

(defcustom isabelle-chosen-logic nil
  "*Choice of logic to use with Isabelle.
If non-nil, added onto the Isabelle command line for invoking Isabelle.

You can set this as a file local variable, using a special comment
at the top of your theory file, like this:

   (* -*- isabelle-chosen-logic: \"ZF\" -*- *)"
  :type (append
	 (list 'choice)
	 (mapcar (lambda (str) (list 'const str)) isabelle-logics-available)
	 (list '(string :tag "Choose another")
	       '(const :tag "Unset (use default)" nil)))
  :group 'isabelle)

(defvar isabelle-chosen-logic-prev nil
  "Value of `isabelle-chosen-logic' on last call of `isabelle-set-prog-name'.")
  
(defun isabelle-hack-local-variables-function ()
  "Hook function for `hack-local-variables-hook'."
  (if (and isabelle-chosen-logic
	   (not (equal isabelle-chosen-logic 
		       isabelle-chosen-logic-prev))
	   (proof-shell-live-buffer))
      (message "Warning: chosen logic %s does not match running Isabelle instance"
	       isabelle-chosen-logic)))

(add-hook 'hack-local-variables-hook 
	  'isabelle-hack-local-variables-function)

(defun isabelle-set-prog-name (&optional filename)
  "Make proper command line for running Isabelle.
This function sets `isabelle-prog-name' and `proof-prog-name'."
  (let*
      ;; The ISABELLE and PROOFGENERAL_LOGIC values (set when run
      ;; under the interface wrapper script) indicate command line
      ;; is set in current Isabelle settings environment.
      ((isabelle (or
		  isabelle-program-name-override  ; override in Emacs
		  (getenv "ISABELLE")	  ; command line override 
		  (isa-getenv "ISABELLE") ; choose to match isatool
		  "isabelle"))		  ; to 
       (isabelle-opts (getenv "ISABELLE_OPTIONS"))
       (opts (concat " -PI"  ;; Proof General + Isar
	      (if proof-shell-unicode " -m PGASCII" "")
	      (if (and isabelle-opts (not (equal isabelle-opts "")))
		  (concat " " isabelle-opts) "")))
       (logic (or isabelle-chosen-logic
		  (getenv "PROOFGENERAL_LOGIC")))
       (logicarg (if (and logic (not (equal logic "")))
		     (concat " " logic) "")))
    (setq isabelle-chosen-logic-prev isabelle-chosen-logic)
    (setq isabelle-prog-name (concat isabelle opts logicarg))
    (setq proof-prog-name isabelle-prog-name)))

(defun isabelle-choose-logic (logic)
  "Adjust isabelle-prog-name and proof-prog-name for running LOGIC."
  (interactive
   (list (completing-read
	  "Use logic: "
	  (mapcar 'list (cons "Default"
			      isabelle-logics-available)))))
  (if (proof-shell-live-buffer)
      (error "Can't change logic while Isabelle is running, please exit process first!"))
  (customize-set-variable 'isabelle-chosen-logic
			  (unless (string-equal logic "Default") logic))
  (isabelle-set-prog-name)
  ;; Settings are potentially different between logics, and
  ;; so are Isar keywords.  Set these to nil so they get
  ;; automatically re-initialised.
  ;; FIXME: Isar keywords change not handled yet.
  (setq proof-assistant-settings nil)
  (setq proof-menu-settings nil))

(defun isa-view-doc (docname)
  "View Isabelle document DOCNAME, using Isabelle tools."
  (if (isa-set-isatool-command)
      (apply 'start-process
	     "isa-view-doc" nil
	     (append (split-string
		      isa-isatool-command) 
		     (list "doc" docname)))))

(defun isa-tool-list-docs ()
  "Generate a list of documentation files available, with descriptions.
This function returns a list of lists of the form
 ((DOCNAME DESCRIPTION) ....)
of Isabelle document names and descriptions.  When DOCNAME is
passed to isa-tool-doc-command, DOCNAME will be viewed."
  (if (isa-set-isatool-command)
      (let ((docs (isa-shell-command-to-string
		   (concat isa-isatool-command " doc"))))
	(unless (string-equal docs "")
	  (mapcan
	   (function (lambda (docdes)
		       (if (proof-string-match "^[ \t]+\\(\\S-+\\)[ \t]+" docdes)
			   (list (list
				  (substring docdes (match-beginning 1) (match-end 1))
				  (substring docdes (match-end 0)))))))
	   (split-string docs "\n"))))))

; TODO: use this, add dialog to query user to save if DB is r/w?  Seems annoying.
; (defun isa-quit (save)
;   "Quit / save the Isabelle session.
; Called with one argument: t to save database, nil otherwise."
;   (interactive "p")
;   (if (not save)
;       (isa-insert-ret "quit();"))
;   (comint-send-eof))

(defconst isabelle-verbatim-regexp "\\`\^VERBATIM: \\(\\(.\\|\n\\)*\\)\\'"
  "Regexp matching internal marker for verbatim command output.")

(defun isabelle-verbatim (str)
  "Mark internal command STR for verbatim output."
  (concat "\^VERBATIM: " str))


;;; ==========  Utility functions ==========

(defcustom isabelle-refresh-logics t
  "*Whether to refresh the list of logics during an interactive session.
If non-nil, then `isatool findlogics' will be used to regenerate
the `isabelle-logics-available' setting.  If this tool does not work
for you, you should disable this behaviour."
  :type 'boolean
  :group 'isabelle)

(defvar isabelle-docs-menu 
  (let ((vc '(lambda (docdes)
	       (vector (car (cdr docdes))
		       (list 'isa-view-doc (car docdes)) t))))
    (list (cons "Isabelle documentation" (mapcar vc (isa-tool-list-docs)))))
  "Isabelle documentation menu.  Constructed when PG is loaded.")

(defvar isabelle-logics-menu-entries nil
  "Menu of logics available.")

(defun isabelle-logics-menu-calculate ()
  (setq isabelle-logics-menu-entries
	(cons "Logics"
	      (append
	       '(["Default"
		  (isabelle-choose-logic nil)
		  :active (not (proof-shell-live-buffer))
		  :style radio
		  :selected (not isabelle-chosen-logic)])
	       (mapcar (lambda (l)
			 (vector l (list 'isabelle-choose-logic l)
				 :active '(not (proof-shell-live-buffer))
				 :style 'radio
				 :selected (list 'equal 'isabelle-chosen-logic l)))
		       isabelle-logics-available)))))

(unless noninteractive
  (isabelle-logics-menu-calculate))

(defvar isabelle-time-to-refresh-logics t
  "Non-nil if we should refresh the logics list.")


(defun isabelle-logics-menu-refresh ()
  "Refresh isabelle-logics-menu-entries, returning new entries."
  (interactive)
  (if (and isabelle-refresh-logics
	   (or isabelle-time-to-refresh-logics (interactive-p)))
      (progn
	(setq isabelle-logics-available (isa-tool-list-logics))
	(isabelle-logics-menu-calculate)
	(if (not (featurep 'xemacs))
	    ;; update the menu manually
	    (easy-menu-add-item proof-assistant-menu nil
				isabelle-logics-menu-entries))
	(setq isabelle-time-to-refresh-logics nil) ;; just done it, don't repeat!
	(run-with-timer 2 nil ;; short delay to avoid doing this too often
			(lambda () (setq isabelle-time-to-refresh-logics t))))))

;; Function for XEmacs only
(defun isabelle-logics-menu-filter (&optional ignored)
  (isabelle-logics-menu-refresh)
  (cdr isabelle-logics-menu-entries))

;; Functions for GNU Emacs only to update logics menu
(if (not (featurep 'xemacs))
(defun isabelle-menu-bar-update-logics ()
  "Update logics menu."
  (and (current-local-map)
       (keymapp (lookup-key (current-local-map)
 			    (vector 'menu-bar (intern proof-assistant))))
       (isabelle-logics-menu-refresh))))

(if (not (featurep 'xemacs))
    (add-hook 'menu-bar-update-hook 'isabelle-menu-bar-update-logics))


(defvar isabelle-logics-menu
   (if (featurep 'xemacs)
       (cons (car isabelle-logics-menu-entries)
	     (list :filter 'isabelle-logics-menu-filter)) ;; generates menu on click
     isabelle-logics-menu-entries)
  "Isabelle logics menu.  Calculated when Proof General is loaded.")

;; Added in PG 3.4: load isar-keywords file.
;; This roughly follows the method given in the interface script.
;; It could be used to add an elisp command at the bottom of
;; a theory file, if we sorted out the load order a bit, or
;; added a facility to reconfigure.
;; TODO: also add something to spill out a keywords file?
(defun isabelle-load-isar-keywords (&optional kw)
  (interactive "sLoad isar keywords: ")
  (let ((userhome  (isa-getenv "ISABELLE_HOME_USER"))
	(isahome   (isa-getenv "ISABELLE_HOME"))
	(isarkwel  "%s/etc/isar-keywords-%s.el")
	(isarel    "%s/etc/isar-keywords.el")
	(ifrdble   (lambda (f) (if (file-readable-p f) f))))
    (load-file
     (or
      (and kw (funcall ifrdble (format isarkwel userhome kw)))
      (and kw (funcall ifrdble (format isarkwel isahome kw)))
      (funcall ifrdble (format isarel userhome))
      (funcall ifrdble (format isarel isahome))
      (locate-library "isar-keywords")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Generic Isabelle menu for Isabelle and Isabelle/Isar
;;

(defpgdefault menu-entries
  isabelle-logics-menu)

(defpgdefault help-menu-entries isabelle-docs-menu)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; X-Symbol language configuration, and adding to completion table
;;

(eval-after-load "x-symbol-isar"
  ;; Add x-symbol tokens to isa-completion-table and rebuild
  ;; internal completion table if completion is already active
  '(progn
     (defpgdefault completion-table
       (append isar-completion-table
	       (mapcar (lambda (xsym) (nth 2 xsym))
		       x-symbol-isar-table)))
     (setq proof-xsym-font-lock-keywords
	   x-symbol-isar-font-lock-keywords)
     (if (featurep 'completion)
	 (proof-add-completions))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Subterm markup -- faking it
;;

(defun isabelle-convert-idmarkup-to-subterm ()
  "Convert identifier markup to subterm markup.
This is a hook setting for `pg-after-fontify-output-hook' to
enable identifiers to be highlighted.  (To disable that behaviour,
the function `pg-remove-specials' can be used instead)."
  ;; NB: the order of doing this is crucial: it must happen after
  ;; fontifying (since replaces chars used for fontifying), but before
  ;; X-Sym decoding (since some chars used for fontifying may clash
  ;; with X-Sym character codes: luckily those codes don't seem to
  ;; cause problems for subterm markup).
  ;; Future version of this should use PGML output in Isabelle2002.
  (goto-char (point-min))
  (while (proof-re-search-forward "[\351-\357]" nil t)
    (replace-match "\372\200\373" nil t))
  (goto-char (point-min))
  (while (proof-re-search-forward "\350" nil t)
    (replace-match "\374" nil t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Context-senstive in-span menu additions
;;

(defun isabelle-create-span-menu (span idiom name)
  (if (string-equal idiom "proof")
      (let ((thm (span-property span 'name)))
	(list (vector
	       "Visualise dependencies"
	       `(proof-shell-invisible-command
		 ,(format "thm_deps %s;" thm))
	       (not (string-equal thm proof-unnamed-theorem-name)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; XML as an SML string: add escapes for quotes
;;

(defun isabelle-xml-sml-escapes (xmlstring)
  (replace-regexp-in-string "\"" "\\\"" xmlstring t t))

(defun isabelle-process-pgip (xmlstring)
  "Return an Isabelle or Isabelle/Isar command to process PGIP in XMLSTRING."
  (format "ProofGeneral.process_pgip \"%s\";"
	  (isabelle-xml-sml-escapes xmlstring)))


(provide 'isabelle-system)
;;; isabelle-system.el ends here
