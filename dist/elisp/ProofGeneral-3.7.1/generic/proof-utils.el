;; proof-utils.el --- Proof General utility functions and macros
;;
;; Copyright (C) 1998-2002 LFCS Edinburgh.
;; Author:      David Aspinall <David.Aspinall@ed.ac.uk> and others
;; License:     GPL (GNU GENERAL PUBLIC LICENSE)
;;
;; proof-utils.el,v 9.2 2008/01/31 22:07:52 da Exp
;;
;;; Commentary:
;; 
;; Loading note: this file is required immediately from proof.el, so
;; no autoloads cookies are added here. 
;;
;; Compilation note: see etc/development-tips.txt
;;

(require 'proof-site)			; basic vars


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Give Emacs version mismatch error here.
;;
;; This file is loaded early, and may be first compiled file
;; loaded if proof-site.el is loaded instead of proof-site.elc.
;;
(eval-and-compile 
  (defun pg-emacs-version-cookie ()
    (format (if (string-match "XEmacs" emacs-version) 
		;; (featurep 'xemacs) gets optimised!!
		"XEmacs %d.%d" "GNU Emacs %d.%d")
	    emacs-major-version emacs-minor-version))
  
  (defconst pg-compiled-for 
    (eval-when-compile (pg-emacs-version-cookie))
    "Version of Emacs we're compiled for (or running on, if interpreted)."))

(if (or (not (boundp 'emacs-major-version))
	(< emacs-major-version 21))
    (error "Proof General is not compatible with Emacs %s" emacs-version))

(unless (equal pg-compiled-for (pg-emacs-version-cookie))
  (error 
   "Proof General was compiled for %s but running on %s: please run \"make clean; make\""
   pg-compiled-for (pg-emacs-version-cookie)))

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  


(require 'proof-compat)		        ; 
(require 'proof-config)			; config vars
(require 'bufhist)			; bufhist 
(require 'proof-syntax)			; syntax utils
(require 'proof-autoloads)		; interface fns


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Handy macros

;;; Code:
(defmacro deflocal (var value &optional docstring)
  "Define a buffer local variable VAR with default value VALUE."
 `(progn
    (defvar ,var nil ,docstring)
    (make-variable-buffer-local (quote ,var))
    (setq-default ,var ,value)))

(defmacro proof-with-current-buffer-if-exists (buf &rest body)
  "As with-current-buffer if BUF exists and is live, otherwise nothing."
  `(if (buffer-live-p ,buf)
       (with-current-buffer ,buf
	 ,@body)))

(deflocal proof-buffer-type nil
  "Symbol for the type of this buffer: 'script, 'shell, 'goals, or 'response.")

;; Slightly specialized version of above.  This is used in commands
;; which work from different PG buffers (goals, response), typically
;; bound to toolbar commands.
(defmacro proof-with-script-buffer (&rest body)
  "Execute BODY in some script buffer: current buf or otherwise proof-script-buffer.
Return nil if not a script buffer or if no active scripting buffer."
  `(cond
    ((eq proof-buffer-type 'script)
     (progn
       ,@body))
    ((buffer-live-p proof-script-buffer)
     (with-current-buffer proof-script-buffer
       ,@body))))
      
(defmacro proof-map-buffers (buflist &rest body)
  "Do BODY on each buffer in BUFLIST, if it exists."
  `(dolist (buf ,buflist)
     (proof-with-current-buffer-if-exists buf ,@body)))

(defmacro proof-sym (string)
  "Return symbol for current proof assistant using STRING."
 `(intern (concat (symbol-name proof-assistant-symbol) "-" ,string)))


(defsubst proof-try-require (symbol)
  "Try requiring SYMBOL.  No error if the file for SYMBOL isn't found."
  (condition-case ()
      (require symbol)
    (file-error nil))
  (featurep symbol))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Simplified version of save-some-buffers, with useful arg
;;

(defun proof-save-some-buffers (buffers)
  ;; code based on extract from files.el in XEmacs 21.4.14
  (map-y-or-n-p
   (lambda (buffer)
     (if
	 (and (buffer-modified-p buffer)
	      (not (buffer-base-buffer buffer))
	      (buffer-file-name buffer))
	 ;; we deliberately don't switch to show the buffer;
	 ;; let's assume user can see it or knows what's in it.
	 (format "Save file %s? "
		 (buffer-file-name buffer))))
   (lambda (buffer)
     (set-buffer buffer)
     (condition-case ()
	 (save-buffer)
       (error nil)))
   buffers))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Macros for defining per-assistant customization settings.
;;
;; This new mechanism is an improved way to handle per-assistant
;; settings.  Instead of declaring a variable
;; "proof-assistant-web-page" and duplicating it in the prover
;; specific code to make the generic setting, we automatically declare
;; "isabelle-web-page", "coq-web-page", etc, using these macros.
;;
;; The advantage of this is that people's save settings will work
;; properly, and that it will become more possible to use more than
;; one instance of PG at a time.  The disadvantage is that it is
;; slightly more complicated, and less "object-oriented" than the
;; previous approach.  It is also a big change to move all settings.
;;
;; NB: this mechanism is work in progress in 3.2.  It will
;; be expanded, although we may leave most low-level
;; settings to use the current mechanism.
;;
;; Notes:
;;
;; Two mechanisms for accessing generic vars:
;;
;; (proof-ass name)   or (proof-assistant-name)
;;

(defmacro proof-ass-sym (sym)
  "Return the symbol for SYM for the current prover.  SYM not evaluated.
This macro should only be called once a specific prover is known."
  `(intern (concat (symbol-name proof-assistant-symbol) "-"
		   (symbol-name ',sym))))

(defmacro proof-ass-symv (sym)
  "Return the symbol for SYM for the current prover.  SYM evaluated.
This macro should only be invoked once a specific prover is engaged."
  `(intern (concat (symbol-name proof-assistant-symbol) "-"
		   (symbol-name ,sym))))

(defmacro proof-ass (sym)
  "Return the value for SYM for the current prover.
This macro should only be invoked once a specific prover is engaged."
  `(symbol-value (intern (concat (symbol-name proof-assistant-symbol) "-"
				 (symbol-name ',sym)))))

(defun proof-defpgcustom-fn (sym args)
  "Define a new customization variable <PA>-sym for current proof assistant.
Helper for macro `defpgcustom'."
  (let ((specific-var (proof-ass-symv sym))
	 (generic-var  (intern (concat "proof-assistant-" (symbol-name sym)))))
    (eval
     `(defcustom ,specific-var
       ,@args
       ;; We could grab :group from @args and prefix it.
       :group ,(quote proof-assistant-internals-cusgrp)))
    ;; For functions, we could simply use defalias.  Unfortunately there
    ;; is nothing similar for values, so we define a new set/get function.
    (eval
     `(defun ,generic-var (&optional newval)
	,(concat "Set or get value of " (symbol-name sym) 
		 " for current proof assistant.
If NEWVAL is present, set the variable, otherwise return its current value.")
	(if newval
	    (setq ,specific-var newval)
	  ,specific-var)))))

(defun undefpgcustom (sym)
  (let ((specific-var (proof-ass-symv sym))
	(generic-var  (intern (concat "proof-assistant-" (symbol-name sym)))))
    (pg-custom-undeclare-variable specific-var)
    (fmakunbound generic-var)))

(defmacro defpgcustom (sym &rest args)
  "Define a new customization variable <PA>-SYM for the current proof assistant.
The function proof-assistant-<SYM> is also defined, which can be used in the
generic portion of Proof General to set and retrieve the value for the current p.a.
Arguments as for `defcustom', which see.

Usage: (defpgcustom SYM &rest ARGS)."
  `(proof-defpgcustom-fn (quote ,sym) (quote ,args)))



(defun proof-defpgdefault-fn (sym value)
  "Helper for `defpgdefault', which see.  SYM and VALUE are evaluated."
  ;; NB: we need this because nothing in customize library seems to do
  ;; the right thing.
  (let ((symbol  (proof-ass-symv sym)))
    (set-default symbol
		 (cond
		  ;; Use saved value if it's set
		  ((get symbol 'saved-value)
		   (car (get symbol 'saved-value)))
		  ;; Otherwise override old default with new one
		  (t
		   value)))))

(defmacro defpgdefault (sym value)
  "Set default for the proof assistant specific variable <PA>-SYM to VALUE.
This should be used in prover-specific code to alter the default values
for prover specific settings.

Usage: (defpgdefault SYM VALUE)"
    `(proof-defpgdefault-fn (quote ,sym) ,value))

;;
;; Make a function named for the current proof assistant.
;;
(defmacro defpgfun (name arglist &rest args)
  "Define function <PA>-SYM as for defun."
  `(defun ,(proof-ass-symv name) ,arglist
     ,@args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Evaluation once proof assistant is known
;; 

(defmacro proof-eval-when-ready-for-assistant (&rest body)
  "Evaluate BODY once the proof assistant is determined (possibly now)."
  `(if (and (boundp 'proof-assistant-symbol) proof-assistant-symbol)
       (progn ,@body)
     (add-hook 'proof-ready-for-assistant-hook (lambda () ,@body))))
    



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Buffers and filenames

(defun proof-file-truename (filename)
  "Returns the true name of the file FILENAME or nil if file non-existent."
  (and filename (file-exists-p filename) (file-truename filename)))

(defun proof-file-to-buffer (filename)
  "Find a buffer visiting file FILENAME, or nil if there isn't one."
  (let* ((buffers (buffer-list))
	 (pos
	  (position (file-truename filename)
		    (mapcar 'proof-file-truename
			    (mapcar 'buffer-file-name
				    buffers))
		    :test 'equal)))
    (and pos (nth pos buffers))))

(defun proof-files-to-buffers (filenames)
  "Converts a list of FILENAMES into a list of BUFFERS."
  (if (null filenames) nil
    (let* ((buffer (proof-file-to-buffer (car filenames)))
	  (rest (proof-files-to-buffers (cdr filenames))))
      (if buffer (cons buffer rest) rest))))

(defun proof-buffers-in-mode (mode &optional buflist)
  "Return a list of the buffers in the buffer list in major-mode MODE.
Restrict to BUFLIST if it's set."
  (let ((bufs-left (or buflist (buffer-list)))
	bufs-got)
    (dolist (buf bufs-left bufs-got)
      (if (with-current-buffer buf (eq mode major-mode))
	  (setq bufs-got (cons buf bufs-got))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Associated buffers
;;

(defun pg-save-from-death ()
  "Prevent this associated buffer from being killed: merely erase it.
A hook function for `kill-buffer-hook'.
This is a fairly crude and not-entirely-robust way to prevent the
user accidently killing an associated buffer."
  (if (and (proof-shell-live-buffer) proof-buffer-type)
      (progn
	(let ((bufname (buffer-name)))
	  (bufhist-erase-buffer)
	  (set-buffer-modified-p nil)
	  (bury-buffer)
	  (error
	   "Warning: buffer %s not killed; still associated with prover process."
	   bufname)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Key functions

(defun proof-define-keys (map kbl)
  "Adds keybindings KBL in MAP.
The argument KBL is a list of tuples (k . f) where `k' is a keybinding
\(vector) and `f' the designated function."
  (mapcar
   (lambda (kbl)
     (let ((k (car kbl)) (f (cdr kbl)))
         (define-key map k f)))
   kbl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Managing font-lock
;;

;; Notes:
;;
;; * Various mechanisms for setting defaults, different between
;;   Emacsen.  Old method(?) was to set "blah-mode-font-lock-keywords"
;;   and copy it into "font-lock-keywords" to engage font-lock.
;;   Present method for XEmacs is to put a 'font-lock-defaults
;;   property on the major-mode symbol, or use font-lock-defaults
;;   buffer local variable.  We use the latter.
;;
;; * Buffers which are output-only are *not* kept in special minor
;;   modes font-lock-mode (or x-symbol-mode).  In case the user
;;   doesn't want fontification we have a user option,
;;   proof-output-fontify-enable.

(deflocal proof-font-lock-keywords nil
  "Value of font-lock-keywords in this buffer.
We set `font-lock-defaults' to '(proof-font-lock-keywords) for
compatibility with X-Symbol, which may hack `font-lock-keywords'
with extra patterns (in non-mule mode).")

(defun proof-font-lock-configure-defaults (autofontify)
  "Set defaults for font-lock based on current font-lock-keywords.
This is a delicate operation, because we only want to use font-lock-mode
in some buffers, so we have to tread carefully around the font-lock
code to avoid it turning itself on in the buffers where that actually
*breaks* fontification.

AUTOFONTIFY must be nil for buffers where we may want to really use
font-lock-mode; in those buffers, we enable syntactic fontification also."
  ;;
  ;; At the moment, the specific assistant code hacks
  ;; font-lock-keywords.  Here we use that value to hack
  ;; font-lock-defaults, which is used by font-lock to set
  ;; font-lock-keywords from again!!  Yuk.
  ;;
  ;; Previously, 'font-lock-keywords was used directly as a setting
  ;; for the defaults.  This has a bad interaction with x-symbol which
  ;; edits font-lock-keywords and loses the setting.  So we make a
  ;; copy of it in a new local variable, proof-font-lock-keywords.
  ;;
  (setq proof-font-lock-keywords font-lock-keywords)

    ;; Setting font-lock-defaults explicitly is required by FSF Emacs
  ;; 20.4's version of font-lock in any case.

  (if autofontify
      (progn
	(make-local-variable 'font-lock-defaults) ; needed??
	(setq font-lock-defaults '(proof-font-lock-keywords))
	;; 12.1.99: For XEmacs, we must also set the mode property.
	;; This is needed for buffers which are put into font-lock-mode
	;; (rather than fontified by hand).
	(put major-mode 'font-lock-defaults font-lock-defaults))
    ;; 11.12.01: Emacs 21 is very eager about turning on font
    ;; lock and has hooks all over the place to do it.  To make
    ;; sure it doesn't happen we have to eradicate all sense of
    ;; having any fontification ability.
    (proof-font-lock-clear-font-lock-vars)
    ;; In fact, this still leaves font-lock switched on! But
    ;; hopefully in a useless way.  XEmacs has better control
    ;; over which modes not to enable it for (although annoying
    ;; that it's a custom setting)
    (if (featurep 'xemacs)
	(setq font-lock-mode-disable-list
	      (cons major-mode font-lock-mode-disable-list)))))

(defun proof-font-lock-clear-font-lock-vars ()
  (kill-local-variable 'font-lock-defaults)
  (kill-local-variable 'font-lock-keywords)
  (setq font-lock-keywords nil)
  (put major-mode 'font-lock-defaults nil))

(defun proof-font-lock-set-font-lock-vars ()
  (setq font-lock-defaults '(proof-font-lock-keywords))
  (setq font-lock-keywords proof-font-lock-keywords))

(defun proof-fontify-region (start end &optional keepspecials)
  "Fontify and decode X-Symbols in region START...END.
Fontifies (keywords only) according to the buffer's font lock defaults.
Uses `proof-x-symbol-decode-region' to decode tokens
if X-Symbol is enabled.
Uses `unicode-tokens-tokens-to-unicode' to decode tokens
if unicode symbols are enabled.

If `pg-use-specials-for-fontify' is set, remove characters
with top bit set after fontifying so they don't spoil cut and paste,
unless KEEPSPECIALS is set to override this.

Returns new END value."
  ;; We fontify first because X-sym decoding changes char positions.
  ;; It's okay because x-symbol-decode works even without font lock.
  ;; Possible disadvantage is that font lock patterns can't refer
  ;; to X-Symbol characters.
  ;; NB: perhaps we can narrow within the whole function, but there
  ;; was an earlier problem with doing that.
  (when proof-output-fontify-enable
    (let ((normal-font-lock-verbose font-lock-verbose))
      ;; Temporarily set font-lock defaults
      (proof-font-lock-set-font-lock-vars)
      (setq font-lock-verbose nil)	; prevent display glitches in XEmacs

      ;; Yukky hacks to immorally interface with font-lock
      (unless (featurep 'xemacs)
	(font-lock-set-defaults))
      (let ((font-lock-keywords proof-font-lock-keywords))
	(if (and (featurep 'xemacs)
		 (>= emacs-major-version 21)
		 (>= emacs-minor-version 4)
		 (not font-lock-cache-position))
	    (progn
	      (setq font-lock-cache-position (make-marker))
	      (set-marker font-lock-cache-position 0)))
	
	(save-restriction
	  (narrow-to-region start end)
	  (run-hooks 'pg-before-fontify-output-hook)
	  (setq end (point-max)))
	;; da: protect against "Nesting too deep for parser" in bad XEmacs
	(condition-case err
	    (font-lock-default-fontify-region start end nil)
	  (t (proof-debug 
	      "Caught condition %s in `font-lock-default-fontify-region'"
	      (car err)))))

      (save-restriction
	(narrow-to-region start end)
	(run-hooks 'pg-after-fontify-output-hook)
	(setq end (point-max)))

      (prog1 ;; prog1 because we return new END value.
	  (cond
	   ((proof-ass x-symbol-enable)
	    (proof-x-symbol-decode-region start end))
	   ((proof-ass unicode-tokens-enable)
	    (unicode-tokens-tokens-to-unicode start end)))
	(proof-font-lock-clear-font-lock-vars)
	(setq font-lock-verbose normal-font-lock-verbose)))))

(defun pg-remove-specials (&optional start end)
  "Remove special characters in region.  Default to whole buffer.
Leave point at END."
  (save-restriction
    (if (and start end)
	(narrow-to-region start end))
    (goto-char (or start (point-min)))
    (while (re-search-forward pg-special-char-regexp end t)
      (replace-match ""))))

(defun pg-remove-specials-in-string (string)
  (proof-replace-regexp-in-string pg-special-char-regexp "" string))

  

;; FIXME todo: add toggle for fontify region which turns it on/off

(defun proof-fontify-buffer ()
  "Call proof-fontify-region on whole buffer."
  (proof-fontify-region (point-min) (point-max)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Messaging and display functions
;;


(defun proof-warn-if-unset (tag sym)
  "Give a warning (with TAG) if symbol SYM is unbound or nil."
  (unless (and (boundp sym) (symbol-value sym))
    (warn "Proof General %s: %s is unset."  tag (symbol-name sym))))

(defun proof-get-window-for-buffer (buffer)
  "Find a window for BUFFER, display it there, return the window.
NB: may change the selected window."
  ;; IF there *isn't* a visible window showing buffer...
  (unless (get-buffer-window buffer 0)
    ;; THEN find somewhere nice to display it
	  (if (and
	     ;; If we're in two-window mode and already displaying a
	     ;; script/response/goals, try to just switch the buffer
	     ;; instead of calling display-buffer which alters sizes.
	     ;; Gives user some stability on display.
	     (not proof-three-window-enable)
	     (> (count-windows) 1)
	     ;; was: (not (eq (next-window) (selected-window)))
	     (memq (window-buffer (next-window nil 'ignoreminibuf))
		   ;; NB: 3.5: added rest of assoc'd buffers here
		   (cons proof-script-buffer (proof-associated-buffers))))
	    (if (eq (selected-window) (minibuffer-window))
		;; 17.8.01: avoid switching the minibuffer's contents
		;; -- terrrible confusion -- use next-window after
		;; script buffer instead.
		;; (another hack which is mostly right)
		(set-window-buffer
		 (next-window
		  (car-safe (get-buffer-window-list proof-script-buffer))
		  'ignoreminibuf) buffer)
	      (if (eq (window-buffer (next-window nil 'ignoreminibuf))
		      proof-script-buffer)
		  ;; switch this window
		  (set-window-buffer (selected-window) buffer)
		;; switch other window
		(set-window-buffer (next-window nil 'ignoreminibuf) buffer)))
	    ;; o/w: call display buffer to configure windows nicely.
	    ;; PC: Experimental this was simply (display-buffer buffer) but I am
	    ;; experimenting the following policy: when in three windows
	    ;; mode, always make new windo on the right pane, that is:
	    ;; always split one of the associated buffers windows
	    ;; this is not perfect, let's see if people like it
	    (let ((associated-windows (proof-associated-windows)))
	      (if (not (and proof-three-window-enable associated-windows))
		  (display-buffer buffer)
		(select-window (car associated-windows)) ; take on assoc. win
		(split-window-vertically)
		(set-window-dedicated-p (selected-window) nil)
		(switch-to-buffer buffer)
		(set-window-dedicated-p (selected-window) t)
		))
	    ))
  ;; Return the window, hopefully the one we first thought of.
  (get-buffer-window buffer 0))

(defun proof-display-and-keep-buffer (buffer &optional pos)
  "Display BUFFER and make window according to display mode.
If optional POS is present, will set point to POS.
Otherwise move point to the end of the buffer.
Ensure that point is visible in window."
  (save-excursion
    (save-selected-window
      (let ((window (proof-get-window-for-buffer buffer)))
	(if (window-live-p window) ;; [fails sometimes?]
	    (progn
	      ;; Set the size and point position.
	      (if proof-three-window-enable
		  (set-window-dedicated-p window proof-three-window-enable))
	      (select-window window)
	      (if proof-shrink-windows-tofit
		  (proof-resize-window-tofit)
		;; If we're not shrinking to fit, allow the size of
		;; this window to change.  [NB: might be nicer to
		;; fix the size based on user choice]
		(setq window-size-fixed nil))
	      ;; For various reasons, point may get moved around in
	      ;; response buffer.  Attempt to normalise its position.
	      (goto-char (or pos (point-max)))
	      (if pos
		  (beginning-of-line)
		(skip-chars-backward "\n\t "))
	      ;; Ensure point visible.  Again, window may have died
	      ;; inside shrink to fit, for some reason
	      (if (window-live-p window)
		  (unless (pos-visible-in-window-p (point) window)
		    (recenter -1)))))))))

(defun proof-clean-buffer (buffer)
  "Erase buffer and hide from display if proof-delete-empty-windows set.
Auto deletion only affects selected frame.  (We assume that the selected
frame is the one showing the script buffer.)
No effect if buffer is dead."
  (if (buffer-live-p buffer)
      (with-current-buffer buffer
	(unless (eq 0 (buffer-size)) ;; checkpoint unless already empty
	  (bufhist-checkpoint-and-erase))
	(set-buffer-modified-p nil)
	(if (eq buffer proof-response-buffer)
	    (setq pg-response-next-error nil))	; all error msgs lost!
	(if proof-delete-empty-windows
	    (delete-windows-on buffer t)))))

(defun proof-message (&rest args)
  "Issue the message ARGS in the response buffer and display it."
  (pg-response-display-with-face (apply 'concat args))
  (proof-display-and-keep-buffer proof-response-buffer))

(defun proof-warning (&rest args)
  "Issue the warning ARGS in the response buffer and display it.
The warning is coloured with proof-warning-face."
  (pg-response-display-with-face (apply 'concat args) 'proof-warning-face)
  (proof-display-and-keep-buffer proof-response-buffer))

(defun pg-internal-warning (message &rest args)
  "Display internal warning MESSAGE with ARGS as for format."
  (let ((formatted (apply 'format message args)))
    (if (fboundp 'display-warning)
	(display-warning 'proof-general formatted)
    (message formatted))))

;; could be a macro for efficiency in compiled code
(defun proof-debug (msg &rest args)
  "Issue the debugging message (format MSG ARGS) in the response buffer, display it.
If proof-general-debug is nil, do nothing."
  (if proof-general-debug
      (let ((formatted (apply 'format msg args)))
	(if (fboundp 'display-warning) ;; use builtin warning system in XEmacs
	    (display-warning 'proof-general formatted 'info)
	  ;; otherwise use response buffer with dedicated font, & display it
	  (progn
	    (pg-response-display-with-face formatted 'proof-debug-message-face)
	    (proof-display-and-keep-buffer proof-response-buffer))))))


;;; A handy utility function used in the "Buffers" menu, and throughout
;; the code.
(defun proof-switch-to-buffer (buf &optional noselect)
  "Switch to or display buffer BUF in other window unless already displayed.
If optional arg NOSELECT is true, don't switch, only display it.
No action if BUF is nil or killed."
  ;; Maybe this needs to be more sophisticated, using
  ;; proof-display-and-keep-buffer ?
  (and (buffer-live-p buf)
       (unless (eq buf (window-buffer (selected-window)))
	 (if noselect
	     ;; FIXME: would like 'norecord arg here to override
	     ;; previous window entering top of MRU list here.
	     ;; In fact, this could be hacked in XEmacs code.
	     ;; GNU Emacs seems *not* to put previously displayed
	     ;; window onto the top of the list with record-buffer:
	     ;; that gives much nicer behaviour than XEmacs here.
	     (display-buffer buf 'not-this-window)
	   (let ((pop-up-windows t))
	     (pg-pop-to-buffer buf 'not-this-window 'norecord))))))
  

;; Originally based on `shrink-window-if-larger-than-buffer', which
;; has a pretty wierd implementation.
;; TODO: desirable improvements would be to add some crafty history based
;; on user resize-events.  E.g. user resizes window, that becomes the
;; new maximum size.
;; FIXME: GNU Emacs has useful "window-size-fixed" which we use
;; HOWEVER, it's still not quite the right thing, it seems to me.
;; We'd like to specifiy a *minimum size* for a given buffer,
;; not a maximum.  With a frame split with just goals/response
;; we'd still get resize errors here using window-size-fixed.
;; FIXME: shrink-to-fit doesn't really work in three-buffer mode,
;; since shrinking one of the associated buffers tends to enlarge the
;; other (rather than just enlarging the proof state)
(defun proof-resize-window-tofit (&optional window)
  "Shrink the WINDOW to be as small as possible to display its contents.
Do not shrink to less than `window-min-height' lines.
Do nothing if the buffer contains more lines than the present window height,
or if some of the window's contents are scrolled out of view,
or if the window is not the full width of the frame,
or if the window is the only window of its frame."
;; da: actually seems okay in this case
  (interactive)
  (or window (setq window (selected-window)))
  ;; some checks before resizing to avoid messing custom display
  ;; [probably somewhat superfluous/extra rare]
  (if
      (or
       ;; The frame must not be minibuffer-only.
       (eq (frame-parameter (window-frame window) 'minibuffer) 'only)
       ;; We've got more than one window, right?
       (= 1 (let ((frame (selected-frame)))
	      (select-frame (window-frame window))
	      (unwind-protect ;; [why is this protected?]
		  (count-windows)
		(select-frame frame)
		(select-window window))))
       ;; the window is the full width, right?
       ;; [if not, we may be in horiz-split scheme, problematic]
       (not (window-leftmost-p window))
       (not (window-rightmost-p window)))
      ;; OK, we're not going to adjust the height here.  Moreover,
      ;; we'll make sure the height can be changed elsewhere.
      (setq window-size-fixed nil)
    (with-current-buffer (window-buffer window)
      (let*
	  ;; weird test cases:
	  ;; cur=45, max=23, desired=121, extraline=0
	  ;; current height
	  ;;; ((cur-height (window-height window))
	   ;; Most window is allowed to grow to
	  ((max-height
	    (/ (frame-height (window-frame window))
	       (if proof-three-window-enable
		   ;; we're in three-window-mode with
		   ;; all horizontal splits, so share the height.
		   3
		 ;; Otherwise assume a half-and-half split
		 2)))
	   ;; If buffer ends with a newline, ignore it when counting
	   ;; height unless point is after it.
	   (extraline
	    (if (and (not (eobp))
		     (eq ?\n (char-after (1- (point-max)))))
		1 0))
	   (test-pos (- (point-max) extraline))
	   ;; Direction of resizing based on whether max position is
	   ;; currently visible.  [ FIXME: not completely sensible:
	   ;; may be displaying end fraction of buffer! ]
	   ;; (shrink (pos-visible-in-window-p test-pos window))
	   ;; Likely desirable height is given by count-lines
	   (desired-height
	    ;; FIXME: is count-lines too expensive for v.large
	    ;; buffers?  Probably not an issue for us, but one
	    ;; wonders at the shrink to fit strategy.
	    ;; NB: way to calculate pixel fraction?
	    (+ extraline 1 (count-lines (point-min) (point-max)))))
	;; Let's shrink or expand.  Uses new GNU Emacs function.
	(let ((window-size-fixed nil))
	  (set-window-text-height window desired-height))
;; 	(cond
;; 	 ((and shrink
;; 	       (> cur-height window-min-height)
;; 	       ;; don't shrink if already too big; leave where it is
;; 	       (< cur-height max-height))
;; 	  (with-selected-window
;; 	   window
;; 	   (shrink-window (- cur-height (max window-min-height desired-height)))))
;; 	 (;; expand
;; 	  (< cur-height max-height)
;; 	  (with-selected-window window
;; 	   (enlarge-window
;; 	    (- (min max-height desired-height) cur-height)))))
	;; If we're at least the desirable height, it must be that the
	;; whole buffer can be seen --- so make sure display starts at
	;; beginning.
	;; NB: shrinking windows can sometimes delete them
	;; (although we don't want it to here!), but make this next
	;; check for robustness.
	(if (window-live-p window)
	    (progn
	      (if (>= (window-height window) desired-height)
		  (set-window-start window (point-min)))
	      ;; window-size-fixed is a GNU Emacs buffer local variable
	      ;; which determines window size of buffer.
	      ;; (setq window-size-fixed (window-height window))
	      ))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Function for submitting bug reports.
;;

(defun proof-submit-bug-report ()
  "Submit an bug report or other report for Proof General."
  (interactive)
  (require 'reporter)
  (let
      ((reporter-prompt-for-summary-p
	"(Very) brief summary of problem or suggestion: "))
    (reporter-submit-bug-report
     "da+pg-bugs@inf.ed.ac.uk"
     "Proof General"
     (list 'proof-general-version 'proof-assistant
	   'x-symbol-version)
     nil nil
     "*** Proof General now uses Trac for project management and bug reporting, please go to:
***
***    http://proofgeneral.inf.ed.ac.uk/trac/search
***
*** To see if your bug has been reported already, and a new ticket if not.
*** To report a bug, either register yourself as a user, or use the generic account
*** username \"pgemacs\" with password \"pgemacs\"
***
*** Please only continue with this email mechanism instead IF YOU REALLY MUST.
*** The address is not monitored very often and quite possibly will be ignored.
***
*** When reporting a bug, please include a small test case for us to repeat it.
*** Please also check that it is not already covered in the BUGS or FAQ files that came with
*** the distribution, or the latest versions at
***    http://proofgeneral.inf.ed.ac.uk/BUGS  and
***    http://proofgeneral.inf.ed.ac.uk/FAQ ")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utils for making functions to adjust user settings
;;;

(defun proof-deftoggle-fn (var &optional othername)
  "Define a function <VAR>-toggle for toggling a boolean customize setting VAR.
Args as for the macro `proof-deftoggle', except will be evaluated."
  (eval
   `(defun ,(if othername othername
	      (intern (concat (symbol-name var) "-toggle"))) (arg)
	      ,(concat "Toggle `" (symbol-name var) "'. With ARG, turn on iff ARG>0.
This function simply uses customize-set-variable to set the variable.
It was constructed with `proof-deftoggle-fn'.")
	      (interactive "P")
	      (customize-set-variable
	       (quote ,var)
	       (if (null arg) (not ,var)
		 (> (prefix-numeric-value arg) 0))))))

(defmacro proof-deftoggle (var &optional othername)
  "Define a function VAR-toggle for toggling a boolean customize setting VAR.
The toggle function uses customize-set-variable to change the variable.
OTHERNAME gives an alternative name than the default <VAR>-toggle.
The name of the defined function is returned."
  `(proof-deftoggle-fn (quote ,var) (quote ,othername)))

(defun proof-defintset-fn (var &optional othername)
  "Define a function <VAR>-intset for setting an integer customize setting VAR.
Args as for the macro `proof-defintset', except will be evaluated."
  (eval
   `(defun ,(if othername othername
	      (intern (concat (symbol-name var) "-intset"))) (arg)
	      ,(concat "Set `" (symbol-name var) "' to ARG.
This function simply uses customize-set-variable to set the variable.
It was constructed with `proof-defintset-fn'.")
	      (interactive (list
			    (read-number
			     (format "Value for %s (int, currently %s): "
				     (symbol-name (quote ,var))
				     (symbol-value (quote ,var))))))
	      (customize-set-variable (quote ,var) arg))))

(defmacro proof-defintset (var &optional othername)
  "Define a function <VAR>-intset for setting an integer customize setting VAR.
The setting function uses customize-set-variable to change the variable.
OTHERNAME gives an alternative name than the default <VAR>-intset.
The name of the defined function is returned."
  `(proof-defintset-fn (quote ,var) (quote ,othername)))

(defun proof-defstringset-fn (var &optional othername)
  "Define a function <VAR>-toggle for setting an integer customize setting VAR.
Args as for the macro `proof-defstringset', except will be evaluated."
  (eval
   `(defun ,(if othername othername
	      (intern (concat (symbol-name var) "-stringset"))) (arg)
	      ,(concat "Set `" (symbol-name var) "' to ARG.
This function simply uses customize-set-variable to set the variable.
It was constructed with `proof-defstringset-fn'.")
	      (interactive ,(format "sValue for %s (a string): "
				    (symbol-name var)))
	      (customize-set-variable (quote ,var) arg))))

(defmacro proof-defstringset (var &optional othername)
  "The setting function uses customize-set-variable to change the variable.
OTHERNAME gives an alternative name than the default <VAR>-stringset.
The name of the defined function is returned."
  `(proof-defstringset-fn (quote ,var) (quote ,othername)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macris for defining user-level functions (previously in proof-menu.el)
;;;


(defmacro proof-defshortcut (fn string &optional key)
  "Define shortcut function FN to insert STRING, optional keydef KEY.
This is intended for defining proof assistant specific functions.
STRING is inserted using `proof-insert', which see.
KEY is added onto proof-assistant map."
  `(progn
     (if ,key
	 (define-key (proof-ass keymap) (quote ,key) (quote ,fn)))
     (defun ,fn ()
       ,(concat "Shortcut command to insert "
		(replace-in-string  string "\\\\" "\\\\=") ;; for substitute-command-keys
		" into the current buffer.\nThis simply calls `proof-insert', which see.")
       (interactive)
       (proof-insert ,string))))

(defmacro proof-definvisible (fn string &optional key)
  "Define function FN to send STRING to proof assistant, optional keydef KEY.
This is intended for defining proof assistant specific functions.
STRING is sent using proof-shell-invisible-command, which see.
STRING may be a string or a function which returns a string.
KEY is added onto proof-assistant map."
  `(progn
     (if ,key
	 (define-key (proof-ass keymap) (quote ,key) (quote ,fn)))
     (defun ,fn ()
       ,(concat "Command to send "
		(if (stringp string)
		    (replace-in-string
		     string "\\\\" "\\\\=") ;; for substitute-command-keys
		  "an instruction")
		" to the proof assistant.")
       (interactive)
       ,(if (stringp string)
	    (list 'proof-shell-invisible-command string)
	  (list 'proof-shell-invisible-command (eval string))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Interface to custom lib
;;;

;; EMACSFIXME: A function that custom ought to provide.
(defun pg-custom-save-vars (&rest variables)
  "Save custom vars VARIABLES."
  (dolist (symbol variables)
    (let ((value (get symbol 'customized-value)))
      ;; This code from customize-save-customized adjusts
      ;; properties so that custom-save-all will save
      ;; the value.
      (when value
	(put symbol 'saved-value value)
	(if (fboundp 'custom-push-theme) ;; XEmacs customize
	    (custom-push-theme 'theme-value symbol 'user 'set value))
	(put symbol 'customized-value nil))))
  (custom-save-all))

;; FIXME: this doesn't do quite same thing as reset button,
;; which *removes* a setting from `custom-set-variables' list
;; in custom.el.  Instead, this adds something to a
;; custom-reset-variables list.
(defun pg-custom-reset-vars (&rest variables)
  "Reset custom vars VARIABLES to their default values."
  ;; FIXME: probably this XEmacs specific
  (apply 'custom-reset-variables
	 (mapcar (lambda (var) (list var 'default))
		 variables)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Finding executables
;;

(defun proof-locate-executable (progname &optional returnnopath extrapath)
  "Search for PROGNAME on PATH.  Return the full path to PROGNAME, or nil.
If RETURNNOPATH is non-nil, return PROGNAME even if we can't find a full path.
EXTRAPATH is a list of extra path components"
  (or
   (cond
    ((fboundp 'executable-find)
     (let ((exec-path (append exec-path extrapath)))
       (executable-find progname)))
    ((fboundp 'locate-file)
     (locate-file progname
		  (append (split-path (getenv "PATH")) extrapath)
		  (if proof-running-on-win32 '(".exe"))
		  1)))
   (if returnnopath progname)))

(provide 'proof-utils)
;;; proof-utils.el ends here
