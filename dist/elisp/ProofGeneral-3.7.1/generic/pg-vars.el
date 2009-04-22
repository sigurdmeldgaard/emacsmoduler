;;; pg-custom.el --- Proof General global variables
;;
;; Copyright (C) 2008 LFCS Edinburgh.
;; Author:      David Aspinall <David.Aspinall@ed.ac.uk> and others
;; License:     GPL (GNU GENERAL PUBLIC LICENSE)
;;
;; pg-vars.el,v 9.1 2008/01/30 15:44:59 da Exp
;;
;;; Commentary:
;; 
;; Global variables used in several files.
;;
;;

;;; Code:

;;;
;;; Early variables
;;;
(defvar proof-assistant-cusgrp nil
  "Symbol for the customization group of the user options for the proof assistant.
Do not change this variable! It is set automatically by the mode 
stub defined in proof-site, from the name given in 
proof-assistant-table.")

(defvar proof-assistant-internals-cusgrp nil
  "Symbol for the customization group of the PG internal settings proof assistant.
Do not change this variable! It is set automatically by the mode 
stub defined in proof-site, from the name given in
proof-assistant-table.")

(defvar proof-assistant ""
  "Name of the proof assistant Proof General is using.
Do not change this variable! It is set automatically by the mode 
stub defined in proof-site, from names given in proof-assistant-table.")

(defvar proof-assistant-symbol nil
  "Symbol for the proof assistant Proof General is using.
Used for automatic configuration based on standard variable names.
Settings will be found by looking for names beginning with this
symbol as a prefix.
Non-nil indicates PG has been initialised for an assistant.
If this is nil, the hook functions in `proof-ready-for-assistant-hook'
are yet to be run.

Do not change this variable! It is set automatically by the mode 
stub defined in proof-site, from the symbols given in 
`proof-assistant-table'.")

(defvar proof-mode-for-shell nil
  "Mode function for proof shell buffers. 
Do not change this variable! It is set automatically by the mode 
stub defined in proof-site to <PA>-shell-mode.")

(defvar proof-mode-for-response nil
  "Mode function for proof response buffer (and trace buffer, if used).
Do not change this variable! It is set automatically by the mode 
stub defined in proof-site to <PA>-response-mode.")

(defvar proof-mode-for-goals nil
  "Mode for proof state display buffers.
Do not change this variable! It is set automatically by the mode 
stub defined in proof-site to <PA>-goals-mode.")

(defvar proof-mode-for-script nil
  "Mode for proof script buffers.
Do not change this variable! It is set automatically by the mode 
stub defined in proof-site to <PA>-mode.")

(defvar proof-ready-for-assistant-hook nil
  "Hook functions to run after PG is configured for a proof assistant.
These functions allow late initialisation, once the choice of prover
has been set.")

;;;
;;; Later variables 
;;; (could be separated to cut down Emacs env pollution)
;;;

(defvar proof-shell-busy nil
  "A lock indicating that the proof shell is processing.
When this is non-nil, proof-shell-ready-prover will give
an error.")

(defvar proof-included-files-list nil
  "List of files currently included in proof process.
This list contains files in canonical truename format
\(see `file-truename').

Whenever a new file is being processed, it gets added to this list
via the proof-shell-process-file configuration settings.
When the prover retracts a file, this list is resynchronised via the
proof-shell-retract-files-regexp and proof-shell-compute-new-files-list
configuration settings.

Only files which have been *fully* processed should be included here.
Proof General itself will automatically add the filenames of a script
buffer which has been completely read when scripting is deactivated.
It will automatically remove the filename of a script buffer which
is completely unread when scripting is deactivated.

NB: Currently there is no generic provision for removing files which
are only partly read-in due to an error, so ideally the proof assistant
should only output a processed message when a file has been successfully
read.")

(defvar proof-script-buffer nil
  "The currently active scripting buffer or nil if none.")

(defvar proof-previous-script-buffer nil
  "Previous value of proof-script-buffer, recorded when scripting turned off.
This can be used to help multiple file handling.")

(defvar proof-shell-buffer nil
  "Process buffer where the proof assistant is run.")

(defvar proof-goals-buffer nil
  "The goals buffer.")

(defvar proof-response-buffer nil
  "The response buffer.")

(defvar proof-trace-buffer nil
  "A tracing buffer for storing tracing output from the proof shell.
See `proof-shell-trace-output-regexp' for details.")

(defvar proof-thms-buffer nil
  "A buffer for displaying a list of theorems from the proof assistant.
See `proof-shell-thm-display-regexp' for details.")

(defvar proof-shell-error-or-interrupt-seen nil
  "Flag indicating that an error or interrupt has just occurred.
Set to 'error or 'interrupt if one was observed from the proof
assistant during the last group of commands.")

(defvar pg-response-next-error nil
  "Error counter in response buffer to count for next error message.")

(defvar proof-shell-proof-completed nil
  "Flag indicating that a completed proof has just been observed.
If non-nil, the value counts the commands from the last command
of the proof (starting from 1).")

;; TODO da: remove proof-terminal-string.  At the moment some
;; commands need to have the terminal string, some don't.
;; It's used variously in proof-script and proof-shell, which
;; is another mess.  [Shell mode implicitly assumes script mode
;; has been configured].
;; We should assume commands are terminated at the specific level.

(defvar proof-terminal-string nil
  "End-of-line string for proof process.")


;;
;; Internal variables
;; -- usually local to a couple of modules but here to avoid 
;; compile warnings
;;

(defvar proof-shell-last-output nil
  "A record of the last string seen from the proof system.
This is raw string, for internal use only.")

(defvar proof-assistant-settings nil
 "A list of default values kept in Proof General for current proof assistant.
A list of lists (SYMBOL SETTING TYPE) where SETTING is a string value
to send to the proof assistant using the value of SYMBOL and 
and the function `proof-assistant-format'.  The TYPE item determines
the form of the menu entry for the setting.")

(defvar pg-tracing-slow-mode nil
  "Non-nil for slow refresh mode for tracing output.")

(defvar proof-nesting-depth 0
  "Current depth of a nested proof.
Zero means outside a proof, 1 means inside a top-level proof, etc.

This variable is maintained in `proof-done-advancing'; it is zeroed
in `proof-shell-clear-state'.")

(defvar proof-last-theorem-dependencies nil
  "Contains the dependencies of the last theorem.  A list of strings.
Set in `proof-shell-process-urgent-message'.")

(provide 'pg-vars)
;; pg-vars.el ends here
