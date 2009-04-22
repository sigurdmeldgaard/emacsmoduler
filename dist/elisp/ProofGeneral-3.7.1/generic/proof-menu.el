;; proof-menu.el  Menus, keymaps, and misc commands for Proof General
;;
;; Copyright (C) 2000,2001 LFCS Edinburgh. 
;; Authors:   David Aspinall
;; License:   GPL (GNU GENERAL PUBLIC LICENSE)
;;
;; proof-menu.el,v 9.4 2008/07/10 21:44:27 da Exp
;;

(require 'proof)           ; proof-deftoggle, proof-eval-when-ready-for-assistant

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Miscellaneous commands
;;;

(defvar proof-display-some-buffers-count 0)

(defun proof-display-some-buffers ()
  "Display the reponse, trace, goals, or shell buffer, rotating.
A fixed number of repetitions of this command switches back to
the same buffer.
Also move point to the end of the response buffer if it's selected.
If in three window or multiple frame mode, display two buffers.
The idea of this function is to change the window->buffer mapping 
without adjusting window layout."
  (interactive)
  ;; The GUI-tessence here is to implement a humane toggle, which
  ;; allows habituation.  E.g. two taps of C-c C-l always
  ;; shows the goals buffer, three the trace buffer, etc.
  ;; (That behaviour makes less sense from the menu, though,
  ;;  where it seems more natural just to rotate from last 
  ;;  position)
  (cond 
   ((and (interactive-p) 
	 (eq last-command 'proof-display-some-buffers))
    (incf proof-display-some-buffers-count))
   (t
    (setq proof-display-some-buffers-count 0)))
  (let* ((assocbufs   (remove-if-not 'buffer-live-p 
				     (list proof-response-buffer
					   proof-thms-buffer
					   proof-trace-buffer
					   proof-goals-buffer
					   )))
					;proof-shell-buffer
	 (numassoc    (length assocbufs)))
    ;; If there's no live other buffers, we don't do anything.
    (unless (zerop numassoc)
      (let
	 ((selectedbuf (nth (mod proof-display-some-buffers-count 
				 numassoc) assocbufs))
	  (nextbuf     (nth (mod (1+ proof-display-some-buffers-count)
				 numassoc) assocbufs)))
	(cond
	 ((or proof-three-window-enable proof-multiple-frames-enable)
	  ;; Display two buffers: next in rotation and goals/response
	  ;; FIXME: this doesn't work as well as it might.
	  (proof-switch-to-buffer selectedbuf 'noselect)
	  (proof-switch-to-buffer (if (eq selectedbuf proof-goals-buffer)
				      proof-response-buffer
				    proof-goals-buffer) 'noselect))
	 (selectedbuf
	  (proof-switch-to-buffer selectedbuf 'noselect)))
	(if (eq selectedbuf proof-response-buffer)
	    (set-window-point (get-buffer-window proof-response-buffer t)
			      (point-max)))
	(pg-hint (pg-response-buffers-hint (buffer-name nextbuf)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Key bindings
;;;

;;;###autoload
(defun proof-menu-define-keys (map)
  ;; Prover specific keymap under C-c C-a 
  (proof-eval-when-ready-for-assistant
      (define-key map [(control c) (control a)] (proof-ass keymap)))
  ;; M-a and M-e are usually {forward,backward}-sentence.
  ;; Some modes also override these with similar commands
  (define-key map [(meta a)] 'proof-backward-command)
  (define-key map [(meta e)] 'proof-forward-command)
  (define-key map [(meta up)] 'proof-backward-command)
  (define-key map [(meta down)] 'proof-forward-command)
  (define-key map [(control meta a)] 'proof-goto-command-start)
  (define-key map [(control meta e)] 'proof-goto-command-end)
  (define-key map [(control c) (control b)] 'proof-process-buffer)
  ;; C-c C-c is proof-interrupt-process in universal-keys
  ;; C-c C-f is proof-find-theorems in universal-keys
  (define-key map [(control c) (control h)] 'proof-help)
  ;; C-c C-l is proof-layout-windows in universal-keys
  ;; C-c C-n is proof-assert-next-command-interactive in universal-keys
  ;; C-c C-o is proof-display-some-buffers in universal-keys
  (define-key map [(control c) (control p)] 'proof-prf)
  (define-key map [(control c) (control r)] 'proof-retract-buffer)
  (define-key map [(control c) (control s)] 'proof-toggle-active-scripting)
  (define-key map [(control c) (control t)] 'proof-ctxt)
  ;; C-c C-u is proof-undo-last-successful-command in universal-keys
  ;; C-c C-w is pg-response-clear-displays in universal-keys
  (define-key map [(control c) (control z)] 'proof-frob-locked-end)
  (define-key map [(control c) (control backspace)] 
    'proof-undo-and-delete-last-successful-command)
  ;; C-c C-v is proof-minibuffer-cmd in universal-keys
  ;; C-c C-. is proof-goto-end-of-locked in universal-keys
  (define-key map [(control c) (control return)] 'proof-goto-point)
  (define-key map [(control c) v] 'pg-toggle-visibility);; FIXME: Emacs binding?
  (cond ((featurep 'xemacs)
	 (define-key map [(control button3)] 'proof-mouse-goto-point)
	 (define-key map [(control button1)] 'proof-mouse-track-insert)) ; no Emacs binding
	(t 
	 (define-key map [(control mouse-3)] 'proof-mouse-goto-point)))
  ;; NB: next binding overwrites comint-find-source-code.  
  (define-key map [(meta p)] 'pg-previous-matching-input-from-input)
  (define-key map [(meta n)] 'pg-next-matching-input-from-input)
  ;; Standard binding for completion
  (define-key map [(control return)] 'proof-script-complete)
  (define-key map [(control c) (control ?\;)] 'pg-insert-last-output-as-comment)
  ;;
  ;; Experimental: span moving functions
  (if proof-experimental-features 
      (progn
	(define-key map [(control meta up)] 'pg-move-region-up)
	(define-key map [(control meta down)] 'pg-move-region-down)))
  ;; Add the universal keys bound in all PG buffers.
  ;; NB: C-c ` is next-error in universal-keys
  (proof-define-keys map proof-universal-keys))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions to define the menus
;;;

;; The main Proof-General generic menu

;;;###autoload
(defun proof-menu-define-main ()
  (easy-menu-define 
   proof-mode-menu  
   proof-mode-map
   "The main Proof General menu"
   (proof-main-menu)))

;; The proof assistant specific menu

(defvar proof-menu-favourites nil
  "The Proof General favourites menu for the current proof assistant.")

;;;###autoload
(defun proof-menu-define-specific ()
  (easy-menu-define 
    proof-assistant-menu 
    proof-mode-map
    (concat "The menu for " proof-assistant)
    (cons proof-assistant
	  (append
	   (proof-ass menu-entries)
	   '("----")
	   (or proof-menu-favourites
	       (proof-menu-define-favourites-menu))
	   (or proof-menu-settings
	       (proof-menu-define-settings-menu))
	   '("----")
	   (list
	    (vector
	     (concat "Start " proof-assistant)
	     'proof-shell-start
	     ':active '(not (proof-shell-live-buffer)))
	    (vector
	     (concat "Exit " proof-assistant)
	     'proof-shell-exit
	     ':active '(proof-shell-live-buffer)))
	   '("----")
	   (list
	    (cons "Help"
		  (append
		   (list
		    (vector
		     (concat proof-assistant " information")
		     'proof-help
		     menuvisiblep proof-info-command)
		    (vector
		     (concat proof-assistant " web page")
		     '(browse-url proof-assistant-home-page)
		     menuvisiblep proof-assistant-home-page))
		   (proof-ass help-menu-entries))))))))

(defun proof-assistant-menu-update ()
  "Update proof assistant menu in scripting buffers."
  (proof-map-buffers (proof-buffers-in-mode proof-mode-for-script)
    ;; NB: easy-menu-remove is odd in XEmacs, it considerably changes the mode popup menu.  
    ;; In GNU Emacs this first instruction does nothing.
    (easy-menu-remove proof-assistant-menu)
    (proof-menu-define-settings-menu)
    (proof-menu-define-specific)
    (easy-menu-add proof-assistant-menu (proof-ass mode-map))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Contents of sub menus
;;

(defvar proof-help-menu
  '("Help"
    ["About PG"        proof-splash-display-screen t]
    ["PG Info"      	(info "ProofGeneral") t]
    ["PG Homepage"  	(browse-url proof-general-home-page) t]
    ["Send Bug Report" proof-submit-bug-report t])
  "Proof General help menu.")

(defvar proof-show-hide-menu
  '(("Show All"
     ["Proofs"    (pg-show-all-portions "proof") t]
     ["Comments"  (pg-show-all-portions "comment") t])
    ("Hide All"
     ["Proofs"    (pg-show-all-portions "proof" 'hide) t]
     ["Comments"  (pg-show-all-portions "comment" 'hide) t]))
  "Show/hide submenu.")

(defvar proof-buffer-menu
  (cons "Buffers"
	'(["Layout Windows"
	   proof-layout-windows]
	  ["Rotate Output Buffers"
	   proof-display-some-buffers
	   :active (buffer-live-p proof-goals-buffer)]
	  ["Clear Responses"
	   pg-response-clear-displays
	   :active (buffer-live-p proof-response-buffer)]
	  "----"
	  ["Active Scripting"
	   (proof-switch-to-buffer proof-script-buffer)
	   :active (buffer-live-p proof-script-buffer)]
	  ["Goals"
	   (proof-switch-to-buffer proof-goals-buffer t)
	   :active (buffer-live-p proof-goals-buffer)]
	  ["Response"
	   (proof-switch-to-buffer proof-response-buffer t)
	   :active (buffer-live-p proof-response-buffer)]
	  ;; FIXME: next test doesn't work: menus are loaded before
	  ;; proof-shell-trace-output-regexp is set (in proof-shell hook).  
	  ;; Should be better with simplified customization mechanism.
	  ;; ( if proof-shell-trace-output-regexp ... )
	  ["Trace"
	   (proof-switch-to-buffer proof-trace-buffer)
	   :active (buffer-live-p proof-trace-buffer)]
	  ["Shell"
	   (proof-switch-to-buffer proof-shell-buffer)
	   :active (buffer-live-p proof-shell-buffer)]))
  "Proof General buffer menu.")
	  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; "Quick" (or main) options
;;

;; First, make the togglers used in options menu below

(proof-deftoggle proof-script-fly-past-comments)
(proof-deftoggle proof-delete-empty-windows)
(proof-deftoggle proof-shrink-windows-tofit)
(proof-deftoggle proof-multiple-frames-enable proof-multiple-frames-toggle)
(proof-deftoggle proof-three-window-enable proof-three-window-toggle)
(proof-deftoggle proof-disappearing-proofs)
(proof-deftoggle proof-strict-read-only)

(proof-deftoggle-fn 'proof-imenu-enable 'proof-imenu-toggle)
(proof-deftoggle proof-keep-response-history)

(proof-eval-when-ready-for-assistant
  (proof-deftoggle-fn 
   (proof-ass-sym x-symbol-enable) 'proof-x-symbol-toggle)
  (proof-deftoggle-fn 
   (proof-ass-sym unicode-tokens-enable) 'proof-unicode-tokens-toggle)
;  (proof-deftoggle-fn 
;   (proof-ass-sym unicode-tokens2-enable) 'proof-unicode-tokens2-toggle)
  (proof-deftoggle-fn 
   (proof-ass-sym maths-menu-enable) 'proof-maths-menu-toggle)
  (proof-deftoggle-fn (proof-ass-sym mmm-enable) 'proof-mmm-toggle))


(defun proof-keep-response-history ()
  "Enable associated buffer histories following `proof-keep-response-history'."
  (if proof-keep-response-history
      (proof-map-buffers (proof-associated-buffers) (bufhist-init))
    (proof-map-buffers (proof-associated-buffers) (bufhist-exit))))

;; Here is the menu

(defconst proof-quick-opts-menu
  (cons 
   "Options"
   `(["Electric Terminator" proof-electric-terminator-toggle
     :style toggle
     :selected proof-electric-terminator-enable]
     ["Fly Past Comments" proof-script-fly-past-comments-toggle
      ,menuvisiblep (not proof-script-use-old-parser)
      :style toggle
      :selected proof-script-fly-past-comments]
     ["Disppearing Proofs" proof-disappearing-proofs-toggle 
      :style toggle
      :selected proof-disappearing-proofs]

     ["Strict Read Only" proof-strict-read-only-toggle
      :style toggle
      :selected proof-strict-read-only]
     
     ["X-Symbol"
      (progn
	(unless x-symbol-mode (proof-x-symbol-toggle 0))
	(proof-x-symbol-toggle (if x-symbol-mode 0 1)))
      :active (and 
	       ;; X-Symbol breaks abruptly on recent 23 versions
	       (not (>= emacs-major-version 23))
	       (not (and (boundp 'unicode-tokens-mode) 
			 unicode-tokens-mode))
	       (proof-x-symbol-support-maybe-available))
      :style toggle
      :selected (and (boundp 'x-symbol-mode) x-symbol-mode)]

     ["Unicode Tokens" 
      (progn
	(unless unicode-tokens-mode (proof-x-symbol-toggle 0))
	(proof-unicode-tokens-toggle (if unicode-tokens-mode 0 1)))
      :active (and (not (and (boundp 'x-symbol-mode) x-symbol-mode))
		   (proof-unicode-tokens-support-available))
      :style toggle
      :selected (and (boundp 'unicode-tokens-mode) 
		     unicode-tokens-mode)]

;;;      ["Unicode Tokens 2" 
;;;       (progn
;;; 	(unless unicode-tokens2-mode (proof-x-symbol-toggle 0))
;;; 	(proof-unicode-tokens2-toggle (if unicode-tokens2-mode 0 1)))
;;;       :active (and (not (and (boundp 'x-symbol-mode) x-symbol-mode))
;;; 		   (proof-unicode-tokens2-support-available))
;;;       :style toggle
;;;       :selected (and (boundp 'unicode-tokens2-mode) 
;;; 		     unicode-tokens2-mode)]

     ["Unicode Maths Menu" (proof-maths-menu-toggle (if maths-menu-mode 0 1))
      :active (proof-maths-menu-support-available)
      :style toggle
      :selected (and (boundp 'maths-menu-mode) maths-menu-mode)]
 
     ["Multiple Modes" (proof-mmm-toggle (if mmm-mode 0 1))
      :active (proof-mmm-support-available)
      :style toggle
      :selected (and (boundp 'mmm-mode) mmm-mode)]
 
     ["Toolbar" proof-toolbar-toggle
      ;; should really be split into :active & GNU Emacs's :visible
      :active (and (or (featurep 'toolbar) (featurep 'tool-bar))
			 (boundp 'proof-buffer-type)
			 ;; only allow toggling of toolbar enable in one
			 ;; buffer to avoid strange effects because we
			 ;; only keep one flag.  (Strange effects because 
			 ;; we only turn it off in one buffer at a time)
			 (eq proof-buffer-type 'script))
      :style toggle
      :selected proof-toolbar-enable]

;;; TODO: Add this in PG 3.7.1 once; see trac #169 
;;;      ["Response history" proof-keep-response-history-toggle
;;;       :style toggle
;;;       :selected proof-keep-response-history]

     ["Index Menu" proof-imenu-toggle
      :active (stringp (locate-library "imenu"))
      :style toggle
      :selected proof-imenu-enable]

     ;; NB: convenience; speedbar isn't saved/resumed automatically.
     ["Speedbar" speedbar
      :active (stringp (locate-library "speedbar"))
      :style toggle
      :selected (and (boundp 'speedbar-frame) speedbar-frame)]

     ("Display"
      ["Layout Windows" proof-layout-windows]
      ["Use Three Panes" proof-three-window-toggle
       :active (not proof-multiple-frames-enable)
       :style toggle
       :selected proof-three-window-enable]
      ;; We use non-Emacs terminology "Windows" in this menu to help
      ;; non-Emacs users.  Cf. Gnome usability studies: menus saying
      ;; "Web Browser" more useful to novices than menus saying "Mozilla"!!
      ["Multiple Windows" proof-multiple-frames-toggle
       :active (and window-system t)
       :style toggle
       :selected proof-multiple-frames-enable]
      ["Delete Empty Panes" proof-delete-empty-windows-toggle
       :active (not proof-multiple-frames-enable)
       :style toggle
       :selected proof-delete-empty-windows]
      ["Shrink to Fit" proof-shrink-windows-tofit-toggle
       :active (not proof-multiple-frames-enable)
       :style toggle
       :selected proof-shrink-windows-tofit])
     ("Follow Mode" 
      ["Follow Locked Region" 
       (customize-set-variable 'proof-follow-mode 'locked)
       :style radio
       :selected (eq proof-follow-mode 'locked)]
;; Not implemented.  See Trac #187
;;       ["Follow On Success" 
;;        (customize-set-variable 'proof-follow-mode 'followsuccess)
;;        :style radio
;;        :selected (eq proof-follow-mode 'followdown)]
      ["Follow Locked Region Down" 
       (customize-set-variable 'proof-follow-mode 'followdown)
       :style radio
       :selected (eq proof-follow-mode 'followdown)]
      ["Keep Locked Region Displayed" 
       (customize-set-variable 'proof-follow-mode 'follow)
       :style radio
       :selected (eq proof-follow-mode 'follow)]
      ["Never Move" 
       (customize-set-variable 'proof-follow-mode 'ignore)
       :style radio
       :selected (eq proof-follow-mode 'ignore)])
     ;; Add this because it's a handy one to set (usually to retract)
     ("Deactivate Action"
      ["Retract" 
       (customize-set-variable 'proof-auto-action-when-deactivating-scripting 'retract)
       :style radio
       :selected (eq proof-auto-action-when-deactivating-scripting 'retract)]
      ["Process" 
       (customize-set-variable 'proof-auto-action-when-deactivating-scripting 'process)
       :style radio
       :selected (eq proof-auto-action-when-deactivating-scripting 'process)]
      ["Query"
       (customize-set-variable 'proof-auto-action-when-deactivating-scripting nil)
       :style radio
       :selected (null proof-auto-action-when-deactivating-scripting)])
     "----"
     ["Reset Options" (proof-quick-opts-reset) 
      (proof-quick-opts-changed-from-defaults-p)]
     ["Save Options" (proof-quick-opts-save) 
      (proof-quick-opts-changed-from-saved-p)]))
  "Proof General quick options.")

(defun proof-quick-opts-vars ()
  "Return a list of the quick option variables."
  (list
   'proof-electric-terminator-enable
   'proof-script-fly-past-comments
   'proof-disappearing-proofs 
   ;;'proof-output-fontify-enable
   'proof-strict-read-only
   (proof-ass-sym x-symbol-enable)
   (proof-ass-sym unicode-tokens-enable)
   (proof-ass-sym maths-menu-enable)
   (proof-ass-sym mmm-enable)
   'proof-toolbar-enable
   'proof-keep-response-history
   'proof-imenu-enable
   ;; Display sub-menu
   'proof-three-window-enable
   'proof-delete-empty-windows
   'proof-multiple-frames-enable
   'proof-shrink-windows-tofit
   'proof-multiple-frames-enable
   ;; Follow mode sub-menu
   'proof-follow-mode
   ;; Deactivate scripting action
   proof-auto-action-when-deactivating-scripting))

(defun proof-quick-opts-changed-from-defaults-p ()
  ;; NB: would be nice to add.  Custom support?
  t)

(defun proof-quick-opts-changed-from-saved-p ()
  ;; NB: would be nice to add.  Custom support?
  t)


;; 
;; We have menu items for saving options and reseting them.
;; We could just store the settings automatically (no save),
;; but then the reset option would have to change to restore
;; to manufacturer settings (rather then user-stored ones).
;;
(defun proof-quick-opts-save ()
  "Save current values of PG Options menu items using custom."
  (interactive)
  (apply 'pg-custom-save-vars (proof-quick-opts-vars)))

(defun proof-quick-opts-reset ()
  "Reset PG Options menu to default (or user-set) values, using custom."
  (interactive)
  (apply 'pg-custom-reset-vars (proof-quick-opts-vars)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Main menu
;;


(defconst proof-config-menu
  (list "----"
	;; buffer menu might better belong in toolbar menu?
	proof-buffer-menu
	proof-quick-opts-menu)
  "Proof General configuration menu.")

(defconst proof-advanced-menu
  (cons "Advanced..."
	(append
	 '(["Complete Identifier" proof-script-complete t]
	   ["Insert last output" pg-insert-last-output-as-comment proof-shell-last-output])
	 (list "-----")
	 proof-show-hide-menu
	 (list "-----")
	 (list (customize-menu-create 'proof-general))
	 (list (customize-menu-create 'proof-general-internals "Internals"))))
  "Advanced sub-menu of script functions and customize.")


(defvar proof-menu  
  '(["Next Error" proof-next-error
     :active pg-next-error-regexp]
    ["Scripting Active" proof-toggle-active-scripting
     :style toggle
     :selected (eq proof-script-buffer (current-buffer))])
  "The Proof General generic menu for scripting buffers.")


(defun proof-main-menu ()
  "Construct and return PG main menu used in scripting buffers."
  (cons proof-general-name
	(append
	 (proof-toolbar-scripting-menu)
	 proof-menu
	 proof-config-menu
	 (list proof-advanced-menu)
	 (list proof-help-menu))))

;;;###autoload
(defun proof-aux-menu ()
  "Construct and return PG auxiliary menu used in non-scripting buffers."
  (cons proof-general-name 
	(append
	 (proof-toolbar-scripting-menu)
	 proof-config-menu
	 (list proof-help-menu))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Favourites mechanism for prover-specific menu
;;;

(defun proof-menu-define-favourites-menu ()
  "Return menu generated from `PA-favourites'."
  (let ((favs (reverse (proof-ass favourites))) ents)
    (while favs
      (setq ents (cons (apply 'proof-def-favourite (car favs)) ents))
      (setq favs (cdr favs)))
    (setq proof-menu-favourites 
	  (list 
	   (cons "Favourites" 
		 (append ents
			 ;; (list "----")  doesn't work for adding before
			 '(["Add Favourite" 
			    (call-interactively 'proof-add-favourite) t]
			   ["Delete Favourite" 
			    (call-interactively 'proof-del-favourite) t]
			   ["Save Favourites"
			    (proof-save-favourites) t])))))))
  
;;; Define stuff from favourites

(defun proof-def-favourite (command inscript menuname &optional key new)
  "Define and a \"favourite\" proof assisant function.
See doc of `proof-add-favourite' for first four arguments.
Extra NEW flag means that this should be a new favourite, so check
that function defined is not already bound.
This function defines a function and returns a menu entry 
suitable for adding to the proof assistant menu."
  (let* ((menunames	(split-string (downcase menuname)))
	 (menuname-sym  (proof-sym (proof-splice-separator "-" menunames)))
	 (menu-fn	menuname-sym) (i 1))
    (while (and new (fboundp menu-fn))
      (setq menu-fn (intern (concat (symbol-name menuname-sym)
				    "-" (int-to-string i))))
      (incf i))
    (if inscript
	(eval `(proof-defshortcut ,menu-fn ,command ,key))
      (eval `(proof-definvisible ,menu-fn ,command ,key)))
    ;; Return menu entry
    (vector menuname menu-fn t)))


;;; Code for adding "favourites" to the proof-assistant specific menu

(defvar proof-make-favourite-cmd-history nil
  "History for proof-make-favourite.")

(defvar proof-make-favourite-menu-history nil
  "History for proof-make-favourite.")

(defun proof-save-favourites ()
  "Save favourites in customization settings."
  (interactive)
  (pg-custom-save-vars (proof-ass-sym favourites)))

(defun proof-del-favourite (menuname)
  "Delete \"favourite\" command recorded at MENUNAME."
  (interactive
   (list
    (completing-read "Menu item to delete: " 
		     (mapcar 'cddr (proof-ass favourites))
		     nil t)))
  (let*
      ((favs       (proof-ass favourites))
       (rmfavs	   (remove-if 
		    (lambda (f) (string-equal menuname (caddr f)))
		    favs)))
    (unless (equal favs rmfavs)
      (easy-menu-remove-item proof-assistant-menu 
			     '("Favourites") menuname)
      (customize-set-variable  (proof-ass-sym favourites) rmfavs))))
  
(defun proof-read-favourite ()
  (let* 
      ((guess  (buffer-substring (save-excursion
				   (beginning-of-line-text)
				   (point)) (point)))
       (cmd (read-string
	     (concat "Command to send to " proof-assistant ": ") 
	     guess
	     proof-make-favourite-cmd-history))
       (ins (y-or-n-p "Should command be recorded in script? "))
       (men (read-string
	     "Name of command on menu: " 
	     cmd
	     proof-make-favourite-menu-history))
       (key (if (y-or-n-p "Set a keybinding for this command? : ")
		;; FIXME: better validation here would be to check
		;; this is a new binding, or remove old binding below.
		(events-to-keys
		 (read-key-sequence 
		  "Type the key to use (binding will be C-c C-a <key>): " 
		  nil t)))))
    ;; result
    (list cmd ins men key)))
	  

(defun proof-add-favourite (command inscript menuname &optional key)
  "Define and add a \"favourite\" proof-assisant function to the menu bar.
The favourite function will issue COMMAND to the proof assistant.  
COMMAND is inserted into script (not sent immediately) if INSCRIPT non-nil.
MENUNAME is the name of the function for the menu.
KEY is the optional key binding."
  (interactive (proof-read-favourite))
  (let*
      ((menu-entry (proof-def-favourite command inscript menuname key t))
       (favs       (proof-ass favourites))
       (rmfavs	   (remove-if 
		    (lambda (f) (string-equal menuname (caddr f)))
		    favs))
       (newfavs    (append 
		    rmfavs 
		    (list (list command inscript menuname key)))))
    ;; If def succeeds, add to customize var
    (customize-set-variable  (proof-ass-sym favourites) newfavs)
    (easy-menu-add-item proof-assistant-menu 
			'("Favourites") menu-entry "Add Favourite")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Proof assistant settings mechanism.
;;;

(defvar proof-menu-settings nil
  "Settings submenu for Proof General.")

(defun proof-menu-define-settings-menu ()
  "Return menu generated from `proof-assistant-settings', update `proof-menu-settings'."
  (if proof-assistant-settings
      (let ((setgs proof-assistant-settings) 
	    (save  (list "----"
			 ["Reset Settings" (proof-settings-reset) 
			  (proof-settings-changed-from-defaults-p)]
			 ["Save Settings" (proof-settings-save)
			  (proof-settings-changed-from-saved-p)]))
	    ents)
	;; TODO: for a large number of settings, we could generate
	;; sub-menus according to the group.  
	(while setgs
	  (setq ents (cons 
		      (apply 'proof-menu-entry-for-setting (car setgs)) ents))
	  (setq setgs (cdr setgs)))
	(setq proof-menu-settings 
	      (list (cons "Settings" 
			  (nconc ents save)))))))

(defun proof-menu-entry-name (symbol)
  "Return a nice menu entry name for SYMBOL."
  ;; NB: for grouped settings, there is a pggroup symbol property and
  ;; by convention the name of the setting begins with grp:
  ;; We strip the group name from the menu entry name.
  (let ((grp (get symbol 'pggroup))
	(nm  (symbol-name symbol)))
    (upcase-initials
     (replace-in-string 
      (if grp (replace-in-string nm (concat (downcase grp) ":") "") nm)
      "-" " "))))
	
(defun proof-menu-entry-for-setting (symbol setting type)
  (let ((entry-name  (proof-menu-entry-name symbol))
	(pasym	     (proof-ass-symv symbol)))
    (cond
     ((eq type 'boolean)
      (vector entry-name 
	      (proof-deftoggle-fn pasym)
	      :style 'toggle
	      :selected pasym))
     ((eq type 'integer)
      (vector entry-name 
	      (proof-defintset-fn pasym)
	      t))
     ((eq type 'string)
      (vector entry-name 
	      (proof-defstringset-fn pasym)
	      t)))))

(defun proof-settings-vars ()
  "Return a list of proof assistant setting variables."
  (mapcar (lambda (setting) (proof-ass-symv (car setting)))
	  proof-assistant-settings))

(defun proof-settings-changed-from-defaults-p ()
  ;; FIXME: would be nice to add.  Custom support?
  t)

(defun proof-settings-changed-from-saved-p ()
  ;; FIXME: would be nice to add.  Custom support?
  t)

(defun proof-settings-save ()
  "Save current values of proof assistant settings using Custom."
  (interactive)
  (apply 'pg-custom-save-vars (proof-settings-vars)))

(defun proof-settings-reset ()
  "Reset proof assistant settings to their default values."
  (interactive)
  (apply 'pg-custom-reset-vars (proof-settings-vars)))

;;; autoload for compiled version: used in macro proof-defpacustom
;;;###autoload
(defun proof-defpacustom-fn (name val args)
  "As for macro `defpacustom' but evaluating arguments."
  (let (newargs setting evalform type)
    (while args
      (cond 
       ((eq (car args) :setting)
	(setq setting (cadr args))
	(setq args (cdr args)))
       ((eq (car args) :eval)
	(setq evalform (cadr args))
	(setq args (cdr args)))
       ((eq (car args) :pgipcmd)
	;; Construct a function which yields a PGIP string 
	(setq setting `(lambda (x) 
			  (pg-pgip-string-of-command (proof-assistant-format ,(cadr args) x))))
	(setq args (cdr args)))
       ((eq (car args) :pggroup) 
	;; use the group as a prefix to the name, and set a pggroup property on it
	(setq name (intern (concat (downcase (cadr args)) ":" (symbol-name name))))
	(put name 'pggroup (cadr args))
	(setq args (cdr args)))
       ((eq (car args) :type)
	(setq type (cadr args))
	(setq args (cdr args))
	(setq newargs (cons type (cons :type newargs))))
       (t
	(setq newargs (cons (car args) newargs))))
      (setq args (cdr args)))
    (setq newargs (reverse newargs))
    ;; PG 3.5 patch 22.4.03: allow empty :setting, :eval,
    ;; because it's handy to put stuff on settings menu but
    ;; inspect the settings elsewhere in code.
    ;; (unless (or setting evalform)
    ;; (error "defpacustom: missing :setting or :eval keyword"))
    (unless (and type
		  (or (eq (eval type) 'boolean) 
		      (eq (eval type) 'integer) 
		      (eq (eval type) 'string)))
      (error "defpacustom: missing :type keyword or wrong :type value"))
    ;; Debug message in case a defpacustom is repeated.
    ;; NB: this *may* happen dynamically, but shouldn't: if the
    ;; interface queries the prover for the settings it supports,
    ;; then the internal list should be cleared first.
    ;; FIXME: for now, we support redefinitions, by calling 
    ;; pg-custom-undeclare-variable.
    (if (assoc name proof-assistant-settings)
	(progn
	  (proof-debug "defpacustom: Proof assistanting setting %s re-defined!"
		       name)
	  (undefpgcustom name)))
    ;; Could consider moving the bulk of the remainder of this
    ;; function to a function proof-assistant-setup-settings which
    ;; defines the custom vals *and* menu entries.  This would allow
    ;; proof assistant customization to manipulate
    ;; proof-assistant-settings directly rather than forcing use of
    ;; defpacustom.  (Probably stay as we are: more abstract)
    (eval
     `(defpgcustom ,name ,val
	,@newargs
	:set 'proof-set-value
	:group 'proof-assistant-setting))
    (cond
     (evalform
      (eval
       `(defpgfun ,name ()
	  ,evalform)))
     (setting
      (eval
       `(defpgfun ,name ()
	  (proof-assistant-invisible-command-ifposs
	   (proof-assistant-settings-cmd (quote ,name)))))))
    (setq proof-assistant-settings
	  (cons (list name setting (eval type)) 
		(remassoc name proof-assistant-settings)))))

;;;###autoload
(defmacro defpacustom (name val &rest args)
  "Define a setting NAME for the current proof assitant, default VAL.
NAME can correspond to some internal setting, flag, etc, for the
proof assistant, in which case a :setting and :type value should be provided.
The :type of NAME should be one of 'integer, 'boolean, 'string.
The customization variable is automatically in group `proof-assistant-setting'.
The function `proof-assistant-format' is used to format VAL.
If NAME corresponds instead to a PG internal setting, then a form :eval to
evaluate can be provided instead."
  (eval-when-compile
    (if (boundp 'proof-assistant-symbol)
	;; declare variable to compiler to prevent warnings
	(eval `(defvar ,(proof-ass-sym name) nil "Dummy for compilation."))))
  `(proof-defpacustom-fn (quote ,name) (quote ,val) (quote ,args)))

(defun proof-assistant-invisible-command-ifposs (cmd)
  "Send CMD as an \"invisible command\" if the proof process is available."
  ;; FIXME: better would be to queue the command, or even interrupt a
  ;; queue in progress.  Also must send current settings at start of
  ;; session somehow.  (This might happen automatically if a queue of
  ;; deffered commands is set, since defcustom calls proof-set-value
  ;; even to set the default/initial value?)
  (if (proof-shell-available-p)
      (progn
	(proof-shell-invisible-command cmd t)
	;; refresh display, 
	;; FIXME: should only do if goals display is active,
	;; messy otherwise. 
	;; (we need a new flag for "active goals display").  
	;; PG 3.5 (patch 22.04.04):
	;; Let's approximate that by looking at proof-nesting-depth.
	(if (and proof-showproof-command 
		 (> proof-nesting-depth 0))
	    (proof-shell-invisible-command proof-showproof-command))
	;;  Could also repeat last command if non-state destroying.
	)))

(defun proof-maybe-askprefs ()
  "If `proof-assistant-settings' is unset, try to issue <askprefs>"
  (if (and (not proof-assistant-settings)
	   proof-shell-issue-pgip-cmd)
      (pg-pgip-askprefs)))
  

(defun proof-assistant-settings-cmd (&optional setting)
  "Return string for settings kept in Proof General customizations.
If SETTING is non-nil, return a string for just that setting. 
Otherwise return a string for configuring all settings.
NB: if no settings are configured, this has no effect."
  (if proof-assistant-settings
      (let
	  ((evalifneeded (lambda (expr)
			   (if (and (cadr expr) ;; setting has PA string?
				    (or (not setting) 
					(eq setting (car expr))))
			       (proof-assistant-format 
				(cadr expr) 
				(eval (proof-ass-symv (car expr))))))))
	(apply 'concat (mapcar evalifneeded 
			       proof-assistant-settings)))))

(defvar proof-assistant-format-table 
  (list
   (cons "%b" '(proof-assistant-format-bool curvalue))
   (cons "%i" '(proof-assistant-format-int curvalue))
   (cons "%s" '(proof-assistant-format-string curvalue)))
  "Table to use with `proof-format' for formatting CURVALUE for assistant.
NB: variable curvalue is dynamically scoped (used in proof-assistant-format).")

(defun proof-assistant-format-bool (value)
  (if value proof-assistant-true-value proof-assistant-false-value))

(defun proof-assistant-format-int (value)
  (funcall proof-assistant-format-int-fn value))

(defun proof-assistant-format-string (value)
  (funcall proof-assistant-format-string-fn value))

(defun proof-assistant-format (string curvalue)
  "Replace a format characters %b %i %s in STRING by formatted CURVALUE.
Formatting suitable for current proof assistant, controlled by
`proof-assistant-format-table' which see.
Finally, apply `proof-assistant-setting-format' if non-nil.
Alternatively, STRING can be a function which yields a string when applied
to the CURVALUE.
As another special case for boolean settings: the setting STRING 
can be a cons cell of two strings, the first one for true (non-nil
value) and the second for false."
  (let ((setting
	 (cond
	  ((stringp string)   ;; use % format characters
	   (proof-format proof-assistant-format-table string))
	  ((functionp string) ;; call the function
	   (funcall string curvalue))
	  ((consp string)     ;; true/false options
	   (if curvalue (car string) (cdr string)))
	  (t ;; no idea what to do
	   (error "proof-assistant-format: called with invalid string arg %s" string)))))
    (if proof-assistant-setting-format
	(funcall proof-assistant-setting-format setting)
      setting)))





(provide 'proof-menu)
;; proof-menu.el ends here.
