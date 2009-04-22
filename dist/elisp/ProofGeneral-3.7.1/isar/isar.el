; isar.el --- Major mode for Isabelle/Isar proof assistant
;; Copyright (C) 1994-2004 LFCS Edinburgh.
;;
;; License:   GPL (GNU GENERAL PUBLIC LICENSE)
;;
;; Maintainers:   David Aspinall, Makarius, Stefan Berghofer
;;
;; Authors:       David Aspinall <David.Aspinall@ed.ac.uk>
;;   	          Markus Wenzel <wenzelm@in.tum.de>
;;
;; Contributors:  David von Oheimb, Sebastian Skalberg
;;
;;
;; isar.el,v 9.5 2008/07/10 18:47:49 da Exp
;;

;;; Code:
(require 'proof)

(eval-when-compile
  (require 'span)
  (require 'proof-syntax)
  (proof-ready-for-assistant 'isar))	; compile for isar

(require 'isabelle-system)		; system code
(require 'isar-find-theorems)		; "Find Theorems" search form

;;
;; Load syntax
;;
(defcustom isar-keywords-name nil
  "Specifies a theory-specific keywords setting to use with Isar.
See -k option for Isabelle interface script."
  :type 'string
  :group 'isabelle-isar)

(or (featurep 'isar-keywords)
    ;; Pickup isar-keywords file from somewhere appropriate,
    ;; giving user chance to set name of file, or based on
    ;; name of logic.
    (isabelle-load-isar-keywords
     (or isar-keywords-name
	 isabelle-chosen-logic)))
(require 'isar-syntax)


;; Completion table for Isabelle/Isar identifiers
(defpgdefault completion-table isar-keywords-major)

(defcustom isar-web-page
  "http://isabelle.in.tum.de/Isar/"
  "URL of web page for Isabelle/Isar."
  :type 'string
  :group 'isabelle-isar)


;; Adjust toolbar entries (must be done before proof-toolbar is loaded).

(eval-after-load "pg-custom"
  '(setq isar-toolbar-entries
	 (remassoc 'qed (remassoc 'goal isar-toolbar-entries))))


(defun isar-strip-terminators ()
  "Remove explicit Isabelle/Isar command terminators `;' from the buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (proof-search-forward ";" (point-max) t)
      (if (not (proof-buffer-syntactic-context))
	  (progn
	    (delete-backward-char 1)
	    (or (proof-looking-at ";\\|\\s-\\|$")
		(insert " ")))))))


(defun isar-markup-ml (string)
  "Return marked up version of ML command STRING for Isar."
  (format "ML_command {* %s *};" string))


(defun isar-mode-config-set-variables ()
  "Configure generic proof scripting mode variables for Isabelle/Isar."
  (setq
   proof-assistant-home-page    isar-web-page
   proof-guess-command-line     'isabelle-set-prog-name
   proof-prog-name-guess	t

   ;; proof script syntax
   proof-terminal-char          ?\;     ; forcibly ends a command
   proof-script-command-start-regexp
   (proof-regexp-alt
    ;; FIXME: this gets { and } wrong: they must _not_ appear as {* *}
    ;; because that's lexically a kind of comment.
    isar-any-command-regexp
    (regexp-quote ";"))
   proof-script-integral-proofs t
   ;; FIXME: use old parser for now to avoid { } problem
   proof-script-use-old-parser  t
   proof-script-comment-start          isar-comment-start
   proof-script-comment-end            isar-comment-end
   proof-script-comment-start-regexp   isar-comment-start-regexp
   proof-script-comment-end-regexp     isar-comment-end-regexp
   proof-string-start-regexp    isar-string-start-regexp
   proof-string-end-regexp      isar-string-end-regexp

   ;; Next few used for func-menu and recognizing goal..save regions in
   ;; script buffer.
   proof-save-command-regexp    isar-save-command-regexp
   proof-goal-command-regexp    isar-goal-command-regexp
   proof-goal-with-hole-regexp  isar-named-entity-regexp
   proof-goal-with-hole-result	2
   proof-save-with-hole-regexp  nil
   proof-script-next-entity-regexps isar-next-entity-regexps
   proof-script-imenu-generic-expression isar-generic-expression
   imenu-syntax-alist isar-script-syntax-table-alist

   proof-indent-enclose-offset  (- proof-indent)
   proof-indent-open-offset     0
   proof-indent-close-offset    0
   proof-indent-any-regexp      isar-indent-any-regexp
;   proof-indent-inner-regexp    isar-indent-inner-regexp
   proof-indent-enclose-regexp  isar-indent-enclose-regexp
   proof-indent-open-regexp     isar-indent-open-regexp
   proof-indent-close-regexp    isar-indent-close-regexp

   ;; proof engine commands
   proof-showproof-command      "pr"
   proof-goal-command           "lemma \"%s\""
   proof-save-command           "qed"
   proof-context-command        "print_context"
   proof-info-command           "welcome"
   proof-kill-goal-command      "ProofGeneral.kill_proof"
;; da: TODO: we should set this from our own setting, not suggest 
;; people customize prover-configuration variables
;   proof-find-theorems-command  "find_theorems %s"               ;; minibuffer
   proof-find-theorems-command  'isar-find-theorems-minibuffer ;; equivalent
;   proof-find-theorems-command  'isar-find-theorems-form        ;; search form
   proof-shell-start-silent-cmd "disable_pr"
   proof-shell-stop-silent-cmd  "enable_pr"
   proof-shell-trace-output-regexp  "\375\\|\^AV"
   ;; command hooks
   proof-goal-command-p         'isar-goal-command-p
   proof-really-save-command-p  'isar-global-save-command-p
   proof-count-undos-fn         'isar-count-undos
   proof-find-and-forget-fn     'isar-find-and-forget
   proof-state-preserving-p     'isar-state-preserving-p
   proof-shell-compute-new-files-list 'isar-shell-compute-new-files-list
   ;; span menu
   proof-script-span-context-menu-extensions 'isabelle-create-span-menu))

(defun isar-shell-mode-config-set-variables ()
  "Configure generic proof shell mode variables for Isabelle/Isar."
  (setq
   pg-topterm-regexp			"\376\\|\^AW"

   pg-topterm-goalhyplit-fn		'isar-goalhyplit-test
   proof-shell-wakeup-char              nil
   proof-shell-annotated-prompt-regexp  "^\\w*[>#] \372\\|^\\w*[>#] \^AS"

   ;; just for comint.
   proof-shell-prompt-pattern   "^\\w*[>#] "

   ;; for issuing command, not used to track cwd in any way.
   proof-shell-cd-cmd		(isar-markup-ml "ThyLoad.add_path \"%s\"")

   ;; Escapes for filenames inside ML strings.
   ;; We also make a hack for a bug in Isabelle, by switching from
   ;; backslashes to forward slashes if it looks like we're running
   ;; on Windows.
   proof-shell-filename-escapes
   (if (fboundp 'win32-long-filename)   ; rough test for XEmacs on win32
       ;; Patterns to unixfy names.
       ;; Jacques Fleuriot's patch in ML does this too: ("^[a-zA-Z]:" . "")
       ;; But I'll risk leaving drive names in, not sure how to replace them.
       '(("\\\\" . "/") ("\"" . "\\\""))
     ;; Normal case: quotation for backslash, quote mark.
     '(("\\\\" . "\\\\") ("\""   . "\\\"")))

   proof-shell-proof-completed-regexp   nil     ; n.a.
   proof-shell-interrupt-regexp         "\364\\*\\*\\* Interrupt\\|\^AM\\*\\*\\* Interrupt"
   proof-shell-error-regexp             "\364\\*\\*\\*\\|\^AM\\*\\*\\*"
   proof-shell-abort-goal-regexp        nil     ; n.a.

   pg-next-error-regexp	  "\\((line \\([0-9]+\\) of \"[^\"]+\")\\)"
   pg-next-error-filename-regexp "\\((line [0-9]+ of \"\\([^\"]+\\)\")\\)"

   ;; matches names of assumptions
   proof-shell-assumption-regexp        isar-id

   proof-shell-start-goals-regexp       "\366\n\\|\^AO\n"
   proof-shell-end-goals-regexp         "\367\\|\^AP"

   proof-shell-init-cmd			nil
   proof-shell-restart-cmd              "ProofGeneral.restart"

   proof-shell-eager-annotation-start-length 2
   proof-shell-eager-annotation-start   "\360\\|\362\\|\^AI\\|\^AK"
   proof-shell-eager-annotation-end     "\361\\|\363\\|\^AJ\\|\^AL"

   ;; Isabelle is learning to talk PGIP...
   proof-shell-match-pgip-cmd		"<pgip"
   proof-shell-issue-pgip-cmd		'isabelle-process-pgip

   ;; Some messages delimited by eager annotations
   proof-shell-clear-response-regexp    "Proof General, please clear the response buffer."
   proof-shell-clear-goals-regexp       "Proof General, please clear the goals buffer."

   ;; Theorem dependencies.   NB: next regex anchored at end with eager annot end
   proof-shell-theorem-dependency-list-regexp "Proof General, theorem dependencies of \\(.*\\) are \"\\(.*\\)\"\\(\361\\|\^AJ\\)"
   proof-shell-theorem-dependency-list-split "\" \""
   proof-shell-show-dependency-cmd "thm %s;"
   proof-shell-identifier-under-mouse-cmd
   '((nil     "thm %s;")
     (string  "term \"%s\";")
     (comment "term \"%s\";"))

   ;; Allow font-locking for output based on hidden annotations, see
   ;; isar-output-font-lock-keywords-1
   pg-use-specials-for-fontify		t
   pg-after-fontify-output-hook	        'pg-remove-specials

   pg-special-char-regexp
   (if proof-shell-unicode "[0-9A-Z]"
     ;; next string could be: "[\350-\377]", but that's buggy with XEmacs 21.5 (beta)
     "�\\|�\\|�\\|�\\|�\\|�\\|�\\|�\\|�\\|8\\|�\\|�\\|�\\|�\\|�\\|�\\|�\\|�\\|�\\|�\\|�\\|�\\|�\\|�\\|�\\|�\\|�\\|�\\|�\\|�\\|�\\|�\\|�\\|�")

   pg-subterm-help-cmd			"term %s"

   proof-cannot-reopen-processed-files  t
   proof-shell-process-file
   (cons
    ;; Theory loader output
    "Proof General, this file is loaded: \"\\(.*\\)\""
    (lambda (str)
      (match-string 1 str)))
   proof-shell-retract-files-regexp
   "Proof General, you can unlock the file \"\\(.*\\)\""
   proof-shell-compute-new-files-list 'isar-shell-compute-new-files-list
   proof-shell-inform-file-processed-cmd "ProofGeneral.inform_file_processed \"%s\""
   proof-shell-inform-file-retracted-cmd "ProofGeneral.inform_file_retracted \"%s\""))


;;;
;;; Theory loader operations
;;;

(defun isar-remove-file (name files cmp-base)
  (if (not files) nil
    (let*
        ((file (car files))
         (rest (cdr files))
         (same (if cmp-base (string= name (file-name-nondirectory file))
                 (string= name file))))
      (if same (isar-remove-file name rest cmp-base)
        (cons file (isar-remove-file name rest cmp-base))))))

(defun isar-shell-compute-new-files-list (str)
  "Compute the new list of files read by the proof assistant.
This is called when Proof General spots output matching
proof-shell-retract-files-regexp."
  (let*
      ((name (match-string 1 str))
       (base-name (file-name-nondirectory name)))
    (if (string= name base-name)
        (isar-remove-file name proof-included-files-list t)
      (isar-remove-file (file-truename name) proof-included-files-list nil))))


;;
;;   Define the derived modes
;;
(eval-and-compile
(define-derived-mode isar-shell-mode proof-shell-mode
   "Isar shell" nil
   (isar-shell-mode-config)))

(eval-and-compile
(define-derived-mode isar-response-mode proof-response-mode
  "response" nil
  (isar-response-mode-config)))

(eval-and-compile                       ; to define vars for byte comp.
(define-derived-mode isar-goals-mode proof-goals-mode
  "proofstate" nil
  (isar-goals-mode-config)))

(eval-and-compile                       ; to define vars for byte comp.
(define-derived-mode isar-mode proof-mode
  "Isar script" 
  "Major mode for editing Isar proof scripts.

\\{isar-mode-map}"
  (isar-mode-config)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Code that's Isabelle/Isar specific                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; help menu
;;;

;;; da: how about a `C-c C-a h ?' for listing available keys?

;;; NB: definvisible must come after derived modes because uses
;;; isar-mode-map
(proof-definvisible isar-help-antiquotations "print_antiquotations" [h A])
(proof-definvisible isar-help-attributes "print_attributes" [h a])
(proof-definvisible isar-help-cases "print_cases" [h c])
(proof-definvisible isar-help-claset "print_claset" [h C])
(proof-definvisible isar-help-commands "print_commands" [h o])
(proof-definvisible isar-help-facts "print_facts" [h f])
(proof-definvisible isar-help-syntax "print_syntax" [h i])
(proof-definvisible isar-help-induct-rules "print_induct_rules" [h I])
(proof-definvisible isar-help-methods "print_methods" [h m])
(proof-definvisible isar-help-simpset "print_simpset" [h S])
(proof-definvisible isar-help-binds "print_binds" [h b])
(proof-definvisible isar-help-theorems "print_theorems" [h t])
(proof-definvisible isar-help-trans-rules "print_trans_rules" [h T])

;;
;; Command menu
;;

;; NB: would be nice to query save of the buffer first for these
;; next two: but only convenient emacs functions offer save for
;; all buffers.
(proof-definvisible isar-cmd-display-draft
 '(format "display_drafts \"%s\"" buffer-file-name)
 [(control d)])

(proof-definvisible isar-cmd-print-draft
  '(if (y-or-n-p
	(format "Print draft of file %s ?" buffer-file-name))
       (format "print_drafts \"%s\"" buffer-file-name)
     (error "Aborted."))
  [(control p)])

(proof-definvisible isar-cmd-refute	"refute" [r])
(proof-definvisible isar-cmd-quickcheck "quickcheck" [(control q)])
(proof-definvisible isar-cmd-sledgehammer "sledgehammer" [(control s)])

(defpgdefault menu-entries
  (append
   (list isabelle-logics-menu)
   (list
    (cons "Commands"
          (list
           ["refute"             isar-cmd-refute         t]
           ["quickcheck"         isar-cmd-quickcheck     t]
           ["sledgehammer"       isar-cmd-sledgehammer   t]
	   ["display draft"	 isar-cmd-display-draft  t])))
   (list
    (cons "Show me ..."
          (list
           ["cases"              isar-help-cases          t]
           ["facts"              isar-help-facts          t]
           ["term bindings"      isar-help-binds          t]
	   "----"
           ["classical rules"    isar-help-claset         t]
           ["induct/cases rules" isar-help-induct-rules   t]
           ["simplifier rules"   isar-help-simpset        t]
           ["theorems"           isar-help-theorems       t]
           ["transitivity rules" isar-help-trans-rules    t]
	   "----"
           ["antiquotations"     isar-help-antiquotations t]
           ["attributes"         isar-help-attributes     t]
           ["commands"           isar-help-commands       t]
           ["inner syntax"       isar-help-syntax         t]
           ["methods"            isar-help-methods        t])))))

;; undo proof commands
(defun isar-count-undos (span)
  "Count number of undos SPAN, return command needed to undo that far."
  (let
      ((case-fold-search nil)       ;FIXME ??
       (ct 0) str i)
    (while span
      (setq str (or (span-property span 'cmd) ""))
      (cond ((eq (span-property span 'type) 'vanilla)
             (or (proof-string-match isar-undo-skip-regexp str)
		 (proof-string-match isar-undo-ignore-regexp str)
                 (setq ct (+ 1 ct))))
            ((eq (span-property span 'type) 'pbp)
             ;; this case for automatically inserted text (e.g. sledgehammer)
             (cond ((not (proof-string-match isar-undo-skip-regexp str))
		    (setq ct 1)
                    (setq i 0)
		    ;; If we find a semicolon, assume several commands,
		    ;; and increment the undo count.
                    (while (< i (length str))
                      (if (= (aref str i) proof-terminal-char)
                          (setq ct (+ 1 ct)))
                      (setq i (+ 1 i))))
                   (t nil))))
      (setq span (next-span span 'type)))
    (isar-undos ct)))

;; undo theory commands
(defun isar-find-and-forget (span)
  "Return commands to be used to forget SPAN."
  (let (str ans answers)
    (while span
      (setq str (or (span-property span 'cmd) ""))
      (setq ans nil)
      (cond
       ;; comment, diagnostic, nested proof command: skip
       ;; (da: adding new span types may break this code,
       ;;  ought to test for type 'cmd before looking at
       ;;  str below)
       ;; FIXME: should adjust proof-nesting-depth here.
       ((or (eq (span-property span 'type) 'comment)
	    (eq (span-property span 'type) 'proverproc)
	    (eq (span-property span 'type) 'proof); da: needed?
            (proof-string-match isar-undo-skip-regexp str)
            (proof-string-match isar-undo-ignore-regexp str)))
       ;; finished goal: undo
       ((eq (span-property span 'type) 'goalsave)
        (setq ans isar-undo))
       ;; open goal: skip and exit
       ((proof-string-match isar-goal-command-regexp str)
        (setq span nil))
       ;; control command: cannot undo
       ((and (proof-string-match isar-undo-fail-regexp str))
        (setq ans (isar-cannot-undo (match-string 1 str)))
        (setq answers nil)
        (setq span nil))
       ;; theory: remove and exit
       ((proof-string-match isar-undo-remove-regexp str)
        (setq ans (isar-remove (match-string 2 str)))
        (setq span nil))
       ;; else: undo
       (t
        (setq ans isar-undo)))
      (if ans (setq answers (cons ans answers)))
      (if span (setq span (next-span span 'type))))
    (if (null answers) proof-no-command (apply 'concat answers))))

(defun isar-goal-command-p (span)
  "Decide whether argument SPAN is a goal or not."
  (proof-string-match isar-goal-command-regexp
		      (or (span-property span 'cmd) "")))

(defun isar-global-save-command-p (span str)
  "Decide whether argument SPAN with command STR is a global save command."
  (or
   (proof-string-match isar-global-save-command-regexp str)
   (let ((ans nil) (lev 0) cmd)
     (while (and (not ans) span (setq span (prev-span span 'type)))
       (setq cmd (or (span-property span 'cmd) ""))
       (cond
        ;; comment: skip
        ((eq (span-property span 'type) 'comment))
        ;; local qed: enter block
        ((proof-string-match isar-save-command-regexp cmd)
         (setq lev (+ lev 1)))
        ;; local goal: leave block, or done
        ((proof-string-match isar-local-goal-command-regexp cmd)
         (if (> lev 0) (setq lev (- lev 1)) (setq ans 'no)))
        ;; global goal: done
        ((proof-string-match isar-goal-command-regexp cmd)
         (setq ans 'yes))))
     (eq ans 'yes))))

(defvar isar-current-goal 1
  "Last goal that Emacs looked at.")

(defun isar-state-preserving-p (cmd)
  "Non-nil if command CMD preserves the proofstate."
  (proof-string-match isar-undo-skip-regexp cmd))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Commands specific to isar                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(proof-defshortcut isar-bold      "\\<^bold>%p" [(control b)])
(proof-defshortcut isar-local     "\\<^loc>%p" [(control c)])
(proof-defshortcut isar-super     "\\<^sup>%p" [(control u)])
(proof-defshortcut isar-sub       "\\<^sub>%p" [(control l)])
(proof-defshortcut isar-longsuper "\\<^bsup>%p\\<^esup>" [?u])
(proof-defshortcut isar-longsub   "\\<^bsub>%p\\<^esub>" [?l])
(proof-defshortcut isar-idsub     "\\<^isub>%p" [(control i)])
(proof-defshortcut isar-raw       "\\<^raw:%p>" [(control r)])
(proof-defshortcut isar-antiquote "@{text \"%p\"}" [(control a)])
(proof-defshortcut isar-ml	  "ML {* %p *}" [(control x)])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Isar shell startup and exit hooks                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar isar-shell-current-line-width nil
  "Current line width of the Isabelle process's pretty printing module.
Its value will be updated whenever the corresponding screen gets
selected.")

(defun isar-shell-adjust-line-width ()
  "Use Isabelle's pretty printing facilities to adjust output line width.
Checks the width in the `proof-goals-buffer'"
  (let ((ans ""))
    (and (buffer-live-p proof-goals-buffer)
         (proof-shell-live-buffer)
         (save-excursion
           (set-buffer proof-goals-buffer)
           (let ((current-width
                  ;; Actually, one might want the width of the
                  ;; proof-response-buffer instead. Never mind.
                  (max 20 (window-width (get-buffer-window proof-goals-buffer t)))))

             (if (equal current-width isar-shell-current-line-width) ()
               (setq isar-shell-current-line-width current-width)
               (set-buffer proof-shell-buffer)
               (setq ans (format "pretty_setmargin %d;" (- current-width 4)))))))
    ans))

;;
;;   Configuring proof output buffer
;;

(defun isar-preprocessing ()  ;dynamic scoping of `string'
  "Insert sync markers - acts on variable STRING by dynamic scoping."
  (if (proof-string-match isabelle-verbatim-regexp string)
      (setq string (match-string 1 string))
    (unless (proof-string-match ";[ \t]*\\'" string)
      (setq string (concat string ";")))
    (setq string (concat
                  "\\<^sync>"
                  (isar-shell-adjust-line-width)
                  ;; da: this was an attempted hack to deal with ML files,
                  ;; unfortunately Isar complains about not seeing a theory
                  ;; command first: ML_setup illegal at first line
                  ;; Another failure is that: (* comment *) foo;
                  ;; ignores foo.  This could be fixed by scanning for
                  ;; comment end in proof-script.el's function
                  ;; proof-segment-upto-cmdstart (which becomes even more
                  ;; Isar specific, then...)
                  ;; (if (proof-string-match "\\.ML$" (buffer-name proof-script-buffer))
                  ;;    (format "ML_command {* %s *};" string)
                  ;;    string)
                  (proof-replace-regexp-in-string "\n" "\\\\<^newline>" string)
                  " \\<^sync>;"))))

(defun isar-mode-config ()
  (isar-mode-config-set-variables)
  (isar-init-syntax-table)
  (setq font-lock-keywords isar-font-lock-keywords-1)
  (setq comment-quote-nested nil) ;; can cope with nested comments
  (set (make-local-variable 'outline-regexp) isar-outline-regexp)
  (set (make-local-variable 'outline-heading-end-regexp) isar-outline-heading-end-regexp)
  (setq blink-matching-paren-dont-ignore-comments t)
  (add-hook 'proof-shell-insert-hook 'isar-preprocessing)
  (proof-config-done))

(defun isar-shell-mode-config ()
  "Configure Proof General proof shell for Isabelle/Isar."
  (isar-init-output-syntax-table)
  (setq font-lock-keywords
	(append
	 isar-output-font-lock-keywords-1
	 (if (boundp 'x-symbol-isar-font-lock-keywords)
	     x-symbol-isar-font-lock-keywords)))
  (isar-shell-mode-config-set-variables)
  (proof-shell-config-done))

(defun isar-response-mode-config ()
  (isar-init-output-syntax-table)
  (setq font-lock-keywords
	(append
	 isar-output-font-lock-keywords-1
	 (if isar-x-symbol-enable
	     x-symbol-isar-font-lock-keywords)))
  (proof-response-config-done))

(defun isar-goals-mode-config ()
  (setq pg-goals-change-goal "prefer %s")
  (setq pg-goals-error-regexp proof-shell-error-regexp)
  (isar-init-output-syntax-table)
  (setq font-lock-keywords
	(append
	 isar-goals-font-lock-keywords
	 (if isar-x-symbol-enable
	     x-symbol-isar-font-lock-keywords)))
  (proof-goals-config-done))

(defun isar-goalhyplit-test ()
  "Value for `pg-topterm-goalhyplit-fn' (see that variable for documentation)."
  (let ((start (point)))
    (if (proof-re-search-forward
	 "\377\\|\^AX" nil t) ; end of literal command (non-standard)
	(progn
	  (delete-region (match-beginning 0) (match-end 0))
	  (cons 'lit (buffer-substring start (match-beginning 0)))))))


(provide 'isar)

;;; isar.el ends here
