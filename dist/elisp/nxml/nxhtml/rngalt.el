;;; rngalt.el --- Tools for making completion addition to nxml mode
;;
;; Author: Lennart Borgman
;; Created: Wed Jan 10 17:17:18 2007
;; Version:
;; Lxast-Updated: Wed Jan 10 17:18:03 2007 (3600 +0100)
;; Keywords:
;; Compatibility:
;;
;; Fxeatures that might be required by this library:
;;
;;   Cannot open load file: rngalt.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defvar rngalt-complete-first-try nil
  "First function to try for completion.
If non-nil should be a function with no parameters.  Used by
`rngalt-complete'.")

(eval-when-compile
  (unless (featurep 'nxml-nxhtml-autostart)
    (let ((efn (expand-file-name "../autostart.el")))
      (load efn))
    (require 'rng-valid)
    (require 'rng-nxml)
    ))

(defvar rngalt-complete-last-try nil
  "Last function to try for completion.
If non-nil should be a function with no parameters.  Used by
`rngalt-complete'.")

(defvar rngalt-completing-read-tag nil
  "Alternate function for completing tag name.
If non-nil should be a function with the same parameters as
`completing-read'.  Used by `rngalt-complete'.")

(defvar rngalt-completing-read-attribute-name nil
  "Alternate function for completing attribute name.
If non-nil should be a function with the same parameters as
`completing-read'.  Used by `rngalt-complete'.")

(defvar rngalt-completing-read-attribute-value nil
  "Alternate function for completing attribute value.
If non-nil should be a function with the same parameters as
`completing-read'.  Used by `rngalt-complete'.")

(defun rngalt-complete ()
  "Complete the string before point using the current schema.
Return non-nil if in a context it understands.

This function should be added to `nxml-completion-hook' before
`rng-complete'. By default it works just like this function, but
you can add your own completion by setting the variables
`rngalt-complete-first-try', `rngalt-completing-read-tag',
`rngalt-completing-read-attribute-name',
`rngalt-completing-read-attribute-value' and
`rngalt-complete-last-try'."
  (interactive)
  (or (when rngalt-complete-first-try
        (funcall rngalt-complete-first-try))
      (and rng-validate-mode
           (let ((lt-pos (save-excursion (search-backward "<" nil t)))
                 xmltok-dtd)
             (and lt-pos
                  (= (rng-set-state-after lt-pos) lt-pos)
                  (or (rngalt-complete-tag lt-pos)
                      (rng-complete-end-tag lt-pos)
                      (rngalt-complete-attribute-name lt-pos)
                      (rngalt-complete-attribute-value lt-pos)
                      (when rngalt-complete-last-try
                        (funcall rngalt-complete-last-try))))))))

(defun rngalt-validate()
  (condition-case err
      (while (rng-do-some-validation) nil)
    (error nil))
  (rng-validate-done))

(defvar rngalt-region-ovl nil)
(defvar rngalt-region-prepared nil)
(defun rngalt-complete-tag-region-prepare()
  (unless rngalt-region-prepared
    (when rngalt-region-ovl
      (when (overlayp rngalt-region-ovl)
        (delete-overlay rngalt-region-ovl))
      (setq rngalt-region-ovl nil))
    (when (and mark-active
               transient-mark-mode)
      (let ((beginning (region-beginning))
            (end       (region-end)))
        (unless (= (point) (region-beginning))
          (goto-char beginning))
        (when (save-excursion
                (when (re-search-forward "\\=[^<]*\\(?:<[^<]*>\\)*[^>]*" end t)
                  (= end (point))))
          (setq rngalt-region-ovl (make-overlay beginning end))
          (overlay-put rngalt-region-ovl 'face 'region)
          )))
    (setq rngalt-region-prepared t)))

(defun rngalt-complete-tag-region-cleanup()
  (when rngalt-region-prepared
    (when (overlayp rngalt-region-ovl)
      (delete-overlay rngalt-region-ovl))
    (deactivate-mark)
    (setq rngalt-region-prepared nil)))

(defun rngalt-complete-tag-region-finish()
  (when (and rngalt-region-prepared
             (overlayp rngalt-region-ovl))
    (let ((here (point)))
      (insert ">")
      (goto-char (overlay-end rngalt-region-ovl))
      (nxml-finish-element)
      (rngalt-validate)
      (goto-char here)))
  (rngalt-complete-tag-region-cleanup))

(defun rngalt-complete-tag (lt-pos)
  "Like `rng-complete-tag' but with some additions.
The additions are:
- Alternate completion.
- Complete around highlighted region.

See also the variable `rngalt-completing-read-tag'."
  (let (rng-complete-extra-strings)
    (when (and (= lt-pos (1- (point)))
	       rng-complete-end-tags-after-<
	       rng-open-elements
	       (not (eq (car rng-open-elements) t))
	       (or rng-collecting-text
		   (rng-match-save
		     (rng-match-end-tag))))
      (setq rng-complete-extra-strings
	    (cons (concat "/"
			  (if (caar rng-open-elements)
			      (concat (caar rng-open-elements)
				      ":"
				      (cdar rng-open-elements))
			    (cdar rng-open-elements)))
		  rng-complete-extra-strings)))
    (when (save-excursion
	    (re-search-backward rng-in-start-tag-name-regex
				lt-pos
				t))
      (and rng-collecting-text (rng-flush-text))
      (rngalt-complete-tag-region-prepare)
      (let ((completion
	     (let ((rng-complete-target-names
		    (rng-match-possible-start-tag-names))
		   (rng-complete-name-attribute-flag nil))
	       (rngalt-complete-before-point (1+ lt-pos)
                                             'rng-complete-qname-function
                                             "Insert tag: "
                                             nil
                                             'rng-tag-history
                                             rngalt-completing-read-tag)))
	    name)
	(when completion
	  (cond ((rng-qname-p completion)
		 (setq name (rng-expand-qname completion
					      t
					      'rng-start-tag-expand-recover))
		 (when (and name
			    (rng-match-start-tag-open name)
			    (or (not (rng-match-start-tag-close))
				;; need a namespace decl on the root element
				(and (car name)
				     (not rng-open-elements))))
		   ;; attributes are required
		   (insert " "))
                 (rngalt-complete-tag-region-finish)
                 (run-hook-with-args 'rngalt-complete-tag-hooks completion)
                 )
		((member completion rng-complete-extra-strings)
		 (insert ">")))))
      (rngalt-complete-tag-region-finish)
      t)))

(defvar rngalt-complete-tag-hooks nil
  "Hook run after completing a tag.
Each function is called with the last name of the last tag
completed.")

(defun rngalt-complete-attribute-name (lt-pos)
  "Like `rng-complete-attribute-name' but with alternate completion.
See the variable `rngalt-completing-read-attribute-name'."
  (when (save-excursion
	  (re-search-backward rng-in-attribute-regex lt-pos t))
    (let ((attribute-start (match-beginning 1))
	  rng-undeclared-prefixes)
      (and (rng-adjust-state-for-attribute lt-pos
					   attribute-start)
	   (let ((rng-complete-target-names
		  (rng-match-possible-attribute-names))
		 (rng-complete-extra-strings
		  (mapcar (lambda (prefix)
			    (if prefix
				(concat "xmlns:" prefix)
			      "xmlns"))
			  rng-undeclared-prefixes))
		 (rng-complete-name-attribute-flag t)
                 completion)
             (setq completion
                   (rngalt-complete-before-point attribute-start
                                                 'rng-complete-qname-function
                                                 "Attribute: "
                                                 nil
                                                 'rng-attribute-name-history
                                                 rngalt-completing-read-attribute-name))
             (when (and completion
                        (< 0 (length completion)))
               (insert "=\"")))))
    t))

(defun rngalt-complete-attribute-value (lt-pos)
  "Like `rng-complete-attribute-value' but with alternate completion.
See the variable `rngalt-completing-read-attribute-value'."
  (when (save-excursion
	  (re-search-backward rng-in-attribute-value-regex lt-pos t))
    (let ((name-start (match-beginning 1))
	  (name-end (match-end 1))
	  (colon (match-beginning 2))
	  (value-start (1+ (match-beginning 3))))
      (and (rng-adjust-state-for-attribute lt-pos
					   name-start)
	   (if (string= (buffer-substring-no-properties name-start
							(or colon name-end))
			"xmlns")
	       (rngalt-complete-before-point
		value-start
		(rng-strings-to-completion-alist
		 (rng-possible-namespace-uris
		  (and colon
		       (buffer-substring-no-properties (1+ colon) name-end))))
		"Namespace URI: "
		nil
		'rng-namespace-uri-history
                rngalt-completing-read-attribute-value) ;; fix-me
	     (rng-adjust-state-for-attribute-value name-start
						   colon
						   name-end)
	     (rngalt-complete-before-point
	      value-start
	      (rng-strings-to-completion-alist
	       (rng-match-possible-value-strings))
	      "Value: "
	      nil
	      'rng-attribute-value-history
              rngalt-completing-read-attribute-value))
           (unless (eq (char-after) (char-before value-start))
             (insert (char-before value-start)))))
    t))

(defun rngalt-complete-before-point (start table prompt &optional predicate hist altcompl)
  "Complete text between START and point.
Works like `rng-complete-before-point' if ALTCOMPL is nil.  When
ALTCOMPL is a function symbol and no completion alternative is
available from table then this is called instead of
`compleating-read' with the same parameters."
  (let* ((orig (buffer-substring-no-properties start (point)))
	 (completion (try-completion orig table predicate))
         (completing-fun (if altcompl altcompl 'completing-read)))
    (cond ((not (or completion completing-fun))
	   (if (string= orig "")
	       (message "No completions available")
	     (message "No completion for %s" (rng-quote-string orig)))
	   (ding)
	   nil)
	  ((eq completion t) orig)
	  ((and completion
                (not (string= completion orig)))
	   (delete-region start (point))
	   (insert completion)
	   (cond ((not (rng-completion-exact-p completion table predicate))
		  (message "Incomplete")
		  nil)
		 ((eq (try-completion completion table predicate) t)
		  completion)
		 (t
		  (message "Complete but not unique")
		  nil)))
	  (t
	   (setq completion
		 (let ((saved-minibuffer-setup-hook
			(default-value 'minibuffer-setup-hook)))
		   (add-hook 'minibuffer-setup-hook
			     'minibuffer-completion-help
			     t)
		   (unwind-protect
		       (funcall completing-fun
                                prompt
                                table
                                predicate
                                nil
                                orig
                                hist)
		     (setq-default minibuffer-setup-hook
				   saved-minibuffer-setup-hook))))
           (when completion
             (delete-region start (point))
             (insert completion))
	   completion))))

(defun rngalt-get-missing-required-attr(single-tag)
  "Get a list of missing required attributes.
This is to be used when completing attribute names.
SINGLE-TAG should be non-nil if the tag has no end tag.

For a typical use see `nxhtml-completing-read-attribute-name' in
nxhtml.el.
"
  ;; FIX-ME: This is a terrible cludge. One day I hope I will
  ;; understand how to write this ;-)
  ;;
  ;; I currently fetch the missing tags from the error message in the
  ;; error overlay set by rng validate.
  (let ((here (point)))
    (unless (save-match-data (looking-at "[^<]\\{,200\\}>"))
      ;; We can probably add a >, so let us do it:
      (when single-tag
        (insert "/"))
      (insert ">")
      (rngalt-validate))
    (goto-char here))
  (let ((ovl (rng-error-overlay-message (or (rng-error-overlay-after (point)) 
                                            (rng-error-overlay-after (1- (point)))))))
    ;;(message "ovl=%s" ovl)(sit-for 1)
    ;;(message "prop ovl=%s" (overlay-properties ovl))(sit-for 1)
    (when (and ovl
               (eq (overlay-get ovl 'category) 'rng-error))
      ;;(message "rng-error")(sit-for 1)
      (let ((msg (overlay-get ovl 'help-echo)))
        ;;(message "msg=%s" msg);(sit-for 1)
        (when (string-match "Missing attributes? \\(.*\\)" msg)
          ;;(message "0=%s" (match-string 0 msg));(sit-for 1)
          ;;(message "1=%s" (match-string 1 msg));(sit-for 1)
          (let* ((matches (match-string 1 msg))
                 (lst (split-string (substring matches 1 (- (length matches) 1)) "\", \"")))
            ;;(message "matches=%s" matches);(sit-for 2)
            ;;(message "lst=%s" lst);(sit-for 1)
            lst))))))




(provide 'rngalt)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rngalt.el ends here
