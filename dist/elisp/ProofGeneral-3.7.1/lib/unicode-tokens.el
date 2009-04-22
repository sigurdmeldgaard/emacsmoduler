;;; unicode-tokens.el --- Support for editing tokens for Unicode characters
;;
;; Copyright(C) 2008 David Aspinall / LFCS Edinburgh
;; Author:    David Aspinall <David.Aspinall@ed.ac.uk>
;; License:     GPL (GNU GENERAL PUBLIC LICENSE)
;;
;; unicode-tokens.el,v 9.19 2008/07/19 12:49:14 da Exp
;;
;; A few functions are adapted from `xmlunicode.el' by Norman Walsh.
;; Created: 2004-07-21, Version: 1.6, Copyright (C) 2003 Norman Walsh
;;
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This is a partial replacement for X-Symbol for Proof General.
;; STATUS: experimental.  
;;
;; Functions to help insert tokens that represent Unicode characters
;; and control code sequences for changing the layout.  Character
;; tokens are useful for programs that do not understand a Unicode
;; encoding.
;; 

;; TODO:
;; -- saving of font-lock-face annotations unreliable, possible confusion 
;;    over handling of lists in format.el
;; -- add input methods for subscript/superscripts (further props in general)
;; -- after change function so inserting control sequences works? or other support
;; -- one-char subs should not be sticky so doesn't extend
;; -- make property removal more accurate/patch in font-lock
;; -- disentangle Isabelle specific code
;; -- perhaps separate out short-cut input method and don't use for tokens
;; -- cleanup insertion functions
;; -- investigate testing for an appropriate glyph

(require 'cl)

(require 'unicode-chars)		; list of Unicode characters

;;
;; Variables that should be set
;; (default settings are for XML, but configuration incomplete; 
;;  use xmlunicode.el instead)
;;

(defvar unicode-tokens-charref-format "&#x%x;"
  "The format for numeric character references")

(defvar unicode-tokens-token-format "&%x;"
  "The format for token character references")

(defvar unicode-tokens-token-name-alist nil
  "Mapping of token names to Unicode strings.")

(defvar unicode-tokens-glyph-list nil
  "List of available glyphs, as characters.
If not set, constructed to include glyphs for all tokens. ")

(defvar unicode-tokens-token-prefix "&"
  "Prefix for start of tokens to insert.")

(defvar unicode-tokens-token-suffix ";"
  "Suffix for end of tokens to insert.")

(defvar unicode-tokens-control-token-match nil
  "Regexp matching tokens")

(defvar unicode-tokens-token-match "&\\([A-Za-z]\\);"
  "Regexp matching tokens")

(defvar unicode-tokens-hexcode-match "&#[xX]\\([0-9a-fA-F]+\\);"
  "Regexp matching numeric token string")

(defvar unicode-tokens-next-character-regexp "&#[xX]\\([0-9a-fA-F]+\\);\\|."
  "Regexp matching a logical character following a control code.")

(defvar unicode-tokens-shortcut-alist
  "An alist of keyboard shortcuts to unicode strings.
The alist is added to the input mode for tokens.
Behaviour is much like abbrev.")

;;
;; Faces
;;

;;
;; TODO: make these into faces but extract attributes
;; to use in `unicode-tokens-annotation-translations'.
;; Let that be dynamically changeable
;; TODO: choose family acccording to likely architecture and what's available
(cond
 ((not (featurep 'xemacs))
(defface unicode-tokens-script-font-face
  (cond
   ((eq window-system 'x) ; Linux/Unix
    '((t :family "URW Chancery L")))
   ((or ; Mac
     (eq window-system 'ns)
     (eq window-system 'carbon))
    '((t :family "Lucida Calligraphy"))))
  "Script font face")

(defface unicode-tokens-fraktur-font-face
  (cond
   ((eq window-system 'x) ; Linux/Unix
    '((t :family "URW Bookman L"))) ;; not at all black letter!
   ((or ; Mac
     (eq window-system 'ns)
     (eq window-system 'carbon))
    '((t :family "Lucida Blackletter"))))
  "Fraktur font face")

(defface unicode-tokens-serif-font-face
  (cond
   ((eq window-system 'x) ; Linux/Unix
    '((t :family "Liberation Serif"))) 
   ((or ; Mac
     (eq window-system 'ns)
     (eq window-system 'carbon))
    '((t :family "Lucida"))))
  "Serif (roman) font face")))


;;
;; Variables initialised in unicode-tokens-initialise 
;;

(defvar unicode-tokens-max-token-length 10
  "Maximum length of a token in underlying encoding.")

(defvar unicode-tokens-codept-charname-alist nil
  "Alist mapping unicode code point to character names.")

(defvar unicode-tokens-token-alist nil
  "Mapping of tokens to Unicode strings.
Also used as a flag to detect if `unicode-tokens-initialise' has been called.")

(defvar unicode-tokens-ustring-alist nil
  "Mapping of Unicode strings to tokens.")


;;
;;; Code:
;;

(defun unicode-tokens-insert-char (arg codepoint)
  "Insert the Unicode character identified by CODEPOINT.
If ARG is non-nil, ignore available glyphs."
  (let ((glyph (memq codepoint unicode-tokens-glyph-list)))
    (cond
     ((and (decode-char 'ucs codepoint) (or arg glyph))
      (ucs-insert codepoint)) ;; glyph converted to token on save
     (t
      (insert (format unicode-tokens-charref-format codepoint))))))

(defun unicode-tokens-insert-string (arg ustring)
  "Insert a Unicode string.
If a prefix is given, the string will be inserted regardless
of whether or not it has displayable glyphs; otherwise, a
numeric character reference for whichever codepoints are not
in the unicode-tokens-glyph-list."
  (mapcar (lambda (char) 
	    (unicode-tokens-insert-char arg char))
	  ustring))

(defun unicode-tokens-character-insert (arg &optional argname)
  "Insert a Unicode character by character name, with completion. 
If a prefix is given, the character will be inserted regardless
of whether or not it has a displayable glyph; otherwise, a
numeric character reference is inserted if the codepoint is not
in the `unicode-tokens-glyph-list'. If argname is given, it is used for
the prompt. If argname uniquely identifies a character, that
character is inserted without the prompt."
  (interactive "P")
  (let* ((completion-ignore-case t)
	 (uniname (if (stringp argname) argname ""))
	 (charname
	  (if (eq (try-completion uniname unicode-chars-alist) t)
	      uniname
	    (completing-read
	     "Unicode name: "
	     unicode-chars-alist
	     nil t uniname)))
	 codepoint glyph)
    (setq codepoint (cdr (assoc charname unicode-chars-alist)))
    (unicode-tokens-insert-char arg codepoint)))

(defun unicode-tokens-token-insert (arg &optional argname)
  "Insert a Unicode string by a token  name, with completion. 
If a prefix is given, the string will be inserted regardless
of whether or not it has displayable glyphs; otherwise, a
numeric character reference for whichever codepoints are not
in the unicode-tokens-glyph-list. If argname is given, it is used for
the prompt. If argname uniquely identifies a character, that
character is inserted without the prompt."
  (interactive "P")
  (let* ((stokname (if (stringp argname) argname ""))
	 (tokname
	  (if (eq (try-completion stokname unicode-tokens-token-name-alist) t)
	      stokname
	    (completing-read
	     "Token name: "
	     unicode-tokens-token-name-alist
	     nil t stokname)))
	 charname ustring)
    (setq ustring (cdr (assoc tokname unicode-tokens-token-name-alist)))
    (unicode-tokens-insert-string arg ustring)))

(defun unicode-tokens-replace-token-after (length)
  (let ((bpoint (point)) ustring)
    (save-excursion
      (forward-char length)
      (save-match-data
	(while (re-search-backward 
		unicode-tokens-token-match 
		(max (- bpoint unicode-tokens-max-token-length) 
		     (point-min)) t nil)
	  (setq ustring 
		(assoc (match-string 1) unicode-tokens-token-name-alist))
	  (if ustring ;; TODO: should check on glyphs here
	      (progn
		(let ((matchlen (- (match-end 0) (match-beginning 0))))
		  (replace-match (replace-quote (cdr ustring)))
		  ;; was: (format "%c" (decode-char 'ucs (cadr codept)))
		  (setq length 
			(+ (- length matchlen) (length (cdr ustring)))))))))))
    length)


;;stolen from hen.el which in turn claims to have stolen it from cxref
(defun unicode-tokens-looking-backward-at (regexp)
  "Return t if text before point matches regular expression REGEXP.
This function modifies the match data that `match-beginning',
`match-end' and `match-data' access; save and restore the match
data if you want to preserve them."
  (save-excursion
    (let ((here (point)))
      (if (re-search-backward regexp (point-min) t)
          (if (re-search-forward regexp here t)
              (= (point) here))))))

;; TODO: make this work for control tokens.  
;; But it's a bit nasty and introduces font-lock style complexity again.
;; Better if we stick with dedicated input methods.
(defun unicode-tokens-electric-suffix ()
  "Detect tokens and replace them with the appropriate string.
This is bound to the character ending `unicode-tokens-token-suffix'
if there is such a unique character."
  (interactive)
  (let ((pos (point))
	(case-fold-search nil)
	amppos codept ustring)
    (search-backward unicode-tokens-token-prefix nil t nil)
    (setq amppos (point))
    (goto-char pos)
    (cond
     ((unicode-tokens-looking-backward-at unicode-tokens-token-match)
      (progn
	(re-search-backward unicode-tokens-token-match nil t nil)
	(if (= amppos (point))
	    (progn
	      (setq ustring 
		     (assoc (match-string 1) 
			    unicode-tokens-token-name-alist))
	      (if ustring  ;; todo: check glyphs avail/use insert fn
		  (replace-match (replace-quote (cdr ustring)))
		   ;; was (format "%c" (decode-char 'ucs (cdr codept))))
		(progn
		  (goto-char pos)
		  (insert unicode-tokens-token-suffix))))
	  (progn
	    (goto-char pos)
	    (insert unicode-tokens-token-suffix)))))
     ((unicode-tokens-looking-backward-at unicode-tokens-hexcode-match)
      (progn
	(re-search-backward unicode-tokens-hexcode-match nil t nil)
	(if (= amppos (point))
	    (progn
	      (setq codept (string-to-number (match-string 1) 16))
	      (if ;; todo : check glyph (memq codept unicode-tokens-glyph-list)
		  codept
		  (replace-match (format "%c" (decode-char 'ucs (cdr codept))))
		(progn
		  (goto-char pos)
		  (insert unicode-tokens-token-suffix))))
	  (progn
	    (goto-char pos)
	    (insert unicode-tokens-token-suffix)))))
     (t
      (insert unicode-tokens-token-suffix)))))

(defvar unicode-tokens-rotate-glyph-last-char nil)

(defun unicode-tokens-rotate-glyph-forward (&optional n)
  "Rotate the character before point in the current code page, by N steps.
If no character is found at the new codepoint, no change is made.
This function may only work reliably for GNU Emacs >= 23."
  (interactive "p")
  (if (> (point) (point-min))
      (let* ((codept  (or (if (or (eq last-command
				      'unicode-tokens-rotate-glyph-forward)
				  (eq last-command
				      'unicode-tokens-rotate-glyph-backward))
			      unicode-tokens-rotate-glyph-last-char)
			  (char-before (point))))
	     (page    (/ codept 256))
	     (pt      (mod codept 256))
	     (newpt   (mod (+ pt (or n 1)) 256))
	     (newcode (+ (* 256 page) newpt))
	     (newname (assoc newcode 
			     unicode-tokens-codept-charname-alist))
	     ;; NOTE: decode-char 'ucs here seems to fail on Emacs <23
	     (newchar (decode-char 'ucs newcode)))
	(when (and newname newchar)
	  (delete-char -1)
	  (insert-char newchar 1)
	  (message (cdr newname))
	  (setq unicode-tokens-rotate-glyph-last-char nil))
	(unless (and newname newchar)
	  (message "No character at code %d" newcode)
	  (setq unicode-tokens-rotate-glyph-last-char newcode)))))

(defun unicode-tokens-rotate-glyph-backward (&optional n)
  "Rotate the character before point in the current code page, by -N steps.
If no character is found at the new codepoint, no change is made.
This function may only work reliably for GNU Emacs >= 23."
  (interactive "p")
  (unicode-tokens-rotate-glyph-forward (if n (- n) -1)))
    


;;
;; Setup quail for Unicode tokens mode
;;

(require 'quail)

(quail-define-package
 "Unicode tokens" "UTF-8" "u" t
 "Unicode characters input method using application specific token names"
 nil t nil nil nil nil nil ; max shortest, could try t
 nil nil nil t)

(defun unicode-tokens-map-ordering (s1 s2)
  "Ordering on (car S1, car S2): order longer strings first."
  (>= (length (car s1)) (length (car s2))))

(defun unicode-tokens-propertise-after-quail (tostring)
  (add-text-properties (- (point) (length tostring)) (point)
		       (list 'utoks tostring)))
  

(defun unicode-tokens-quail-define-rules ()
  "Define the token and shortcut input rules.
Calculated from `unicode-tokens-token-name-alist' and 
`unicode-tokens-shortcut-alist'.
Also sets `unicode-tokens-token-alist'."
  (let ((unicode-tokens-quail-define-rules 
	 (list 'quail-define-rules)))
;; failed experiment (wrong place for rule/wrong type?): attempt to propertise 
;; after translation
;;	       '((advice . unicode-tokens-propertise-after-quail
;;		 (face . proof-declaration-name-face)))))
    (let ((ulist (copy-list unicode-tokens-token-name-alist))
	  ustring tokname token)
      ;; sort in case of non-terminated token syntax (empty suffix)
      (setq ulist (sort ulist 'unicode-tokens-map-ordering))
      (setq unicode-tokens-token-alist nil)
      (while ulist
	(setq tokname (caar ulist))
	(setq ustring (cdar ulist))
	(setq token (format unicode-tokens-token-format tokname))
	(cond 
	 ;; Some error checking (but not enough)
	 ((eq (length tokname) 0)
	  (warn "Empty token name (mapped to \"%s\") in unicode tokens list"
		ustring))
	 ((eq (length ustring) 0)
	  (warn "Empty token mapping, ignoring token \"%s\" in unicode tokens list"
		token))
	 ((assoc token unicode-tokens-token-alist)
	  (warn "Duplicated token entry, ignoring subsequent mapping of %s" token))
	 ((rassoc ustring unicode-tokens-token-alist)
	  (warn "Duplicated target \"%s\", ignoring token %s" ustring token))
	 (t
	  (nconc unicode-tokens-quail-define-rules
		 (list (list token 
			     (vector ustring))))
	  (setq unicode-tokens-token-alist
		(nconc unicode-tokens-token-alist
		       (list (cons token ustring))))))
	(setq ulist (cdr ulist))))
    ;; make reverse map: convert longer ustring sequences first
    (setq unicode-tokens-ustring-alist
	  (sort
	   (mapcar (lambda (c) (cons (cdr c) (car c))) 
		   unicode-tokens-token-alist)
	   'unicode-tokens-map-ordering))
    (let ((ulist (copy-list unicode-tokens-shortcut-alist))
	  ustring shortcut)
      (setq ulist (sort ulist 'unicode-tokens-map-ordering))
      (while ulist
	(setq shortcut (caar ulist))
	(setq ustring (cdar ulist))
	(nconc unicode-tokens-quail-define-rules
	       (list (list shortcut
			   (vector ustring))))
	(setq ulist (cdr ulist))))
    (eval unicode-tokens-quail-define-rules)))



;;
;; File format for saving tokens in plain ASCII.
;;

(defvar unicode-tokens-format-entry
  '(unicode-tokens "Tokens encoding unicode characters."
		   nil
		   unicode-tokens-tokens-to-unicode ; decode
 		   unicode-tokens-unicode-to-tokens ; encode
		   t nil)
  "Value for `format-alist'.")

(add-to-list 'format-alist unicode-tokens-format-entry)

(defconst unicode-tokens-ignored-properties
  '(vanilla type fontified face auto-composed
    rear-nonsticky field inhibit-line-move-field-capture
    utoks)
  "Text properties to ignore when saving files.")

(put 'font-lock-face 'format-list-valued t)

(defconst unicode-tokens-annotation-translations
  `((font-lock-face
     ;; FIXME: this is faulty; format.el makes wrong calculations with
     ;; list valued properties, and sometimes loses these settings.
     ((:weight bold)	    "bold")
     ((:slant italic)	    "italic")
     ; ,(face-all-attributes 'unicode-tokens-script-font-face) "script")
     ; ,(face-all-attributes 'unicode-tokens-fraktur-font-face) "fraktur")
     ; ,(face-all-attributes 'unicode-tokens-serif-font-face) "serif")
     ((:family "PakTypeNaqsh") "script")
     ((:family "URW Bookman L") "fraktur")
     ((:family "Liberation Serif") "serif")
     (proof-declaration-name-face "loc1")
     (default	))
    (display   
     ((raise 0.4)    "superscript")
     ((raise -0.4)   "subscript")
     ((raise 0.35)   "superscript1")
     ((raise -0.35)  "subscript1")
     ((raise 0.3)    "idsuperscript1")
     ((raise -0.3)   "idsubscript1")
     (default	)))
  "Text property table for annotations.")


(defun unicode-tokens-remove-properties (start end)
  "Remove text properties we manage between START and END."
  (remove-text-properties 
   ;; NB: this is approximate and clashes with anyone else who
   ;; looks after font-lock-face or display
   start end (mapcar 'car unicode-tokens-annotation-translations)))
  

(defun unicode-tokens-tokens-to-unicode (&optional start end)
  "Decode a tokenised file for display in Emacs."
  (save-excursion
    (save-restriction
      (narrow-to-region start (or end (point-max)))
      (let ((case-fold-search proof-case-fold-search)
	    (buffer-undo-list t)
	    (modified (buffer-modified-p))
	    (inhibit-read-only t))
	(setq unicode-tokens-next-control-token-seen-token nil)
	(format-replace-strings unicode-tokens-token-alist nil (point-min)
				(point-max))
;; alternative experiment: store original tokens inside text properties
;;	(unicode-tokens-replace-strings-propertise unicode-tokens-token-alist)
	(format-deannotate-region (point-min)
				  (point-max)
				  unicode-tokens-annotation-translations
				  'unicode-tokens-next-control-token)
	(set-buffer-modified-p modified))
      (goto-char (point-max)))))

(defvar unicode-tokens-next-control-token-seen-token nil
  "Records currently open single-character control token.")

(defun unicode-tokens-next-control-token ()
  "Find next control token and interpret it.
If `unicode-tokens-next-control-token' is non-nil, end current control sequence
after next character (single character control sequence)."
  (let (result)
    (when unicode-tokens-next-control-token-seen-token
      (if (re-search-forward unicode-tokens-next-character-regexp nil t)
	  (setq result (list (match-end 0) (match-end 0)
			     unicode-tokens-next-control-token-seen-token 
			     nil)))
      (setq unicode-tokens-next-control-token-seen-token nil))
    (while (and (not result)
		(re-search-forward unicode-tokens-control-token-match nil t))
      (let*
	  ((tok  (match-string 1))
	   (annot
	    (cond
	     ((equal tok "bsup")    '("superscript" t))
	     ((equal tok "esup")    '("superscript" nil))
	     ((equal tok "bsub")    '("subscript" t))
	     ((equal tok "esub")    '("subscript" nil))
	     ((equal tok "bbold")   '("bold" t))
	     ((equal tok "ebold")   '("bold" nil))
	     ((equal tok "bitalic") '("italic" t))
	     ((equal tok "eitalic") '("italic" nil))
	     ((equal tok "bscript") '("script" t))
	     ((equal tok "escript") '("script" nil))
	     ((equal tok "bfrak")   '("fraktur" t))
	     ((equal tok "efrak")   '("fraktur" nil))
	     ((equal tok "bserif")  '("serif" t))
	     ((equal tok "eserif")  '("serif" nil))
	     ((equal tok "loc") 
	      (list (setq unicode-tokens-next-control-token-seen-token
			  "loc1") t))
	     ((equal tok "sup") 
	      (list (setq unicode-tokens-next-control-token-seen-token
			  "superscript1") t))
	     ((equal tok "sub") 
	      (list (setq unicode-tokens-next-control-token-seen-token
			  "subscript1") t))
	     ((equal tok "isup") 
	      (list (setq unicode-tokens-next-control-token-seen-token
			  "idsuperscript1") t))
	     ((equal tok "isub") 
	      (list (setq unicode-tokens-next-control-token-seen-token
			  "idsubscript1") t)))))
	(if annot
	    (setq result
		  (append
		   (list (match-beginning 0) (match-end 0))
		   annot)))))
    result))

;; TODO: this should be instance specific  
(defconst unicode-tokens-annotation-control-token-alist 
  '(("bold" .         ("bbold" . "ebold"))
    ("subscript" .    ("bsub" . "esub"))
    ("superscript" .  ("bsup" . "esup"))
    ("subscript1" .   ("sub"))
    ("superscript1" . ("sup"))
    ("idsubscript1" . ("isub"))
    ("idsuperscript1" . ("isup"))
    ("loc1"	    . ("loc"))
    ;; non-standard
    ("italic" .       ("bitalic" . "eitalic"))
    ("script" .       ("bscript" . "escript"))
    ("fraktur" .      ("bfrak" . "efrak"))
    ("serif" .        ("bserif" . "eserif"))))
  
(defun unicode-tokens-make-token-annotation (annot positive)
  "Encode a text property start/end by adding an annotation in the file."
  (let ((toks (cdr-safe
	       (assoc annot unicode-tokens-annotation-control-token-alist))))
    (cond
     ((and toks positive)
      (format unicode-tokens-control-token-format  (car toks)))
     ((and toks (cdr toks))
      (format unicode-tokens-control-token-format  (cdr toks)))
     (t ""))))

(defun unicode-tokens-find-property (name)
  (let ((props unicode-tokens-annotation-translations)
	prop vals val vname)
    (catch 'return
      (while props
	(setq prop (caar props))
	(setq vals (cdar props))
	(while vals
	  (setq val (car vals))
	  (if (member name (cdr val))
	      (throw 'return (list prop (car val))))
	  (setq vals (cdr vals)))
	(setq props (cdr props))))))
      
(defun unicode-tokens-annotate-region (beg end &optional annot)
  (interactive "r")
  (or annot 
      (if (interactive-p)
	  (setq annot
		(completing-read "Annotate region as: " 
				 unicode-tokens-annotation-control-token-alist
				 nil t))
	(error "In unicode-tokens-annotation-control-token-alist: TYPE must be given.")))
  (add-text-properties beg end
		       (unicode-tokens-find-property annot)))

(defun unicode-tokens-annotate-region-with (annot)
  `(lambda (beg end)
     (interactive "r")
     (unicode-tokens-annotate-region beg end ,annot)))

(defun unicode-tokens-annotate-string (annot string)
  (add-text-properties 0 (length string)
		       (unicode-tokens-find-property annot)
		       string)
  string)

(defun unicode-tokens-unicode-to-tokens (&optional start end buffer)
  "Encode a buffer to save as a tokenised file."
  (let ((case-fold-search proof-case-fold-search)
	(buffer-undo-list t)
	(modified (buffer-modified-p)))
    (save-restriction
      (save-excursion
	(narrow-to-region (or start (point-min)) (or end (point-max)))
	(format-insert-annotations
	 (format-annotate-region (point-min) (point-max) 
				 unicode-tokens-annotation-translations
				 'unicode-tokens-make-token-annotation
				 unicode-tokens-ignored-properties))
;; alternative experiment: store original tokens inside text properties
;;	(unicode-tokens-replace-strings-unpropertise)
	(format-replace-strings unicode-tokens-ustring-alist 
				nil (point-min) (point-max))
	(set-buffer-modified-p modified)))))


(defun unicode-tokens-replace-strings-propertise (alist &optional beg end)
  "Do multiple replacements on the buffer.
ALIST is a list of (FROM . TO) pairs, which should be proper arguments to
`search-forward' and `replace-match', respectively.
The original contents FROM are retained in the buffer in the text property `utoks'.
Optional args BEG and END specify a region of the buffer on which to operate."
  (save-excursion
    (save-restriction
      (or beg (setq beg (point-min)))
      (if end (narrow-to-region (point-min) end))
      (while alist
	(let ((from (car (car alist)))
	      (to   (cdr (car alist)))
	      (case-fold-search nil))
	  (goto-char beg)
	  (while (search-forward from nil t)
	    (goto-char (match-beginning 0))
	    (insert to)
	    (set-text-properties (- (point) (length to)) (point)
				 (cons 'utoks
				       (cons from 
					     (text-properties-at (point)))))
	    (delete-region (point) (+ (point) (- (match-end 0)
						 (match-beginning 0)))))
	  (setq alist (cdr alist)))))))

;; NB: this is OK, except that now if we edit with raw symbols, we
;; don't get desired effect but instead rely on hidden annotations.
;; Also doesn't work as easily with quail.
;; Can we have a sensible mixture of both things?
(defun unicode-tokens-replace-strings-unpropertise (&optional beg end)
  "Reverse the effect of `unicode-tokens-replace-strings-unpropertise'.
Replaces contiguous text with 'utoks' property with property value."
  (let ((pos (or beg (point-min)))
	(lim (or end (point-max)))
	start to)
    (save-excursion
      (while (and 
	      (setq pos (next-single-property-change pos 'utoks nil lim))
	      (< pos lim))
	(if start
	    (progn
	      (setq to (get-text-property start 'utoks))
	      (goto-char start)
	      (insert to)
	      (set-text-properties start (point)
				   (text-properties-at start))
	      (delete-region (point) (+ (point) (- pos start)))
	      (setq start nil))
	  (setq start pos))))))
	  


  

;; 
;; Minor mode
;;

(defvar unicode-tokens-mode-map (make-sparse-keymap)
  "Key map used for Unicode Tokens mode.")

(define-minor-mode unicode-tokens-mode
  "Minor mode for unicode token input." nil " Utoks"
  unicode-tokens-mode-map
  (unless unicode-tokens-token-alist
    (unicode-tokens-initialise))
  (when unicode-tokens-mode
    (when (boundp 'text-property-default-nonsticky)
      (make-variable-buffer-local 'text-property-default-nonsticky)
      (setq text-property-default-nonsticky
	    ;; We want to use display property with stickyness
	    (delete '(display . t) text-property-default-nonsticky)))
    (if (and
	 (fboundp 'set-buffer-multibyte)
	 (not (buffer-base-buffer)))
	(set-buffer-multibyte t))
    (let ((inhibit-read-only t))
      ;; format is supposed to manage undo, but doesn't remap
      (setq buffer-undo-list nil) 
      (format-decode-buffer 'unicode-tokens))
    (set-input-method "Unicode tokens"))
  (unless unicode-tokens-mode
    (when (boundp 'text-property-default-nonsticky)
      (add-to-list 'text-property-default-nonsticky '(display . t)))
    ;; leave buffer encoding as is
    (let ((inhibit-read-only t)
	  (modified (buffer-modified-p)))
      ;; format is supposed to manage undo, but doesn't remap
      (setq buffer-undo-list nil) 
      (format-encode-buffer 'unicode-tokens)
      (unicode-tokens-remove-properties (point-min) (point-max))
      (set-buffer-modified-p modified))
    (inactivate-input-method)))

;; 
;; Initialisation
;;
(defun unicode-tokens-initialise ()
  "Initialise tables."
  ;; Calculate max token length
  (let ((tlist unicode-tokens-token-name-alist)
	(len 0) tok)
    (while tlist
      (when (> (length (caar tlist)) 0)
	  (setq len (length (caar tlist)))
	  (setq tok (caar tlist)))
      (setq tlist (cdr tlist)))
    (setq unicode-tokens-max-token-length
	  (length (format unicode-tokens-token-format tok))))
  ;; Names from code points
  (setq unicode-tokens-codept-charname-alist
	(mapcar (lambda (namechar)
		  (cons (cdr namechar) (car namechar)))
		unicode-chars-alist))
  ;; Default assumed available glyph list based on tokens;
  ;; TODO: filter with what's really available, if can find out.
  ;; TODO: allow altering of this when the token-name-alist is reset
  ;; in proof-token-name-alist (unless test here is for specific setting)
  (unless unicode-tokens-glyph-list
    (setq unicode-tokens-glyph-list
	  (reduce (lambda (glyphs tokustring)
		    (append glyphs (string-to-list (cdr tokustring))))
		  unicode-tokens-token-name-alist
		  :initial-value nil)))
  (unicode-tokens-quail-define-rules)
  ;; Key bindings
  (if (= (length unicode-tokens-token-suffix) 1)
      (define-key unicode-tokens-mode-map
	(vector (string-to-char unicode-tokens-token-suffix))
	'unicode-tokens-electric-suffix))
   (define-key unicode-tokens-mode-map [(control ?,)]
     'unicode-tokens-rotate-glyph-backward)
   (define-key unicode-tokens-mode-map [(control ?.)]
     'unicode-tokens-rotate-glyph-forward)
  )

;;
;; Menu
;;

(easy-menu-define unicode-tokens-menu unicode-tokens-mode-map
  "Format menu"
  (cons "Format"
	(mapcar 
	 (lambda (fmt)
	   (vector fmt
		   (unicode-tokens-annotate-region-with (downcase fmt))
		   :active 'region-exists-p))
	   '("Subscript" "Superscript" 
	     "Supscript1" "Superscript1" 
	     "Idsubscript1" "Idsuperscript1"
	     ;; don't encourage these as saving seems unreliable
	     ;; "Bold" "Italic" "Script" "Fraktur" "Serif"
	     ))))
  
(provide 'unicode-tokens)

;;; unicode-tokens.el ends here
