;;; popcmp.el --- Completion enhancements, popup etc
;;
;; Author: Lennart Borgman
;; Created: Tue Jan 09 12:00:29 2007
;; Version:
;; Lxast-Updated:
;; Keywords:
;; Compatibility:
;;
;; Fxeatures that might be required by this library:
;;
;;   None
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

(defun point-to-coord(point)
  "Return coordinates of point in selected window.
The coordinates are in the form \(\(XOFFSET YOFFSET) WINDOW)."
  (let* ((pn (posn-at-point point))
	 (x-y (posn-x-y pn))
	 (x (car x-y))
	 (y (cdr x-y))
	 (pos (list (list x (+ y 20)) (selected-window))))
    pos))

(defun popup-menu-at-point (menu &optional prefix)
  "Popup the given menu at point.
This is similar to `popup-menu' and MENU and PREFIX has the same
meaning as there.  The position for the popup is however where
the window point is."
  (let ((where (point-to-coord (point))))
    (popup-menu menu where prefix)))

(defmacro define-toggle (symbol value doc &rest args)
  "Declare SYMBOL as a customizable variable with a toggle function.
The purpose of this macro is to define a defcustom and a toggle
function suitable for use in a menu. The doc string for the
toggle function (which is shown if you type C-h k and then
chooses the menu item) includes a description of the SYMBOL.

SYMBOL defaults to VALUE.  DOC is the variable documentation and
The remaining arguments ARGS should have the same form as for
`defcustom':

    [KEYWORD value]...

The only keyword allowed here is :group.

A `defcustom' named SYMBOL and a `defun' named SYMBOL-toggle is
defined. This function toggles the value of SYMBOL.  The doc
string for the function says that it toggles SYMBOL and also
tells that the doc string for SYMBOL is DOC.

To create a menu item something similar to this can be used:

    \(define-key map [SYMBOL]
      \(list 'menu-item \"Toggle nice SYMBOL\"
            'SYMBOL-toggle
            :button '(:toggle . SYMBOL)))"
  (declare (doc-string 3))
  (list
   'progn
   (let ((var-decl (list 'custom-declare-variable
                         (list 'quote symbol)
                         (list 'quote value)
                         doc)))
     (while args
       (let ((arg (car args)))
         (setq args (cdr args))
         (unless (symbolp arg)
           (error "Junk in args %S" args))
         (let ((keyword arg)
               (value (car args)))
           (unless args
             (error "Keyword %s is missing an argument" keyword))
           (setq args (cdr args))
           (cond ((memq keyword '(:group :tag :set))
                  (setq var-decl (append var-decl (list keyword value))))
                 (t
                  (lwarn '(define-toggle) :error "Keyword %s can't be used here" keyword))))))
     var-decl)
   (let* ((SYMBOL-toggle (intern (concat (symbol-name symbol) "-toggle")))
          (SYMBOL-name (symbol-name symbol))
          (fun-doc (concat "Toggles the \(boolean) value of `"
                           SYMBOL-name
                           "'.\n"
                           "To set it permanently customize this variable.\n"
                           "\nDescription of `" SYMBOL-name "':\n"
                           doc)))
     `(defun ,SYMBOL-toggle()
        ,fun-doc
        (interactive)
        (customize-set-variable (quote ,symbol) (not ,symbol)))
     )))

(defgroup popcmp nil
  "Customization group for popup completion."
  :tag "Completion Style \(popup etc)"
  :group 'convenience)

(define-toggle popcmp-popup-completion t
  "Use a popup menu for some completions if non-nil.
When completion is used for alternatives tighed to text at the
point in buffer it may make sense to use a popup menu for
completion.  This variable let you decide whether normal style
completion or popup style completion should be used then.

This style of completion is not implemented for all completions.
It is implemented for specific cases but the choice of completion
style is managed generally by this variable for all these cases.

See also the options `popcmp-short-help-beside-alts' and
`popcmp-group-alternatives' which are also availabe when popup
completion is availabe."
  :tag "Popup style completion"
  :group 'popcmp)

(define-toggle popcmp-short-help-beside-alts t
  "Show a short help text beside each alternative.
If this is non-nil a short help text is shown beside each
alternative for which such a help text is available.

This works in the same circumstances as
`popcmp-popup-completion'."
  :tag "Short help beside alternatives"
  :group 'popcmp)

(define-toggle popcmp-group-alternatives t
  "Do completion in two steps.
For some completions the alternatives may have been grouped in
sets. If this option is non-nil then you will first choose a set
and then an alternative within this set.

This works in the same circumstances as
`popcmp-popup-completion'."
  :tag "Group alternatives"
  :group 'popcmp)

(defun popcmp-getsets(alts available-sets)
  (let ((sets nil))
    (dolist (tg alts)
      (let (found)
        (dolist (s available-sets)
          (when (member tg (cdr s))
            (setq found t)
            (let ((sets-entry (assq (car s) sets)))
              (unless sets-entry
                (setq sets (cons (list (car s)) sets))
                (setq sets-entry (assq (car s) sets)))
                  (setcdr sets-entry (cons tg (cdr sets-entry))))))
        (unless found
          (let ((sets-entry (assq 'unsorted sets)))
            (unless sets-entry
              (setq sets (cons (list 'unsorted) sets))
              (setq sets-entry (assq 'unsorted sets)))
            (setcdr sets-entry (cons tg (cdr sets-entry)))))))
    (setq sets (sort sets (lambda(a b)
                            (string< (format "%s" b)
                                     (format "%s" a)))))
    ;;(dolist (s sets) (setcdr s (reverse (cdr s))))
    sets))

(defun popcmp-getset-alts(set-name sets)
  ;; Allow both strings and symbols as keys:
  (let ((set (or (assoc (downcase set-name) sets)
                 (assoc (read (downcase set-name)) sets))))
    (cdr set)))

(defvar popcmp-completing-with-help nil)

(defun popcmp-add-help(alt alt-help-hash)
  (if alt-help-hash
      (let ((h (if (hash-table-p alt-help-hash)
                   (gethash alt alt-help-hash)
                 (let ((hh (assoc alt alt-help-hash)))
                   (cadr hh)))
                 ))
        (if h
            (concat alt " -- " h)
          alt))
    alt))

(defun popcmp-remove-help(alt-with-help)
  (replace-regexp-in-string " -- .*" "" alt-with-help))

(defun popcmp-completing-read-nopop(prompt
                                    table
                                    &optional predicate require-match
                                    initial-input hist def inherit-input-method
                                    alt-help
                                    alt-sets)
  (let ((alts
         (if (and popcmp-group-alternatives alt-sets)
             (all-completions initial-input table predicate)
           (if popcmp-short-help-beside-alts
               (all-completions "" table predicate)
             table))))
    (when (and popcmp-group-alternatives alt-sets)
      (let* ((sets (popcmp-getsets alts alt-sets))
             (set-names (mapcar (lambda(elt)
                                  (capitalize (format "%s" (car elt))))
                                sets))
             set)
        (setq set
              (downcase
               (completing-read (concat
                                 (substring prompt 0 (- (length prompt) 2))
                                 ", select group: ")
                                set-names
                                nil t
                                nil nil nil inherit-input-method)))
        (if (= 0 (length set))
            (setq alts nil)
          (setq alts (popcmp-getset-alts set sets)))))
    (if (not alts)
        ""
      (when popcmp-short-help-beside-alts
        (setq alts (mapcar (lambda(a)
                             (popcmp-add-help a alt-help))
                           alts)))
      (popcmp-remove-help
       (completing-read prompt
                        alts ;table
                        predicate require-match
                        initial-input hist def inherit-input-method)))))

(defun popcmp-completing-read-pop(prompt
                                  table
                                  &optional predicate require-match
                                  initial-input hist def inherit-input-method
                                  alt-help
                                  alt-sets)
  (let ((matching-alts (all-completions initial-input table predicate))
        completion)
    (let ((pop-map (make-sparse-keymap prompt))
          (where (point-to-coord (point)))
          (sets (when (and popcmp-group-alternatives alt-sets)
                  (popcmp-getsets matching-alts alt-sets)))
          (add-alt (lambda(k tg)
                     (define-key k
                       (read (format "[popcmp-%s]" tg))
                       (list 'menu-item
                             (popcmp-add-help tg alt-help)
                             `(lambda()
                                (interactive)
                                (setq completion ,tg)))))))
      (if sets
          (dolist (s sets)
            (let ((k (make-sparse-keymap)))
              (dolist (tg (cdr s))
                (funcall add-alt k tg))
              (define-key pop-map
                (read (format "[popcmps-%s]" (car s)))
                (list 'menu-item
                      (capitalize (format "%s" (car s)))
                      k))))
        (dolist (tg matching-alts)
          (funcall add-alt pop-map tg)))
      (popup-menu-at-point pop-map)
      completion)))

(defun popcmp-completing-read(prompt
                              table
                              &optional predicate require-match
                              initial-input hist def inherit-input-method
                              alt-help
                              alt-sets)
  "Read a string in the minubuffer with completion, or popup a menu.
This function can be used instead `completing-read'. The main
purpose is to provide a popup style menu for completion when
completion is tighed to text at point in a buffer. If a popup
menu is used it will be shown at the point. Whether a popup menu
or minibuffer completion is used is governed by
`popcmp-popup-completion'.

The variables PROMPT, TABLE, PREDICATE, REQUIRE-MATCH,
INITIAL-INPUT, HIST, DEF and INHERIT-INPUT-METHOD all have the
same meaning is for `completing-read'.

ALT-HELP should be nil or a hash variable or an association list
with the completion alternative as key and a short help text as
value.  You do not need to supply help text for all alternatives.
The use of ALT-HELP is set by `popcmp-short-help-beside-alts'.

ALT-SETS should be nil or an association list that has as keys
groups and as second element an alternative that should go into
this group.
"
  (popcmp-mark-completing initial-input)
  (unwind-protect
      (if popcmp-popup-completion
          (popcmp-completing-read-pop
           prompt
           table
           predicate require-match
           initial-input hist def inherit-input-method
           alt-help
           alt-sets)
        (popcmp-completing-read-nopop
         prompt
         table
         predicate require-match
         initial-input hist def inherit-input-method
         alt-help
         alt-sets))
    (popcmp-unmark-completing)))

(defvar popcmp-mark-completing-ovl nil)

(defun popcmp-mark-completing(initial-input)
  (let ((start (- (point) (length initial-input)))
        (end (point)))
    (if (overlayp popcmp-mark-completing-ovl)
        (move-overlay popcmp-mark-completing-ovl start end)
      (setq popcmp-mark-completing-ovl (make-overlay start end))
      (overlay-put popcmp-mark-completing-ovl 'face 'match)))
  (sit-for 0))

(defun popcmp-unmark-completing()
  (when popcmp-mark-completing-ovl
    (delete-overlay popcmp-mark-completing-ovl)))


;; (defun popcmp-temp()
;;   (interactive)
;;   (let* ((coord (point-to-coord (point)))
;;          (x (nth 0 (car coord)))
;;          (y (nth 1 (car coord)))
;;          (emacsw32-max-frames nil)
;;          (f (make-frame
;;              (list '(minibuffer . only)
;;                    '(title . "Input")
;;                    '(name . "Input frame")
;;                    (cons 'left x)
;;                    (cons 'top y)
;;                    '(height . 1)
;;                    '(width . 40)
;;                    '(border-width . 1)
;;                    '(internal-border-width . 2)
;;                    '(tool-bar-lines . nil)
;;                    '(menu-bar-lines . nil)
;;                    ))))
;;     f))


(provide 'popcmp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; popcmp.el ends here
