
;;; mindent.el --- simple non-syntax sensitive indentation

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Copyright (C) Ian Zimmerman, February 2002

;; $Id: mindent.el,v 1.2 2004/07/06 03:52:16 itz Exp $

(defvar mindent-comment-predicate (lambda () nil)
"A predicate function called to determine if the point is in a comment.
By default, always returns nil.")

(make-variable-buffer-local 'mindent-comment-predicate)

(defconst mindent-mode-map
(let ((kmap (make-sparse-keymap)))
(define-key kmap "\C-c[" 'mindent-backward-to-same-indent)
(define-key kmap "\C-c]" 'mindent-forward-to-same-indent)
(define-key kmap "\C-c\\" 'mindent-backward-to-less-indent)
(define-key kmap "\C-c/" 'mindent-forward-to-less-indent)
(define-key kmap "\C-c=" 'mindent-indent-relative)
(define-key kmap "\C-c-" 'mindent-unindent-line)
(define-key kmap "\C-c'" 'mindent-indent-region-relative)
(define-key kmap "\C-c`" 'mindent-unindent-region)
(define-key kmap "\C-c." 'mindent-indent-to-char)
(define-key kmap "\C-c^" 'mindent-indent-as-line-starting)
(define-key kmap "\C-c," 'mindent-indent-region-to-char)
kmap)
"Keymap used in mindent mode.")

;;;###autoload
(define-minor-mode mindent-mode
"Set or toggle the mindent minor mode.
\\<mindent-mode-map> This mode provides simple, syntax-oblivious
indentation for languages that are too difficult to parse with current
Emacs mechanisms, such as Haskell. Commands:

\\[mindent-backward-to-same-indent] - Move point back N lines with less \
or same indentation.
\\[mindent-forward-to-same-indent] - Move point forward N lines with \
less or same indentation.
\\[mindent-backward-to-less-indent] - Move point back N lines with \
strictly less indentation.
\\[mindent-forward-to-less-indent] - Move point forward N lines with \
strictly less indentation.
\\[mindent-indent-relative] - Indent current line relative to the Nth \
indentation point.
\\[mindent-unindent-line] - Unindent current line N levels.
\\[mindent-indent-region-relative] - Indent current region relative to \
the Nth indentation point.
\\[mindent-unindent-region] - Unindent region N levels.
\\[mindent-indent-to-char] - Indent current line to the Nth occurrence \
of C on the previous line.
\\[mindent-indent-as-line-starting] - Indent current line just like \
the Nth preceding line starting with C.
\\[mindent-indent-region-to-char] - Indent current region to the Nth \
occurrence of C on the previous line.
\\[mindent-indent-to-mouse] - Indent current line to column where \
mouse was clicked.
\\[mindent-indent-as-mouse-line] - Indent current line just like line \
where mouse was clicked.
\\[mindent-indent-region-to-mouse] - Indent current region to column \
where mouse was clicked.
\\[mindent-indent-region-as-mouse-line] - Indent current region just \
like line where mouse was clicked.

`mindent-comment-predicate' is a variable holding a predicate function
called to determine if the point is in a comment.
" nil " Mindent" mindent-mode-map
(cond
(mindent-mode
(make-local-variable 'mindent-saved-indent-line-function)
(setq mindent-saved-indent-line-function indent-line-function)
(make-local-variable 'indent-line-function)
(setq indent-line-function 'mindent-indent-line)
(make-local-variable 'mindent-saved-indent-region-function)
(setq mindent-saved-indent-region-function indent-region-function)
(make-local-variable 'indent-region-function)
(setq indent-region-function 'mindent-indent-region))
(t
(setq indent-line-function mindent-saved-indent-line-function)
(setq indent-region-function mindent-saved-indent-region-function))))

;;;###autoload
(defun turn-on-mindent-mode ()
"Turn on the mindent minor mode."
(interactive)
(mindent-mode 1))

;;;###autoload
(defun turn-off-mindent-mode ()
"Turn off the mindent minor mode."
(interactive)
(mindent-mode 0))

(defun mindent-newline-and-indent (&optional p)
"Like \\<global-map>\\[newline-and-indent].
Additionally, if P is an integer, indent the new line P spaces relative
to the current line."
(interactive "P")
(let ((i (current-indentation))
(offset (get-text-property (point) 'mindent-offset)))
(newline-and-indent)
(when (integerp p) (setq i (+ i p)))
(when offset
(setq i (+ i offset))
(let ((end (next-single-property-change (point) 'mindent-offset))
(start (save-excursion (beginning-of-line) (point))))
(remove-text-properties start end '(mindent-offset nil))))
(indent-line-to i)))

(defun mindent-backward-to-same-indent (&optional n)
"Move point back N lines with less or same indentation."
(interactive "p")
(beginning-of-line 1)
(if (< n 0) (mindent-forward-to-same-indent (- n))
(while (> n 0)
(let ((i (current-indentation)))
(forward-line -1)
(while (or (> (current-indentation) i)
(funcall mindent-comment-predicate)
(looking-at
(concat "[ \t]*\\(\n\\|" comment-start-skip "\\)")))
(forward-line -1)))
(setq n (1- n))))
(back-to-indentation))

(defun mindent-forward-to-same-indent (&optional n)
"Move point forward N lines with less or same indentation."
(interactive "p")
(beginning-of-line 1)
(if (< n 0) (mindent-backward-to-same-indent (- n))
(while (> n 0)
(let ((i (current-indentation)))
(forward-line 1)
(while (or (> (current-indentation) i)
(funcall mindent-comment-predicate)
(looking-at
(concat "[ \t]*\\(\n\\|" comment-start-skip "\\)")))
(forward-line 1)))
(setq n (1- n))))
(back-to-indentation))

(defun mindent-backward-to-less-indent (&optional n)
"Move point back N lines with stricly less indentation."
(interactive "p")
(beginning-of-line 1)
(if (< n 0) (mindent-forward-to-less-indent (- n))
(while (> n 0)
(let ((i (current-indentation)))
(forward-line -1)
(while (or (>= (current-indentation) i)
(funcall mindent-comment-predicate)
(looking-at
(concat "[ \t]*\\(\n\\|" comment-start-skip "\\)")))
(if (bobp) (error "Beginning of buffer"))
(forward-line -1)))
(setq n (1- n))))
(back-to-indentation))

(defun mindent-forward-to-less-indent (&optional n)
"Move point forward N lines with strictly less indentation."
(interactive "p")
(beginning-of-line 1)
(if (< n 0) (mindent-backward-to-less-indent (- n))
(while (> n 0)
(let ((i (current-indentation)))
(forward-line 1)
(while (or (>= (current-indentation) i)
(funcall mindent-comment-predicate)
(looking-at
(concat "[ \t]*\\(\n\\|" comment-start-skip "\\)")))
(if (eobp) (error "End of buffer"))
(forward-line 1)))
(setq n (1- n))))
(back-to-indentation))

(defun mindent-internal-indent-once (&optional bol-ok)
"Indent to the next available indentation point.
Indentation points are the columns where the previous nonblank line
has a blank character, followed by a nonblank character.
Additionally, if BOL-OK is set, a nonblank character in column 0
is considered an indentation point."
(let* ((here (current-column))
(indp
(save-excursion
(if (not (re-search-backward "\\(.\\|\n\\)\n" nil t)) 0
(beginning-of-line)
(while (and (not (bobp))
(or (funcall mindent-comment-predicate)
(looking-at (concat "[ \t]*\\(\n\\|"
comment-start-skip
"\\)"))))
(forward-line -1))
(end-of-line)
(let ((endp (point)))
(move-to-column here)
(if (and bol-ok (bolp) (looking-at "\\S ")) nil
(re-search-forward "\\s \\S " endp 'move)
(backward-char 1))
(current-column))))))
(indent-line-to indp)))

(defun mindent-internal-indent (n &optional bol-ok)
"Go forward N indentation points, or as many as possible.
Assume point is exactly on left margin to start with."
(cond
((= n 0) nil)
((> n 0)
(mindent-internal-indent-once bol-ok)
(mindent-internal-indent (- n 1)))))

(defsubst mindent-maybe-reindent (goal)
(if (<= (current-column) (current-indentation)) (indent-line-to goal)
(save-excursion (indent-line-to goal))))

(defun mindent-unindent-line (n)
"Unindent current line N levels.
That is, find the Nth stricly less indented line preceding
this one and reindent to it."
(interactive "p")
(let ((indp
(save-excursion
(mindent-backward-to-less-indent n)
(current-indentation))))
(mindent-maybe-reindent indp)))

(defun mindent-indent-line ()
"Indent the current line the same as last nonblank one."
(interactive)
(let ((indp
(cond
((save-excursion
(not (re-search-backward "\\(.\\|\n\\)\n" nil t))) 0)
((and
(funcall mindent-comment-predicate)
(not
(save-excursion
(let ((p (point)))
(beginning-of-line)
(re-search-forward comment-start-skip p t))))
(save-excursion
(forward-line -1)
(beginning-of-line)
(or (and (funcall mindent-comment-predicate)
(looking-at "[ \t]*"))
(looking-at (concat "[ \t]*" comment-start-skip))))
(save-excursion
(goto-char (match-end 0))
(current-column))))
(t
(save-excursion
(re-search-backward "\\(.\\|\n\\)\n")
(beginning-of-line)
(while (and (not (bobp))
(or (funcall mindent-comment-predicate)
(looking-at (concat "[ \t]*\\(\n\\|"
comment-start-skip
"\\)"))))
(forward-line -1))
(current-indentation))))))
(mindent-maybe-reindent indp)))

(defun mindent-indent-relative (n)
"Indent current line relative to the Nth indentation point.
Indentation points are the columns where the previous nonblank line
has a blank character, followed by a nonblank character.
With negative N, unindent current line N levels."
(interactive "p")
(if (< n 0) (mindent-unindent-line (- n))
(if (> (current-column) (current-indentation))
(save-excursion
(back-to-indentation)
(mindent-internal-indent n))
(back-to-indentation)
(mindent-internal-indent n))))

(defun mindent-indent-to-char (c n)
"Indent current line to the Nth occurrence of C on the previous line."
(interactive "cIndent to character: \np")
(let* ((search-string
(if (or (char-equal c ?-) (char-equal c ?^) (char-equal c ?\\ ))
(concat "^\\" (char-to-string c))
(concat "^" (char-to-string c))))
(start-column (current-indentation))
(goal (save-excursion
(forward-line -1)
(end-of-line)
(let ((limit (point)))
(move-to-column start-column)
(while (> n 0)
(skip-chars-forward search-string limit)
(setq n (1- n))
(if (and (> n 0) (< (point) limit))
(forward-char 1)))
(current-column)))))
(mindent-maybe-reindent goal)))

(defun mindent-indent-to-mouse (ev)
"Indent current line to column where mouse was clicked."
(interactive "@e")
(let ((indp (car (posn-col-row (event-start ev)))))
(mindent-maybe-reindent indp)))

(defun mindent-indent-as-mouse-line (ev)
"Indent current line just like line where mouse was clicked."
(interactive "@e")
(let ((indp
(save-excursion
(mouse-set-point ev)
(current-indentation))))
(mindent-maybe-reindent indp)))

(defun mindent-indent-as-line-starting (c n)
"Indent current line just like the Nth preceding line starting with C."
(interactive "cIndent as line starting with: \np")
(if (zerop n) nil
(let* ((direction (if (> n 0) -1 1))
(num (abs n))
(indp
(save-excursion
(beginning-of-line 1)
(while (> num 0)
(forward-line direction)
(setq num (1- num))
(while (or (not (looking-at
(concat
"[ \t]*"
(regexp-quote (char-to-string c)))))
(funcall mindent-comment-predicate)
(looking-at
(concat "[ \t]*\\(\n\\|"
comment-start-skip
"\\)")))
(if (or (bobp) (eobp)) (error "No such line"))
(forward-line direction)))
(back-to-indentation)
(current-column))))
(mindent-maybe-reindent indp))))

;; functions that reindent entire regions
(defun mindent-compute-block-indent (pos)
"Given a position POS, compute the amount of space needed to indent
the current line so that it is indented to the column which is the
current columns at POS. The result can be negative, if unindenting is
necessary to get to POS."
(let ((goal
(save-excursion
(goto-char pos)
(current-column))))
(- goal (current-indentation))))

(defun mindent-unindent-region (n start end)
"Unindent region N levels.
That is, find the Nth stricly less indented line preceding
the first on in the region, and reindent to it."
(interactive "p\nr")
(save-excursion
(goto-char start)
(let* ((p (save-excursion
(mindent-backward-to-less-indent n) (point)))
(offset (mindent-compute-block-indent p)))
(indent-code-rigidly start end offset))))

(defun mindent-indent-region (start end)
"Indent current region as a block to the level of preceding line."
(interactive "r")
(save-excursion
(goto-char start)
(let* ((indp
(save-excursion
(forward-line -1)
(back-to-indentation)
(point)))
(offset (mindent-compute-block-indent indp)))
(indent-code-rigidly start end offset))))

(defun mindent-indent-region-relative (n start end)
"Indent current region relative to the Nth indentation point.
Indentation points are the columns where the previous nonblank line
has a blank character, followed by a nonblank character.
With negative N, unindent current region N levels."
(interactive "p\nr")
(if (< n 0) (mindent-unindent-region (- n) start end)
(save-excursion
(goto-char start)
(let* ((here (current-indentation))
(indp
(save-excursion
(if (not (re-search-backward "\\(.\\|\n\\)\n" nil t))
(point-min)
(beginning-of-line)
(while (and (not (bobp))
(or (funcall mindent-comment-predicate)
(looking-at (concat "[ \t]*\\(\n\\|"
comment-start-skip
"\\)"))))
(forward-line -1))
(end-of-line)
(let ((endp (point)))
(move-to-column here)
(re-search-forward "\\s \\S " endp 'move n)
(backward-char 1)
(point)))))
(offset (mindent-compute-block-indent indp)))
(indent-code-rigidly start end offset)))))

(defun mindent-indent-region-to-char (c n start end)
"Indent current region to the Nth occurrence of C on the previous line."
(interactive "cIndent to character: \np\nr")
(save-excursion
(goto-char start)
(let* ((search-string
(if (or (char-equal c ?-)
(char-equal c ?^)
(char-equal c ?\\ ))
(concat "^\\" (char-to-string c))
(concat "^" (char-to-string c))))
(start-column (current-indentation))
(goal (save-excursion
(forward-line -1)
(end-of-line)
(let ((limit (point)))
(move-to-column start-column)
(while (> n 0)
(skip-chars-forward search-string limit)
(setq n (1- n))
(if (and (> n 0) (< (point) limit))
(forward-char 1)))
(current-column))))
(offset (- goal start-column)))
(indent-code-rigidly start end offset))))

(defun mindent-indent-region-to-mouse (ev start end)
"Indent current region to column where mouse was clicked."
(interactive "@e\nr")
(save-excursion
(goto-char start)
(let ((c (progn (back-to-indentation) (current-column)))
(goal (car (posn-col-row (event-start ev)))))
(indent-code-rigidly start end (- goal c)))))

(defun mindent-indent-region-as-mouse-line (ev start end)
"Indent current region just like line where mouse was clicked."
(interactive "@e\nr")
(save-excursion
(goto-char start)
(let ((indp
(save-excursion
(mouse-set-point ev)
(back-to-indentation)
(point))))
(indent-code-rigidly
start end
(mindent-compute-block-indent indp)))))

(provide 'mindent)

;;; mindent.el ends here