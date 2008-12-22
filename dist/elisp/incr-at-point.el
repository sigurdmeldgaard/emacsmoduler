;;; incr-at-point.el --- increment/decrement the integer under the point

;; Author: Mark Triggs <mst@dishevelled.net>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Aaron S. Hawley has taken this code and improved it greatly.  The new
;; version is available at:
;;   http://www.uvm.edu/~ashawley/gnu/emacs/lisp/integers.el
;;
;; This code is inspired by a similar feature in Vim. The function
;; INCREMENT-NUMBER-AT-POINT will increment or decrement a number of the form
;; [+-][0-9]+ under the point. To use it, add something like the following to
;; your ~/.emacs:

;; (define-key global-map [M-up] 'increment-number-at-point)
;; (define-key global-map [M-down] (lambda (&optional n)
;;                                   (interactive "p")
;;                                   (increment-number-at-point (or (- (abs n))
;;                                                                  -1))))

;;; Code:

(defun increment-number-at-point (&optional amount)
  (interactive "p")
  (let ((n (integer-at-point))
        (bounds (integer-at-point-bounds)))
    (if n
        (destructuring-bind (a . b) bounds
          (goto-char b)
          (let ((p (point-marker)))
            (delete-region a b)
            (insert-before-markers (number-to-string (+ n amount)))
            (goto-char (marker-position p))))
      (error "No number under point"))))

(defun integer-at-point ()
  (destructuring-bind (a . b) (or (integer-at-point-bounds) (cons nil nil))
    (if (and a b)
        (string-to-number (buffer-substring a b))
      nil)))

(defun integer-at-point-bounds ()
  (save-excursion
    (cond ((or (looking-at "[0-9]")
               (and (save-excursion
                      (forward-char -1)
                      (looking-at "[0-9]"))
                    (progn (forward-char -1)
                           t)))
           (while (looking-at "[0-9]")
             (forward-char -1))
           (forward-char 1)
           (let ((beginning (point)))
             (while (looking-at "[0-9]")
               (forward-char 1))
             (cons beginning (point))))
          (t nil))))



(provide 'incr-at-point)
;;; incr-at-point.el ends here
