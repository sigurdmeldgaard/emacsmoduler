;;; nxml-where.el --- Show XML path in header line
;;
;; Author: Lennart Borgman
;; Maintainer:
;; Created: Tue Dec 19 14:59:01 2006
(defconst nxml-where:version "0.1");; Version:
;; Lxast-Updated: Tue Dec 19 15:11:47 2006 (3600 +0100)
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
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

;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(eval-when-compile
  (unless (featurep 'nxml-nxhtml-autostart)
    (let ((efn (expand-file-name "../autostart.el")))
      (load efn))
    (require 'nxml-mode)))


(defvar nxml-where-header-line-format nil)
(make-variable-buffer-local 'nxml-where-header-line-format)

(defvar nxml-where-table (make-hash-table :test 'eq :weakness 'key))

(defvar nxml-where-modes '(nxml-mode nxhtml-mode))

(defvar nxml-where-update-timer nil)

(define-minor-mode nxml-where-mode
  "Shows path in mode line."
  :global nil :group 'nxml
  (if nxml-where-mode
      ;;Turn it on
      (progn
        (setq nxml-where-header-line-format header-line-format)
        (setq nxml-where-update-timer
              (run-with-idle-timer idle-update-delay t 'nxml-where-update))
;;         (dolist (buf (buffer-list))
;;           (with-current-buffer buf
;;             (setq nxml-where-mode
;;                   (member major-mode nxml-where-modes))))
        )
    ;; Turn it off
    (when (timerp nxml-where-update-timer)
      (cancel-timer nxml-where-update-timer))
    (setq nxml-where-update-timer nil)
    (setq header-line-format nxml-where-header-line-format)
    (dolist (buf (buffer-list))
      (with-current-buffer buf (setq nxml-where-mode nil)))))

(defun nxml-where-update ()
  ;; "Update the Which-Function mode display for all windows."
  ;; (walk-windows 'nxml-where-update-1 nil 'visible))
  (nxml-where-update-1 (selected-window)))

(defun nxml-where-update-1 (window)
  "Update the Which Function mode display for window WINDOW."
  (with-selected-window window
    (when nxml-where-mode
      (condition-case info
	  (let ((current (nxml-where)))
	    (unless (equal current (gethash window nxml-where-table))
	      (puthash window current nxml-where-table)
	      ;;(force-mode-line-update)
              (setq header-line-format current)
              ))
	(error
	 (setq nxml-where-mode nil)
	 (error "Error in nxml-where-update: %s" info))))))

(defun nxml-where()
  (let (path)
    (save-excursion
      (catch 'err
        (while (> (point) (point-min))
          (condition-case err
              (progn
                (nxml-backward-up-element)
                (save-match-data
                  (looking-at "<[a-z1-6]+")
                  ;;(message "looking at=(%s)" (buffer-substring (point) (match-end 0))) (sit-for 0.3)
                  (setq path (cons (buffer-substring (point) (match-end 0)) path))
                  ))
            (error (throw 'err "uh?"))))))
    (format "Path: %s" path)))



(provide 'nxml-where)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nxml-where.el ends here
