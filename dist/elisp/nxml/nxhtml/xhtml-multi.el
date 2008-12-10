;;; xhtml-multi.el --- Editing of embedded <?MODE...?> parts
;;
;; Description:
;; Author:
;; Maintainer:
;; Created: Tue Dec 19 15:39:09 2006
(defconst xhtml-multi:version "0.51") ;; Version:
;; Last-Updated: Tue Feb 20 23:30:09 2007 (3600 +0100)
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
;;   Handling of XHTML files with <?php ?> type subfields. See /gr
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

(defgroup xhtml-multi nil
  "Customization group for `xhtml-multi-mode'."
  :group 'nxhtml
  :group 'html)

(defcustom xhtml-multi-xthml-mode 'nxhtml-mode
  "Mode to use for XHTML part of buffer."
  :type 'function
  :group 'xhtml-multi)

(defcustom xhtml-multi-major-modes
  '(php-mode
    nxml-mode
    nxhtml-mode
    html-mode)
  "List of major modes that should use `xhtml-multi-mode'."
  :type '(repeat function)
  :group 'xhtml-multi)

(defcustom xhtml-multi-submodes
  '(("php" . php-mode))
  "Modes to use for <?LANG ?> parts of buffer.
Association list where the first part is matched to LANG and the
second part is the major mode function to use."
  :type '(repeat (cons string function))
  :group 'xhtml-multi)

(defface xhtml-multi-outside-submode
  '((((class color) (min-colors 88) (background dark))
     ;;:background "blue3")
     :background "gray34")
    (((class color) (min-colors 88) (background light))
     ;;:background "lightgoldenrod2")
     :background "gray88")
    (((class color) (min-colors 16) (background dark))
     :background "blue3")
    (((class color) (min-colors 16) (background light))
     :background "lightgoldenrod2")
    (((class color) (min-colors 8))
     :background "blue" :foreground "white")
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "gray"))
  "Face for parts outside a submode."
  :group 'xhtml-multi)


(defvar xhtml-multi-update-timer nil)

(defvar xhtml-multi-mode-in-buffer nil)
(make-variable-buffer-local 'xhtml-multi-mode-in-buffer)

(defun xhtml-multi-update ()
  "Update major mode for buffer in selected window."
  ;; (walk-windows 'xhtml-multi-update-1 nil 'visible))
  (xhtml-multi-update-1 (selected-window)))

(defun xhtml-multi-update-1 (window)
  "Update major mode for buffer in window WINDOW."
  (when xhtml-multi-mode
    (with-selected-window window
      (when xhtml-multi-mode-in-buffer
        (let (errmsg)
          (condition-case info
              (xhtml-multi-set-major)
            (error (setq errmsg (error-message-string info))
                   (message "In xhtml-multi-update: %s" errmsg)
                   (setq xhtml-multi-mode-in-buffer nil))))))))

(defvar xhtml-multi-internal-change-major nil)

(defun xhtml-multi-change-major-fun()
  ;; For after-change-major-mode-hook
  (unless xhtml-multi-internal-change-major
    (when (memq major-mode xhtml-multi-major-modes)
      (setq xhtml-multi-mode-in-buffer t)))
  t)

(defvar xhtml-multi-sub-begin nil)
(make-variable-buffer-local 'xhtml-multi-sub-begin)
(defvar xhtml-multi-sub-end nil)
(make-variable-buffer-local 'xhtml-multi-sub-end)

(defvar xhtml-multi-ovl-begin nil)
(make-variable-buffer-local 'xhtml-multi-ovl-begin)
(defvar xhtml-multi-ovl-end nil)
(make-variable-buffer-local 'xhtml-multi-ovl-end)

(defcustom xhtml-multi-dim t
  "Dim text outside current select mode region."
  :type 'boolean
  :group 'xhtml-multi)

(defun xhtml-multi-set-major()
  (let* (
         (here (point))
         php-end
         php-begin
         want-major)
    (save-excursion
      (setq php-end   (search-backward "?>" nil t))
      (goto-char here)
      (setq php-begin (search-backward "<?" nil t))
      (when (looking-at "<\\?\\([a-zA-Z]+\\)")
        (let ((mode (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
              (mode-cons))
          (when (setq mode-cons (assoc mode xhtml-multi-submodes))
            (setq want-major (cdr mode-cons))))))
    (when (or (and (not php-begin)
                   (not php-end))
              (and php-begin
                   php-end
                   (< php-begin php-end))
              (and php-end
                   (not php-begin)))
      (when xhtml-multi-ovl-begin
        (delete-overlay xhtml-multi-ovl-begin))
      (when xhtml-multi-ovl-end
        (delete-overlay xhtml-multi-ovl-end))
      (setq want-major xhtml-multi-xthml-mode))
    (when want-major
      (unless (eq want-major xhtml-multi-xthml-mode)
        (unless (or (not xhtml-multi-dim)
                    (and xhtml-multi-sub-begin
                         (= xhtml-multi-sub-begin
                            php-begin)))
          (unless xhtml-multi-ovl-begin
            (setq xhtml-multi-ovl-begin (make-overlay (point-min) php-begin)))
          (move-overlay xhtml-multi-ovl-begin (point-min) php-begin)
          (overlay-put xhtml-multi-ovl-begin 'face 'xhtml-multi-outside-submode))
        (save-excursion
          (goto-char here)
          (setq php-end (search-forward "?>" nil t)))
        (unless (or (not xhtml-multi-dim)
                    (and xhtml-multi-sub-end
                         (= xhtml-multi-sub-end
                            php-end)))
          (unless xhtml-multi-ovl-end
            (setq xhtml-multi-ovl-end (make-overlay php-end (point-max))))
          (move-overlay xhtml-multi-ovl-end php-end (point-max))
          (overlay-put xhtml-multi-ovl-end 'face 'xhtml-multi-outside-submode)))
      (unless (eq major-mode want-major)
        (let ((xhtml-multi-internal-change-major t))
          (let ((ovl-beg xhtml-multi-ovl-begin)
                (ovl-end xhtml-multi-ovl-end))
            (funcall want-major)
            (setq xhtml-multi-ovl-begin ovl-beg)
            (setq xhtml-multi-ovl-end   ovl-end))
          (setq xhtml-multi-mode-in-buffer t))))))

(define-minor-mode xhtml-multi-mode
  "Switch major mode automatically for different parts of buffer.
The buffer should contain XHTML style code mixed with some
embedded mode, for example <?php ?>. When point is outside the
embedding a HTML style major mode is used (see
`xhtml-multi-xthml-mode'). When point is inside the embedding a
major mode based on the language choosen there is selected (see
`xhtml-multi-submodes')."
  :global t
  :group 'xhtml-multi
  (if xhtml-multi-mode
      ;;Turn it on
      (progn
        (add-hook 'after-change-major-mode-hook 'xhtml-multi-change-major-fun)
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (xhtml-multi-change-major-fun)))
        (setq xhtml-multi-update-timer
              (run-with-idle-timer idle-update-delay t 'xhtml-multi-update)))
    ;; Turn it off
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (overlayp xhtml-multi-ovl-begin)
          (delete-overlay xhtml-multi-ovl-begin))
        (when (overlayp xhtml-multi-ovl-end)
          (delete-overlay xhtml-multi-ovl-end))))
    (remove-hook 'after-change-major-mode-hook 'xhtml-multi-change-major-fun)
    (when (timerp xhtml-multi-update-timer)
      (cancel-timer xhtml-multi-update-timer))
    (setq xhtml-multi-update-timer nil)))


(provide 'xhtml-multi)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; xhtml-multi.el ends here
