;; nxhtml-autoload.el -- Autoloading of nxthml-mode

;; Copyright (C) 2005, 2006, 2007 by Lennart Borgman

;; Author: Lennart Borgman <lennartDOTborgmanDOT073ATstudentDOTluDOTse>
;; Created: Sat Feb 11 00:06:14 2006
;; Version: 0.5
;; Last-Updated: Mon Jan 15 03:11:37 2007 (3600 +0100)
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

(if (not (featurep 'nxml-enc))
    (progn
      (lwarn
       '(nxhtml-autoload)
       :emergency
       (concat
        "\n\n\nERROR: nxml not loaded!\n\n"
        "    Please load nxml before loading nxhtml!\n"
        "    Load nxml by calling rng-auto.el in the nxml distribution.\n\n\n\n"))
      (sit-for 10))

  (add-to-list 'load-path
               (file-name-directory
                (if load-file-name load-file-name buffer-file-name)))

  (autoload 'nxhtml-mode "nxhtml" "Mode for editing XHTML files" t)

  (require 'fmode)
  (fmode-replace-default-mode 'html-mode 'nxhtml-mode)
  (fmode-replace-default-mode 'xml-mode 'nxml-mode)

  (require 'xmlpe)
  (add-to-list 'auto-mode-alist  '("\\.htmlf" . xmlpe-auto-mode))
  (xmlpe-set-mode-alist-entry '("\\.htmlf" nxhtml-mode nil "xhtml-iso-8859-1"))

  (require 'html-site)

  ;;; Change below if you need to:
  (autoload 'css-mode "css-mode" "Mode for editing css files" t)
  (add-to-list 'auto-mode-alist '(".*\\.css"    . css-mode))

  (autoload 'ecmascript-mode "ecmascript-mode" "Mode for JavaScript" t)
  (add-to-list 'auto-mode-alist '(".*\\.js"     . ecmascript-mode))

  (autoload 'php-mode "php-mode" "Mode for editing php files" t)
  (add-to-list 'auto-mode-alist '(".*\\.php"    . php-mode))

  (add-hook 'nxml-mode-hook
            (lambda ()
              (define-key nxml-mode-map [M-left]  'nxml-backward-element)
              (define-key nxml-mode-map [M-right] 'nxml-forward-element)
              (define-key nxml-mode-map [M-up]    'nxml-backward-up-element)
              (define-key nxml-mode-map [M-down]  'nxml-down-element)))

  (add-hook 'css-mode-hook        (lambda () (require 'xhtml-help) (xhtml-help-mode t)))
  (add-hook 'ecmascript-mode-hook (lambda () (require 'xhtml-help) (xhtml-help-mode t)))
  )

(provide `nxhtml-autoload)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nxhtml-autoload.el ends here
