(robust-load-elisp (concat grail-dist-elisp "color-theme"))
(require 'my-color-theme)
(my-color-theme)

(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
(setf x-select-enable-clipboard t)

(custom-set-faces
 '(flymake-errline ((((class color)) (:background "DarkRed"))))
 '(flymake-errline ((t (:background "DarkRed"))))
 '(flymake-warnline ((((class color)) (:background "DarkBlue")))))

(set-variable 'visible-bell nil)

(defvar fullscreen-p nil)
(defun toggle-fullscreen()
  (interactive)
  (setq fullscreen-p (not fullscreen-p))
  (if fullscreen-p
      (set-frame-parameter nil 'fullscreen 'fullboth)
    (set-frame-parameter nil 'fullscreen 'fullheight)))

(global-set-key [f3] 'toggle-fullscreen)

(require 'mic-paren)
(paren-activate)
