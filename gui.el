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

(require 'mic-paren)
(paren-activate)
