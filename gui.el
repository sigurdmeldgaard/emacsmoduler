(robust-load-elisp (load (concat grail-dist-elisp "color-theme")))
(robust-load-elisp (load (concat grail-local-elisp "my-color-theme")))

(my-color-theme)

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (set-variable 'color-theme-is-global nil)
            (select-frame frame)
            (if window-system
                (my-color-theme)
              (color-theme-tty-dark))))

(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
(setf x-select-enable-clipboard t)

(custom-set-faces
 '(flymake-errline ((((class color)) (:background "DarkRed"))))
 '(flymake-errline ((t (:background "DarkRed"))))
 '(flymake-warnline ((((class color)) (:background "DarkBlue")))))

(set-variable 'visible-bell t)

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
(setq default-cursor-type 'box)
