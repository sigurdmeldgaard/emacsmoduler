(robust-load-elisp (load (concat grail-dist-elisp "color-theme")))
(robust-load-elisp (load (concat grail-local-elisp "my-color-theme")))
(color-theme-initialize)

(custom-set-faces
 '(diff-added ((t (:foreground "Green"))) 'now)
 '(diff-removed ((t (:foreground "Red"))) 'now))

(fset 'original-color-theme (color-theme-make-snapshot))
(my-color-theme)

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (set-variable 'color-theme-is-global nil)
            (select-frame frame)
            (if window-system
                (my-color-theme)
              (color-theme-tty-dark))))


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
(setq default-cursor-type 'box)
