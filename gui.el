(custom-set-faces
 '(diff-added ((t (:foreground "Green"))))
 '(diff-removed ((t (:foreground "Red"))))
 '(flymake-errline ((((class color)) (:background "DarkRed"))))
 '(flymake-errline ((t (:background "DarkRed"))))
 '(flymake-warnline ((((class color)) (:background "DarkBlue")))))

(unless (eq window-system 'ns)
  (set-variable 'visible-bell t))

(add-hook 'calendar-mode-hook
          (lambda () (require 'face-remap) (buffer-face-mode-invoke 'fixed nil)))

(defvar fullscreen-p nil)
(defun toggle-fullscreen()
  (interactive)
  (setq fullscreen-p (not fullscreen-p))
  (if fullscreen-p
      (set-frame-parameter nil 'fullscreen 'fullboth)
    (set-frame-parameter nil 'fullscreen 'fullheight)))

(global-set-key [f3] 'toggle-fullscreen)

(paren-activate)
(setq default-cursor-type 'box)

(defun smooth-scroll (increment)
  (scroll-up increment) (sit-for 0.05)
  (scroll-up increment) (sit-for 0.02)
  (scroll-up increment) (sit-for 0.02)
  (scroll-up increment) (sit-for 0.05)
  (scroll-up increment) (sit-for 0.06)
  (scroll-up increment))

(global-set-key [(wheel-down)] '(lambda () (interactive) (smooth-scroll 1)))
(global-set-key [(wheel-up)] '(lambda () (interactive) (smooth-scroll -1)))
