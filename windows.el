;; Windows-specific code
(setq w32-apps-modifier 'super)
; Deactivate speck on windows for now...
(defun speck-mode (s) (interactive) nil)
