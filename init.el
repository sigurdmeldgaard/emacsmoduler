(defvar dist-elisp
  (concat emacsmoduler-path "/dist/elisp/")
  "The directory containing third-party elisp extensions of Emacs.")

(defvar local-elisp
  (concat emacsmoduler-path "/local/elisp/")
  "The directory containing third-party elisp extensions of Emacs.")

(add-to-list 'load-path dist-elisp)
(add-to-list 'load-path local-elisp)


(cond
 ((daemonp) 
  (progn
    (add-to-list 'after-make-frame-functions 'grail-load-gui-configuration-once t)
    (add-hook 'before-make-frame-hook 'grail-load-display-configuration-once) ))
 ((window-system) 
  (progn
    (load "gui.el"))))

(load
 (cond
  ((string-equal "gnu/linux" system-type)  "linux.el")
  ((string-equal "darwin"    system-type)  "darwin.el")
  ((string-equal "windows"   system-type)  "windows.el")))

(load "byte-code-cache")
(load "settings")
(load "commands")
(load "loadfiles")
(load "keys")
(load "small")

(provide 'init)