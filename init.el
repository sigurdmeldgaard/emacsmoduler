(package-initialize)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(dolist (package '(yasnippet yas-jit yasnippet-bundle
       expand-region undo-tree sr-speedbar
       solarized-theme smex parenface paredit
       org mic-paren color-theme-solarized
       color-theme-sanityinc-solarized color-theme
       auctex all rainbow-delimiters magit mark-multiple
       mark-more-like-this ido-ubiquitous haskell-mode))
  (unless (package-installed-p package)
    (package-install package)))


(defvar dist-elisp (concat emacsmoduler-path "/dist/elisp/"))
(defvar local-elisp (concat emacsmoduler-path "/local/elisp/"))

(add-to-list 'load-path dist-elisp)
(add-to-list 'load-path local-elisp)

(cond
 ((daemonp) ())
 ((window-system) 
    (load "gui")))

(load
 (cond
  ((string-equal "gnu/linux" system-type)  "linux.el")
  ((string-equal "darwin"    system-type)  "darwin.el")
  ((string-equal "windows-nt"   system-type)  "windows.el")))

(load "byte-code-cache")
(load "settings")
(load "commands")
(load "loadfiles")
(load "keys")
(load "small")

(provide 'init)
