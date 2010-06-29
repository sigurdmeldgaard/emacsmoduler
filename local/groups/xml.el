(require 'nxml-enc)
(robust-load-elisp (load (concat grail-dist-elisp "nxml/autostart")))
(autoload 'php-mode "php-mode" nil t)

(setq nxml-slash-auto-complete-flag t)
(add-to-list 'auto-mode-alist '("\\.php[34]?\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.htm[l]?\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
