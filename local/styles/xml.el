
(robust-load-elisp (concat grail-dist-elisp "nxml/autostart.el"))
(autoload 'php-mode "php-mode" nil t)

(setq nxml-slash-auto-complete-flag t)
(add-to-list 'auto-mode-alist '("\\.php[34]?\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.htm[l]?\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
