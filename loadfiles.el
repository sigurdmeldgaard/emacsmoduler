(add-to-list 'load-path (concat emacsmoduler-path "/modes/"))
(add-to-list 'load-path (concat emacsmoduler-path "/modes/mmm-mode-0.4.8"))
(add-to-list 'load-path (concat emacsmoduler-path "/modes/haskell-mode-2.3"))
(add-to-list 'load-path (concat emacsmoduler-path "/modes/emacs-rails"))
(add-to-list 'load-path (concat emacsmoduler-path "/modes/gnuplot-mode"))
(add-to-list 'load-path (concat emacsmoduler-path "/modes/org/lisp"))
(add-to-list 'load-path (concat emacsmoduler-path "/modes/org/contrib"))
(add-to-list 'load-path (concat emacsmoduler-path "/util"))
(add-to-list 'load-path (concat emacsmoduler-path "/modes/auctex"))

(defmacro try (name &rest form)
  `(condition-case err
       (progn
              ,@form)
     (error
      (message (format "Error, %s during load of %s" err ,name)))))


;(require 'byte-code-cache)

(try "Haskell site"
     (load "haskell-site-file"))

;(try "CUA!!" (require 'cua))

(try (require 'mmm-auto))

(try "ido"
       (require 'ido)
       (ido-mode 1 nil))

(try "Factor stuff" (require 'factor)
     (setq factor-binary "~/factor/factor"
           factor-image "~/factor/factor.image"))


(autoload 'd-mode "d-mode" nil t)

(autoload 'peep-mode "peep" nil t)
(autoload 'russian-mode "russian-mode" nil t)

(progn
  (autoload 'js2-mode "js2" nil t)
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  (setq js2-basic-offset 2)
  (setq js2-use-font-lock-faces t))

(when (featurep 'x)
  (try "colors!" (require 'my-color-theme)))

(try "erlang" (require 'erlang))

(try "nxml" (load "modes/nxml/autostart"))

;(load "rng-auto")

;(require 'lua-mode)
(autoload 'longlines-mode "longlines" nil t)
(autoload 'hoogle-mode "hoogle" nil t)
(autoload 'xquery-mode "xquery-mode" nil t)

;(autoload 'css-mode "css-mode")

(autoload 'php-mode "php-mode" nil t)

(try "Tex blah" (require 'tex-site))
(load "tex-site")

(require 'tramp)

(require 'better-registers)

(require 'smooth-scrolling)
(autoload 'haml-mode "haml-mode" nil t)

(require 'snippet)
(require 'find-recursive)

(require 'rails)

(require 'iedit)

(require 'sr-speedbar)

(autoload 'log4j-mode "log4j-mode" "Major mode for viewing log files." t)
(add-to-list 'auto-mode-alist '("\\.log\\'" . log4j-mode))

(try "Elpa" (when
                (load
                 (expand-file-name "~/.emacs.d/elpa/package.el"))
              (package-initialize)))

(when window-system
  (require 'mic-paren)
  (paren-activate))

(autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)

(require 'org-install)

;;Unicode naming
;(load "unichars")
;(load "xmlunicode")
