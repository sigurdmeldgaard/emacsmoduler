;; Standard browser.
(setq browse-url-generic-program "open")

;; Use built-in viewer for pdf's
(eval-after-load 'latex 
  '(progn
     (add-to-list 'TeX-output-view-style
                  '("^pdf$" "." "open %o %(outpage)"))))

;(require 'growl)

