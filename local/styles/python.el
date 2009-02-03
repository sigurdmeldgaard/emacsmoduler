;(load "pymacs")
;(load "python")

;;(eval-after-load "pymacs"
;;  '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))
;(defun load-ropemacs ()
;    "Load pymacs and ropemacs"
;    (interactive)
;    (require 'pymacs)
;    (pymacs-load "ropemacs" "rope-")
;    ;; Automatically save project python buffers before refactorings
;    (setq ropemacs-confirm-saving 'nil))

;(add-hook 'python-mode-hook 'load-ropemacs)
;(autoload 'pysmell-mode "pysmell" "Code completion for python" t)

(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pycheck" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))

;(setq python-mode-hook nil)
;(add-hook 'python-mode-hook
;          '(lambda () (eldoc-mode 1)))
(add-hook 'python-mode-hook
          '(lambda () (flymake-mode 1)))

(add-hook 'find-file-hook 'flymake-find-file-hook)
