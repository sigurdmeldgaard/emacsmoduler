(when (featurep 'flymake)
  (setq flymake-allowed-file-name-masks
        (remove-if '(lambda (a) (equal (car a) "\\.c\\'")) flymake-allowed-file-name-masks))
  (setq flymake-allowed-file-name-masks
        (remove-if '(lambda (a) (equal (car a) "\\.h\\'")) flymake-allowed-file-name-masks))
  (setq flymake-allowed-file-name-masks
        (remove-if '(lambda (a) (equal (car a) "\\.cpp\\'")) flymake-allowed-file-name-masks)))
