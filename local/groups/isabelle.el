(eval-after-load 'proof-script
  '(progn
     (define-key proof-mode-map (kbd "C-M-8")
       (lambda () (interactive)
         (unicode-tokens-mode)))
     (add-to-list 'isar-shortcut-alist '("``" . "´"))
     (add-to-list 'isar-shortcut-alist '("/\\\\" . "⋀"))
     (add-to-list 'isar-shortcut-alist '("{|" . "⦃"))
     (add-to-list 'isar-shortcut-alist '("|}" . "⦄"))))