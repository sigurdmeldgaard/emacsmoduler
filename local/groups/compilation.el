(setq compilation-directory-matcher
 '("\\(?:\\(?:Entering\\|Leavin\\(g\\)\\) directory
`\\(.+\\)'$\\)\\|\\(?:[^]^[]*\\][[:space:]]*\\(\\(?:[[:alnum:]]*/\\)+\\)\\)\\|\\(?:^\\(\\[\\)\\)"
(2 . 1) (3 . 9) (4 . 4)))

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
