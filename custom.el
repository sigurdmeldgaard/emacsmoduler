(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "firefox")
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(custom-safe-themes (quote ("6cfe5b2f818c7b52723f3e121d1157cf9d95ed8923dbc1b47f392da80ef7495d" "71efabb175ea1cf5c9768f10dad62bb2606f41d110152f4ace675325d28df8bd" "71b172ea4aad108801421cc5251edb6c792f3adbaecfa1c52e94e3d99634dee7" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "54d1bcf3fcf758af4812f98eb53b5d767f897442753e1aa468cfeb221f8734f9" "374e79a81930979e673b8e0869e135fb2450b18c6474ca145f104e0c6f003267" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "485737acc3bedc0318a567f1c0f5e7ed2dfde3fb" default)))
 '(dired-recursive-copies (quote always))
 '(dired-recursive-deletes (quote top))
 '(display-time-mode t)
 '(global-hl-line-mode nil nil (hl-line))
 '(haskell-program-name "ghci \"+.\"")
 '(haskell-refac-chasePaths (quote ("/home/sigurd/haskellleg" "/home/sigurd/haskellleg/HaRe_20012006/tools/base/tests/HaskellLibraries" "/usr/lib/ghc-6.4.1")))
 '(ido-enable-flex-matching t)
 '(ido-rotate-file-list-default t)
 '(ido-use-filename-at-point (quote guess))
 '(ido-use-virtual-buffers t)
 '(indent-tabs-mode nil)
 '(isa-isatool-command "~/bin/isabelle")
 '(isar-shortcut-alist (quote (("|}" . "\342\246\204") ("{|" . "\342\246\203") ("/\\\\" . "\342\213\200") ("``" . "\302\264") ("\\/" . "\342\210\250") ("/\\" . "\342\210\247") ("+O" . "\342\212\225") ("-O" . "\342\212\226") ("xO" . "\342\212\227") ("/O" . "\342\212\230") (".O" . "\342\212\231") ("|+" . "\342\200\240") ("|++" . "\342\200\241") ("<=" . "\342\211\244") ("|-" . "\342\212\242") (">=" . "\342\211\245") ("-|" . "\342\212\243") ("||" . "\342\210\245") ("==" . "\342\211\241") ("~=" . "\342\211\240") ("~:" . "\342\210\211") ("~~~" . "\342\211\215") ("~~" . "\342\211\210") ("~==" . "\342\211\205") ("|<>|" . "\342\213\210") ("|=" . "\342\212\250") ("=." . "\342\211\220") ("_|_" . "\342\212\245") ("</" . "\342\211\256") (">=/" . "\342\211\261") ("=/" . "\342\211\240") ("==/" . "\342\211\242") ("~/" . "\342\211\201") ("~=/" . "\342\211\204") ("~~/" . "\342\211\211") ("~==/" . "\342\211\207") ("<-" . "\342\206\220") ("->" . "\342\206\222") ("=>" . "\342\207\222") ("<->" . "\342\206\224") ("<=>" . "\342\207\224") ("|->" . "\342\206\246") ("<--" . "\342\237\265") ("<==" . "\342\237\270") ("-->" . "\342\237\266") ("==>" . "\342\237\271") ("<==>" . "\342\237\267") ("|-->" . "\342\237\274") ("<-->" . "\342\237\267") ("<<" . "\302\253") ("[|" . "\342\237\246") (">>" . "\302\273") ("|]" . "\342\237\247") ("---" . "\342\200\224") ("\\nat" . "\342\204\225") ("\\int" . "\342\204\244") ("\\rat" . "\342\204\232") ("\\real" . "\342\204\235") ("\\complex" . "\342\204\202") ("\\euro" . "\342\202\254") ("\\yen" . "\302\245") ("\\cent" . "\302\242"))))
 '(isar-unicode-tokens-enable t)
 '(isar-x-symbol-enable nil)
 '(ispell-program-name "aspell" t)
 '(load-home-init-file t t)
 '(lpr-command "kprinter")
 '(nxml-slash-auto-complete-flag t t)
 '(org-agenda-files (quote ("~/Dropbox/org/main.org")))
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("marmelade" . "http://marmalade-repo.org/packages/"))))
 '(python-python-command "python")
 '(safe-local-variable-values (quote ((TeX-master . rapport) (TeX-master . "rapport"))))
 '(scheme-program-name "guile")
 '(semantic-c-dependency-system-include-path (quote ("/usr/include" "/opt/local/include")))
 '(server-temp-file-regexp "^/tmp/Re\\|/draft$\\|itsalltext/")
 '(smiley-regexp-alist nil)
 '(speedbar-directory-unshown-regexp "^\\(CVS\\|RCS\\|SCCS\\|_DARCS\\|\\..*\\)\\'" t)
 '(speedbar-use-images nil t)
 '(spell-command "aspell")
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added ((t (:foreground "Green"))))
 '(diff-removed ((t (:foreground "Red"))))
 '(flymake-errline ((((class color)) (:background "DarkRed"))))
 '(flymake-warnline ((((class color)) (:background "DarkBlue"))))
 '(org-block ((t (:inherit shadow :family "Monaco")))))
