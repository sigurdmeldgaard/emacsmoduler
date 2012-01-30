(global-undo-tree-mode)

(when (featurep 'mule)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'latin-1)
  (set-language-environment 'utf-8)
  (server-start))

(if (locate-library "edit-server")
    (progn
      (require 'edit-server)
      (setq edit-server-new-frame nil)
      (edit-server-start)))

(defconst use-backup-dir t)
(setq backup-directory-alist (quote ((".*" . "~/backup/temp/")))
      version-control t                ; Use version numbers for backups
      kept-new-versions 5             ; Number of newest versions to keep
      kept-old-versions 2              ; Number of oldest versions to keep
      delete-old-versions t            ; Ask to delete excess backup versions?
      backup-by-copying-when-linked t) ; Copy linked files, don't rename.

(setq tramp-default-method "scp")

(setq browse-url-browser-function 'browse-url-generic)

(setq ispell-program-name "aspell")

(defun ido-find-tag ()
  "Find a tag using ido"
  (interactive)
  (tags-completion-table)
  (let (tag-names)
    (mapc (lambda (x)
            (unless (integerp x)
              (push (prin1-to-string x t) tag-names)))
          tags-completion-table)
    (find-tag (ido-completing-read "Tag: " tag-names nil nil (thing-at-point 'symbol)))))

;Hacks to make AUCtex happy
(setq byte-compile-verbose t)
(setq byte-compile-warnings t)
(setq TeX-save-query nil) ;;autosave before compiling



(require 'fixpath)
