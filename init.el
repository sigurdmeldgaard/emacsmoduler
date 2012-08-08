(package-initialize)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(dolist (package '(yasnippet yas-jit yasnippet-bundle
       expand-region undo-tree sr-speedbar
       solarized-theme smex parenface paredit
       org mic-paren color-theme-solarized
       color-theme-sanityinc-solarized color-theme
       auctex all rainbow-delimiters magit mark-multiple
       mark-more-like-this ido-ubiquitous haskell-mode))
  (unless (package-installed-p package)
    (package-install package)))


(defvar dist-elisp (concat emacsmoduler-path "/dist/elisp/"))
(defvar local-elisp (concat emacsmoduler-path "/local/elisp/"))

(add-to-list 'load-path dist-elisp)
(add-to-list 'load-path local-elisp)

(cond
 ((daemonp) ())
 ((window-system) 
    (load "gui")))

(load
 (cond
  ((string-equal "gnu/linux" system-type)  "linux.el")
  ((string-equal "darwin"    system-type)  "darwin.el")
  ((string-equal "windows-nt"   system-type)  "windows.el")))

(load "byte-code-cache")
;;; Settings ====================
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

(ido-ubiquitous-mode 1)

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


(require 'dbus)

(defun th-evince-sync (file linecol)
  (message "hej")
  (let ((buf (get-buffer file))
        (line (car linecol))
        (col (cadr linecol)))
    (if (null buf)
        (message "Sorry, %s is not opened..." file)
      (switch-to-buffer buf)
      (goto-line (car linecol))
      (unless (= col -1)
        (move-to-column col)))))

(setq org-agenda-files '("~/Dropbox/org/main.org"))

(dbus-register-signal
 :session nil "/org/gnome/evince/Window/0"
 "org.gnome.evince.Window" "SyncSource"
 'th-evince-sync)


(require 'fixpath)

(load "commands")
(load "loadfiles")
(load "keys")
(load "small")

(provide 'init)
