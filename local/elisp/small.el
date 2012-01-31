(defun isearch-forward-current-word-keep-offset ()
  "Mimic vi search foward at point feature."
  (interactive)
  (let ((re-curword) (curword) (offset (point))
        (old-case-fold-search case-fold-search) )
    (setq curword (thing-at-point 'symbol))
    (setq re-curword (concat "\\<" (thing-at-point 'symbol) "\\>") )
    (beginning-of-thing 'symbol)
    (setq offset (- offset (point))) ; offset from start of symbol/word
    (setq offset (- (length curword) offset)) ; offset from end
    (forward-char)
    (setq case-fold-search nil)
    (if (re-search-forward re-curword nil t)
      ;; else
        (backward-char offset)
      (progn (goto-char (point-min))
             (if (re-search-forward re-curword nil t)
                 (progn (message "Searching from top. %s" (what-line))
                        (backward-char offset))
               ;; else
               (message "Searching from top: Not found"))))
    (setq case-fold-search old-case-fold-search)))

(defun isearch-backward-current-word-keep-offset ()
  "Mimic vi search backwards at point feature."
  (interactive)
  (let ((re-curword) (curword) (offset (point))
        (old-case-fold-search case-fold-search) )
    (setq curword (thing-at-point 'symbol))
    (setq re-curword (concat "\\<" curword "\\>") )
    (beginning-of-thing 'symbol)
    (setq offset (- offset (point))) ; offset from start of symbol/word
    (forward-char)
    (setq case-fold-search nil)
    (if (re-search-backward re-curword nil t)
        (forward-char offset)
      ;; else
      (progn (goto-char (point-max))
             (if (re-search-backward re-curword nil t)
                 (progn (message "Searching from bottom. %s" (what-line))
                        (forward-char offset))
               ;; else
               (message "Searching from bottom: Not found"))))
    (setq case-fold-search old-case-fold-search)))


(defun my-inferior-erlang (name)
  "Run an inferior Erlang.
This is just like running Erlang in a normal shell, except that
an Emacs buffer is used for input and output.
\\<comint-mode-map>
The command line history can be accessed with  \\[comint-previous-input]  and  \\[comint-next-input].
The history is saved between sessions.

Entry to this mode calls the functions in the variables
`comint-mode-hook' and `erlang-shell-mode-hook' with no arguments.

The following commands imitate the usual Unix interrupt and
editing control characters:
\\{erlang-shell-mode-map}"
  (interactive "M")
  (require 'comint)
  (let ((opts inferior-erlang-machine-options))
    (cond ((eq inferior-erlang-shell-type 'oldshell)
           (setq opts (append (list  "-sname" name "-oldshell") opts)))
          ((eq inferior-erlang-shell-type 'newshell)
           (setq opts (append (list "-newshell" "-env" "TERM" "vt100" "-sname" name) opts))))
    (setq inferior-erlang-buffer
          (apply 'make-comint
                 inferior-erlang-process-name inferior-erlang-machine
                 nil opts)))
  (setq inferior-erlang-process
        (get-buffer-process inferior-erlang-buffer))
  (process-kill-without-query inferior-erlang-process)
  (switch-to-buffer inferior-erlang-buffer)
  (if (and (not (eq system-type 'windows-nt))
           (eq inferior-erlang-shell-type 'newshell))
      (setq comint-process-echoes t))
  ;; `rename-buffer' takes only one argument in Emacs 18.
  (condition-case nil
      (rename-buffer (concat inferior-erlang-buffer-name name) t)
    (error (rename-buffer inferior-erlang-buffer-name)))
  (erlang-shell-mode))

(defun open-my-way (arg)
  "opens a new line under the selected, and moves cursor down there"
  (interactive "p")
  (end-of-line)
  (newline-and-indent))

(defun make-test-case (filename)
  "Creates a small java-program"
  (interactive "F")
  (find-file filename)
  (string-match "/\\([^./]*\\).java" filename)
  (let ((classname (substring filename(match-beginning 1) (match-end 1))))
    (insert "public class " classname
            "{\n    public " classname
            "(){}\n    public int test(){\n        return 123;\n    }\n}\n" )))

(defun goto-matching-paren ()
  (interactive)
  (let (pos dir mismatch face (oldpos (point)))

    (cond ((eq (char-syntax (preceding-char)) ?\))
           (setq dir -1))
          ((eq (char-syntax (following-char)) ?\()
           (setq dir 1)))
    (when dir
      (save-excursion
        (save-restriction
          (when blink-matching-paren-distance
            (narrow-to-region
             (max (point-min) (- (point) blink-matching-paren-distance))
             (min (point-max) (+ (point) blink-matching-paren-distance))))
          (condition-case  ()
              (setq pos (scan-sexps (point) dir))
            (error (setq pos t mismatch t)))))
      (goto-char pos))))

(defun toggle-current-word ()
  "Toggles case at the beginning of the current word"
  (interactive "")
  (save-excursion
    (let* ((word-beginning (car (bounds-of-thing-at-point 'word)))
           (s word-beginning)
           (e (+ s 1))
           (a (buffer-substring s e)))
      (if (equal a (upcase a))
          (downcase-region s e)
          (upcase-region s e)))))

(defun make-containers (n)
  (interactive "p")
  (random t)
  (let ((i 0))
    (while (< i n)
      (insert (format "INSERT INTO Containers values(%d, %d, %d, '%s', null);\n"
                      i
                      (random 2)
                      (+ 1 (random 30))
                      (aref ["Olsen"
                             "Mikkelsen"
                             "Carlsen"
                             "Hogersen"
                             "Rasmussen"] (random 5))))
      (setq i (+ i 1)))))

(defun undansk ()
  (interactive"")
  (rep "æ" "ae")
  (rep "ø" "oe")
  (rep "å" "aa")
  (rep "Æ" "Ae")
  (rep "Ø" "Oe")
  (rep "Å" "Aa"))

(global-set-key "\C-c\C-f"
                (lambda ()
                  "Toggles font-size"
                  (interactive "")
                  (setq small-lettersp (not small-lettersp))
                  (if small-lettersp
                      (set-face-font 'default "-*-fixed-medium-r-*-*-12-*-*-*-*-*-iso8859-*")
                    (set-face-font 'default "-*-fixed-medium-r-*-*-20-*-*-*-*-*-iso8859-*"))))
(setq small-lettersp t)

(defun rep (from to)
  (save-excursion
    (beginning-of-buffer)
    (while (search-forward from nil t)
      (replace-match to nil t))))

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))

    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn
        (copy-file filename newname 1)
        (delete-file filename)
        (set-visited-file-name newname)
        (set-buffer-modified-p nil)
        t))))


(defun dos-to-unix-eol (fpath)
  (interactive)
  (while (re-search-forward "\r\n" nil t)
    (replace-match "\n" nil nil)))


(defun dos-to-unix-eol (fpath)
  "Change file's line ending to unix convention."
    (let ((coding-system-for-read 'dos)
	  (coding-system-for-write 'unix))
      (let ((mybuffer (find-file fpath)))
        (save-buffer)
        (kill-buffer mybuffer))))

(defun dired-dos2unix-marked-files ()
  "Change to unix line ending for marked (or next arg) files."
  (interactive)
  (mapc 'dos-to-unix-eol (dired-get-marked-files)))


