;;;
;;----------------------------------------------------------------------
;; grail-fn.el
;; Primary Author: Mike Mattie
;; Copyright (C) 2008 Mike Mattie
;; License: LPGL-v3
;;----------------------------------------------------------------------

;; definitions that are essential to the Emacs boot. This was split
;; from my general utility collection so that the risk of introducing
;; bugs/complexity early in the boot process could be minimized.

;;----------------------------------------------------------------------
;; utilities.
;;----------------------------------------------------------------------

(defun list-filter-nil ( list )
  "Filter nil symbols from a list"
  (remq 'nil list))

(defun seq-filter-nil ( &rest list-seq )
  "Filter nil symbols from a sequence."
  (list-filter-nil list-seq))

(defun map-filter-nil ( func &rest seq )
  "map-filter-nil FUNC LIST

   Filter the nil elements of LIST from the input and output of
   function FUNC.

   FUNC is applied to the non-nil elements of SEQ ala mapcar. The
   result is either a list or nil if filtering eliminated all
   output."
  (lexical-let
    ((rvalue nil))

    (dolist (element seq)
      (when element
        (lexical-let
          ((transform (funcall func element)))
          (when transform
            (push transform rvalue)))))
    (reverse rvalue)))

;;----------------------------------------------------------------------
;; styles
;;----------------------------------------------------------------------

(defun load-style ( style-name )
  (lexical-let*
    ((load-name  (concat grail-local-styles style-name ".el"))
     (style-file (file-if-readable load-name)))

    (if style-file
      (robust-load-elisp style-file)
      (progn
        (message "load-style (loader-fn.el): could not load style %s from: %s\n"
          style-name
          load-name)
        nil)) ))

(defvar requested-styles-list
  nil
  "List of styles requested by the user.")

(defun load-requested-styles ()
  (mapc 'load-style
    requested-styles-list))

(defun use-styles ( &rest request-list )
  (mapc
    (lambda ( name )
      (push name requested-styles-list))
    request-list))

;;----------------------------------------------------------------------
;; filter-ls: a general purpose tools for filtering directory listings.
;;----------------------------------------------------------------------

(defun filter-ls-predicate ( attr-name attr-match )
  "create predicate filters for path/mode values"
  (cond
    ((string-equal "type" attr-name) `(char-equal ,attr-match  (aref (cdr path-pair) 0)))
    ((string-equal "path" attr-name) `(string-match ,attr-match (car path-pair)))
    ((string-equal "name" attr-name) `(string-match ,attr-match (file-name-nondirectory (car path-pair)))) ))

(defun filter-ls-attributes ( filter-form )
  "implement the various attribute filters for the filter-ls form"
  (lexical-let
    ((attr-name (symbol-name (car filter-form)))
      (attr-match (cadr filter-form)))

    (if (char-equal ?! (aref attr-name 0))
      (list 'not (filter-ls-predicate (substring attr-name 1) attr-match))
      (filter-ls-predicate attr-name attr-match))
    ))

(defmacro filter-ls (path path-type &rest filters)
  "filter-ls PATH PATH-TYPE
  a form for flexibly filtering the result of listing a directory with attributes

   t   absolute paths
   nil relative paths"
  `(apply 'map-filter-nil
     (lambda ( path-pair )
       (if ,(cons 'and (mapcar 'filter-ls-attributes filters))
         (car path-pair)))

     ;; reduce the attributes to a pair of the path, and the mode string
     (mapcar (lambda ( attr-list )
               (cons (car attr-list) (nth 9 attr-list)))
       ;; get the list of files.
       (directory-files-and-attributes ,path ,path-type)) ))

;;----------------------------------------------------------------------
;; elpa
;;----------------------------------------------------------------------

(defconst elpa-url
  "http://tromey.com/elpa/")

(defun load-elpa-when-installed ()
  (when (and
         (file-accessible-directory-p grail-dist-elpa)
         (load-elisp-if-exists (concat grail-dist-elpa "package.el")))
    (setq-default package-user-dir grail-dist-elpa)
    (push grail-dist-elpa package-directory-list)
    (package-initialize)))

(defun grail-install-elpa ()
  (interactive)
  (condition-case nil
      (progn
        (make-directory grail-dist-elpa t)

        (with-temp-buffer
          (url-insert-file-contents (concat elpa-url "package.el"))
          (write-file (concat grail-dist-elpa "package.el")))

        (load-elpa-when-installed))

    (error
     (progn
       (message "ELPA installation failed.\n")
       nil)) ))
