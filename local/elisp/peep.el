; Joos Peep-hole-mode for emacs, feel free to use and modify and whatever
; By Sigurd Meldgaard stm@daimi.au.dk
; V. 0.1

(defvar peep-mode-hook nil)

(defvar peep-mode-map (make-keymap) "Keymap for peep major mode")

(define-key peep-mode-map "\C-c\C-p" 'peep-pattern-skeleton)


(define-skeleton peep-pattern-skeleton
      ""
      \n "pattern " _ " x:" \n 
      "x ~"\n
      " -> 0 " \n )


(defun peep-indent-line ()
  "Indent current line as peep code"
  (interactive)
  (let ((opoint (point)))
    (beginning-of-line)
    (cond ((bobp) 
           (indent-line-to 0))
          ((looking-at "^[ \t]*pattern [a-z0-9_]* [a-z0-9_]*:")
           (indent-line-to 0))
          ((looking-at "^[ \t]*.*\\(&&\\|||\\)")
           (indent-line-to 5))
          ((looking-at "^[ \t]*//")
           (indent-line-to 0))
          ((looking-at "^[ \t]*\\([a-z0-9_]*[ ]?:\\)")
           (indent-line-to (- 7
                              (- (match-end 1) (match-beginning 1)))))
          ((looking-at "^[ \t]*\\([a-z0-9_]* ~\\)")
           (indent-line-to (- 7
                              (- (match-end 1) (match-beginning 1)))))
          ((looking-at "^[ \t]*\\(-> [0-9]*\\)")
           (indent-line-to (- 7
                              (- (match-end 1) (match-beginning 1)))))
          (t (indent-line-to 8)))))
    
(defvar peep-mode-syntax-table (make-syntax-table) "Syntax table for peep-mode")

(modify-syntax-entry ?_ "w" peep-mode-syntax-table)
(modify-syntax-entry ?/ ". 124b" peep-mode-syntax-table)
(modify-syntax-entry ?* ". 23" peep-mode-syntax-table)
(modify-syntax-entry ?\n "> b" peep-mode-syntax-table)

(defvar peep-font-lock-keywords nil)

(let ((peep-keywords (concat "\\<\\("
                             (regexp-opt '("aaload" "aastore" "aconst_null" "aload"
                                           "areturn" "arraylength" "astore" "athrow"
                                           "baload" "bastore" "caload"
                                           "castore" "checkcast" "dup" "dup2" "dup_x1"
                                           "dup_x2" "getfield" "getstatic" "goto" "i2b"
                                           "i2c" "i2s" "iadd" "iaload" "iand"
                                           "iastore" "idiv" "if" "ifcmp" "iinc"
                                           "iload" "imul" "inc" "ineg" "instanceof"
                                           "invokeinterface" "invokespecial" "invokestatic" "invokevirtual"
                                           "ior" "irem" "ireturn" "istore"
                                           "isub" "ixor" "label" "ldc_int"
                                           "ldc_string" "multianewarray" "new" "nop"
                                           "nstruction" "pop" "putfield" "putstatic"
                                           "return" "saload" "sastore" "swap" "pattern"))
                             "\\)\\>"))
      
      (peep-operators (regexp-opt '(":" "->" "&&" "||" "+" "==" "-" "*")))
      
      (peep-comparisons (concat "\\<\\(" (regexp-opt '("eq" "aeq" "ne" "ane" "lt" "gt" "le" "ge")) "\\)\\>"))) 
 
  (setq peep-font-lock-keywords
	(list `("//.*"
		. font-lock-comment-face)
	      `("^[\t ]*:.*"
		. font-lock-type-face)
	      `(,peep-operators
		. font-lock-keyword-face)
	      `("^[\t ]*\\*.*"
		. font-lock-doc-string-face)
	      `(,peep-keywords
                  . font-lock-function-name-face)
	      '("\\<\\d*\\>"
		. font-lock-constant-face)
	      `(,peep-comparisons
		. font-lock-builtin-face))))


(define-derived-mode peep-mode fundamental-mode "Peep"
  "Major mode for editing joos peephole files."
  
  (set-syntax-table peep-mode-syntax-table)
  (use-local-map peep-mode-map)
  
  (setq comment-start "// "
        comment-end   ""
              comment-start-skip "// *")
  
  
  (set (make-local-variable 'font-lock-defaults) '(peep-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'peep-indent-line)  
  
  (font-lock-mode)
  (run-hooks 'peep-mode-hook))
  
(add-to-list 'auto-mode-alist '("\\.peep\\'" . peep-mode))

(provide 'peep)


