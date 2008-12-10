(defun linux-c-mode ()
  "C mode with adjusted defaults for use with Paolo!"
  (interactive)
  (c-mode)
  (c-set-style "linux")
  (setq tab-width 8)
  (setq indent-tabs-mode t)
  (setq c-basic-offset 8)
  (yas/define 'c-mode "LABEL" "LABEL_BEGIN(${1:OPCODE_NAME});
    $0
    LABEL_END($1);" "LABEL_BEGIN(OPCODE) ...")
  (yas/define 'c-mode "pr" "printf(\"Printing %s:%d %d$0\\n\", __FILE__, __LINE__ $1);"
	      "printf(\"...\\n\");"))

(add-to-list 'auto-mode-alist
	     '(".*/micropython.*/.*\\.[ch]$"
	       . linux-c-mode))