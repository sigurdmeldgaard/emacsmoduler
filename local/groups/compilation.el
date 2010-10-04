(setq compilation-directory-matcher
      `( , (rx 
           (or (seq (regexp "\\(?:Entering\\|Leavin\\(g\\)\\) directory `\\(.+\\)'$"))
               (seq (* (not (any "[]")))
                    (regexp "\\]")
                    (* space)
                    (group (+ (* alnum) "/"))
                    (group line-start "[")))) (2 . 1) (3 . nil) (4 . 4)))

"\\(?:\\(?:Entering\\|Leavin\\(g\\)\\) directory`\\(.+\\)'$\\)\\|\\(?:[^]^[]*\\][[:space:]]*\\(\\(?:[[:alnum:]]*/\\)+\\)\\)\\|\\(?:^\\(\\[\\)\\)"
"\\(?:\\(?:\\(?:Entering\\|Leavin\\(g\\)\\) directory `\\(.+\\)'$\\)\\|[^][]*[[:space:]]*\\(\\(?:[[:alnum:]]*/\\)+\\)\\)\\(^\\[\\)"
