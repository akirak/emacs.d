(require 'whitespace)
(diminish 'whitespace-mode)
(setq whitespace-display-mappings
      ;; all numbers are Unicode codepoint in decimal. try (insert-char 182 ) to see it
      '(
        (space-mark 32 [183] [46]) ; 32 SPACE, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
        (newline-mark 10 [182 10]) ; 10 LINE FEED
        (tab-mark 9 [187 9] [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
        ))
(setq whitespace-style '(face tabs trailing tab-mark))
(add-hook 'prog-mode-hook 'whitespace-mode)

(provide 'init-whitespace)
