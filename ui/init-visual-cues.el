(use-package highlight-indent-guides
  :init
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package fill-column-indicator
  :init
  (add-hook 'prog-mode-hook 'fci-mode))

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

(use-package git-gutter
  :diminish git-gutter-mode
  :init
  (global-git-gutter-mode))

(use-package dimmer
  :init
  (dimmer-mode 1)
  :custom
  (dimmer-exclusion-regexp "\\(\\*Help\\*\\|\\*helm\\)"))

(provide 'init-visual-cues)
