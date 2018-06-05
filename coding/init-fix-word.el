(use-package fix-word
  :commands (fix-word-upcase fix-word-downcase fix-word-capitalize)
  :general
  ("M-u" 'fix-word-upcase)
  ("M-l" 'fix-word-downcase)
  ("M-c" 'fix-word-capitalize))

(provide 'init-fix-word)
