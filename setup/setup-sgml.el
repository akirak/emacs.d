
(use-package sgml-mode
  :straight (:type built-in)
  :config
  ;; For commands in sgml-mode see https://www.gnu.org/software/emacs/manual/html_node/emacs/HTML-Mode.html
  (akirak/bind-generic :keymaps 'sgml-mode-map :package 'sgml-mode
    "ic" #'sgml-name-char
    "ia" #'sgml-attributes
    "kt" #'sgml-delete-tag))

(use-package html-mode
  :straight (:type built-in))

(provide 'setup-sgml)
