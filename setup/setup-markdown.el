(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\(own\\)\\'" . gfm-mode)
         ("\\.mdx\\'" . markdown-mode))
  :general
  (:keymaps 'markdown-mode-map
            "C-c '" #'markdown-edit-code-block)
  :custom
  (markdown-command "pandoc -f gfm -t html"))

(provide 'setup-markdown)
