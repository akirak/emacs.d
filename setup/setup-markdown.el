(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode))
  :ensure-system-package pandoc
  :custom
  (markdown-command "pandoc -f gfm -t html"))

(provide 'setup-markdown)
