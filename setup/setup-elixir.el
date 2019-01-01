;; Alchemist package for Elixir support on Emacs
;; See https://alchemist.readthedocs.io/en/latest/configuration/ for setup
(use-package alchemist
  :general
  (:keymaps 'elixir-mode-map
            "C-c d s" 'alchemist-server-start)
  :custom
  (alchemist-key-command-prefix "C-c ,"))

(provide 'setup-elixir)
