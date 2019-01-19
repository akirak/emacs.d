;; Alchemist package for Elixir support on Emacs
;; See https://alchemist.readthedocs.io/en/latest/configuration/ for setup
(defconst akirak/alchemist-key-command-prefix "C-,")

(use-package alchemist
  :after elixir-mode
  :general
  (:keymaps 'alchemist-mode-map
            :prefix akirak/alchemist-key-command-prefix
            "c" '(:ignore t :wk "compile")
            "e" '(:ignore t :wk "execute")
            "f" '(:ignore t :wk "point")
            "m" '(:ignore t :wk "mix")
            "mt" '(:ignore t :wk "mix-test")
            "X" '(:ignore t :wk "hex")
            "h" '(:ignore t :wk "help")
            "p" '(:ignore t :wk "project")
            "i" '(:ignore t :wk "iex")
            "v" '(:ignore t :wk "eval")
            "o" '(:ignore t :wk "macroexpand"))
  :custom
  (alchemist-key-command-prefix (kbd akirak/alchemist-key-command-prefix)))

(akirak/which-key-add-stripped-prefix "alchemist-")

(provide 'setup-elixir)
