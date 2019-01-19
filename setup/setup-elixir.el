;; Alchemist package for Elixir support on Emacs
;; See https://alchemist.readthedocs.io/en/latest/configuration/ for setup
(use-package alchemist)

(defconst akirak/alchemist-key-command-prefix "C-,")
(general-setq alchemist-key-command-prefix
              (kbd akirak/alchemist-key-command-prefix))

(general-define-key :keymaps 'alchemist-mode-map
                    :package 'alchemist
                    :prefix akirak/alchemist-key-command-prefix
                    "c" '(nil :wk "compile")
                    "e" '(nil :wk "execute")
                    "f" '(nil :wk "point")
                    "m" '(nil :wk "mix")
                    "mt" '(nil :wk "mix-test")
                    "X" '(nil :wk "hex")
                    "h" '(nil :wk "help")
                    "p" '(nil :wk "project")
                    "i" '(nil :wk "iex")
                    "v" '(nil :wk "eval")
                    "o" '(nil :wk "macroexpand"))

(akirak/which-key-add-stripped-prefix "alchemist-")

(provide 'setup-elixir)
