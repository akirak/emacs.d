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

(add-to-list 'counsel-outline-settings
             `(elixir-mode
               :outline-regexp
               ,(rx (seq (* space)
                         (group (or (sequence "def" (* (not space)))
                                    (sequence "@" (* (not space)))
                                    "alias"
                                    "import"
                                    "require"
                                    "use"
                                    "test"
                                    "doctest"
                                    "setup_all"))
                         (+ space)
                         (group (*? any))
                         (or ", do:" " do" eol)))
               :outline-level
               (lambda ()
                 (pcase (match-string 1)
                   ("defmodule" 1)
                   (_ 2)))
               :outline-title
               (lambda ()
                 (let ((s1 (match-string 1))
                       (s2 (match-string 2)))
                   (pcase s1
                     ((pred (string-prefix-p "@")) s1)
                     ("defstruct" s1)
                     ((pred (string-prefix-p "def")) s2)
                     (_ (concat s1 " " s2)))))))

(provide 'setup-elixir)
