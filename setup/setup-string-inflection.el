(use-package string-inflection
  :general
  (:keymaps 'akirak/generic-prefix-map
            "." (defrepeater 'string-inflection-cycle)
            "c" #'akirak/string-inflection-hydra/body)
  :config
  (akirak/bind-generic :keymaps 'java-mode-map
    "." (defrepeater 'string-inflection-java-style-cycle))
  (akirak/bind-generic :keymaps 'python-mode-map
    "." (defrepeater 'string-inflection-python-style-cycle))
  (defhydra akirak/string-inflection-hydra (:hint nil)
    "
string inflection
[_C_] CamelCase        [_-_] lisp-case
[_c_] lowerCamelcase   [_\__] under_score
[_u_] UPCASE           [_=_] Capital_Underscore
"
    ("_" string-inflection-underscore)
    ("C" string-inflection-camelcase)
    ("c" string-inflection-lower-camelcase)
    ("-" string-inflection-lisp)
    ("=" string-inflection-capital-underscore)
    ("u" string-inflection-upcase)
    ("SPC" string-inflection-all-cycle "all cycle")
    ("q" nil "quit" :exit t)))

(provide 'setup-string-inflection)
