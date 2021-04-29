(use-package string-inflection
  :general
  (:keymaps 'akirak/generic-prefix-map
            "." #'string-inflection-cycle-repeat
            "c" #'akirak/string-inflection-hydra/body)
  :config
  (defrepeater 'string-inflection-cycle)
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
    ("j" mozc-temp-convert "mozc" :exit t)
    ("p" pyim-convert-string-at-point "pyim" :exit t)
    ("q" nil "quit" :exit t)))

(provide 'setup-string-inflection)
