;;; setup-yaml.el --- Support for YAML -*- lexical-binding: t -*-

;; Stolen most of the configuration in Spacemacs:
;; https://github.com/syl20bnr/spacemacs/blob/master/layers/+lang/yaml/packages.el
(use-package yaml-mode
  :mode (("\\.\\(yml\\|yaml\\)\\'" . yaml-mode)
         ("Procfile\\'" . yaml-mode))
  :general
  (:keymaps 'yaml-mode-map
            "C-m" 'newline-and-indent))

;; To activate this minor mode in specific projects, add the following
;; configuration to =.dir-locals.el=:
;;
;; #+begin_src emacs-lisp
;; ((yaml-mode
;;   (mode . ansible)))
;; #+end_src
(use-package ansible
  :disabled t
  :commands (ansible)
  :init
  (defalias 'ansible-mode 'ansible))

(provide 'setup-yaml)
;;; setup-yaml.el ends here
