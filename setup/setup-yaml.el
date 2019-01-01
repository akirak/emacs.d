;;; setup-yaml.el --- Support for YAML -*- lexical-binding: t -*-

;; Stolen most of the configuration in Spacemacs:
;; https://github.com/syl20bnr/spacemacs/blob/master/layers/+lang/yaml/packages.el
(use-package yaml-mode
  :mode (("\\.\\(yml\\|yaml\\)\\'" . yaml-mode)
         ("Procfile\\'" . yaml-mode))
  :general
  (:keymaps 'yaml-mode-map
            "C-m" 'newline-and-indent)
  :hook
  (yaml-mode . (lambda () (flycheck-mode 1))))

(provide 'setup-yaml)
;;; setup-yaml.el ends here
