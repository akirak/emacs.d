;;; setup-yaml.el --- Support for YAML -*- lexical-binding: t -*-

;; Stolen most of the configuration in Spacemacs:
;; https://github.com/syl20bnr/spacemacs/blob/master/layers/+lang/yaml/packages.el
(use-package yaml-mode
  :mode (("\\.\\(yml\\|yaml\\)\\'" . yaml-mode)
         ("Procfile\\'" . yaml-mode))
  :general
  (:keymaps 'yaml-mode-map
            "C-m" 'newline-and-indent)
  :config
  (defun akirak/maybe-setup-hpack ()
    "Set up a hook for hpack if the file is package.yaml"
    (when (ignore-errors
            (string-suffix-p "/package.yaml" (buffer-file-name)))
      (add-hook 'after-save-hook #'akirak/hpack 'append 'local)))
  :hook
  (yaml-mode . akirak/maybe-setup-hpack))

;; Also see setup-ansible.el

(provide 'setup-yaml)
;;; setup-yaml.el ends here
