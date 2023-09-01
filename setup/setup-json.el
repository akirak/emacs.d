(use-package json-mode
  :mode (("\\.bowerrc$" . json-mode)
         ("\\.jshintrc$" . json-mode)
         ("\\.json_schema$" . json-mode)
         ("\\.json\\'" . akirak/maybe-json-mode))
  :config
  (defun akirak/maybe-json-mode ()
    "Turn on `json-mode' unless the file is larger than 50 KB."
    (interactive)
    (if (< (buffer-size) (* 50 1024))
        (json-mode)
      (buffer-disable-undo)
      (fundamental-mode))))

(use-package counsel-jq
  :straight (counsel-jq :host github :repo "200ok-ch/counsel-jq")
  :config
  (akirak/bind-mode :package 'json-mode :keymaps 'json-mode-map
    "q" #'counsel-jq))

(provide 'setup-json)
