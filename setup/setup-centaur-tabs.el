(use-package centaur-tabs
  :config
  (defun akirak/centaur-tabs-hide-tab (name)
    ;; Hide if the major mode is not a derived mode
    (let* ((buffer (get-buffer name))
           (mode (buffer-local-value 'major-mode buffer)))
      (not (get mode 'derived-mode-parent))))
  (centaur-tabs-mode t)
  :general
  ("C-c [" (defrepeater 'centaur-tabs-backward-tab))
  ("C-c {" 'centaur-tabs-backward-tab-other-window)
  ("C-c ]" (defrepeater 'centaur-tabs-forward-tab))
  ("C-c }" 'centaur-tabs-forward-tab-other-window)
  :custom
  (centaur-tabs-hide-tab-function 'akirak/centaur-tabs-hide-tab)
  (centaur-tabs-style "bar")
  (centaur-tabs-set-close-button nil)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "*"))

(provide 'setup-centaur-tabs)
