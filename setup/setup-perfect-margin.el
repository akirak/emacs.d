(use-package perfect-margin
  :straight (:host github :repo "akirak/perfect-margin")
  :if (window-system)
  :config

  (defcustom akirak/perfect-margin-threshold 170
    "FIXME"
    :type 'number)

  (defsubst akirak/perfect-margin-threshold-p ()
    (> (frame-width) akirak/perfect-margin-threshold))

  (defun akirak/perfect-margin-conditionally (&rest _args)
    (when (frame-size-changed-p)
      (if (akirak/perfect-margin-threshold-p)
          (if perfect-margin-mode
              (perfect-margin-margin-windows)
            (perfect-margin-mode 1))
        (when perfect-margin-mode
          (perfect-margin-mode -1)))))

  (advice-add 'perfect-margin-margin-frame
              :override
              #'akirak/perfect-margin-conditionally)

  (add-hook 'window-size-change-functions
            #'akirak/perfect-margin-conditionally)

  (perfect-margin-mode (or (akirak/perfect-margin-threshold-p) -1))

  :custom
  (perfect-margin-visible-width 92)
  (perfect-margin-ignore-modes '(exwm-mode
                                 doc-view-mode
                                 pdf-view-mode
                                 nov-mode
                                 vterm-mode
                                 html-mode
                                 magit-repolist-mode
                                 magit-submodule-list-mode
                                 ;; For log files
                                 fundamental-mode))
  (perfect-margin-ignore-regexps `("^minibuf" "^[*]"
                                   "^ \\*LV\\*"
                                   "^ \\*which-key\\*")))

(provide 'setup-perfect-margin)
