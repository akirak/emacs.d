(use-package tramp
  :straight (:type built-in)

  :config
  (setq akirak/remote-process-forbidding-functions
        '(dired-k--inside-git-repository-p
          git-gutter:in-repository-p))

  (defun akirak/remote-allow-process-p (&rest _args)
    (not (when-let (remote (file-remote-p default-directory))
           (string-prefix-p (rx bol "/rclone:")
                            remote))))

  (dolist (func akirak/remote-process-forbidding-functions)
    (advice-add func
                :before-while #'akirak/remote-allow-process-p)))

(provide 'setup-tramp)
