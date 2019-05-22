;;; setup-exec-path.el --- Hack exec-path against direnv -*- lexical-binding: t -*-

(defun akirak/with-default-exec-path (orig &rest args)
  (let ((exec-path (default-value 'exec-path)))
    (apply orig args)))

(advice-add #'direnv--detect :around #'akirak/with-default-exec-path)
(advice-add #'vc-git-command :around #'akirak/with-default-exec-path)
(advice-add #'magit-process-file :around #'akirak/with-default-exec-path)

(provide 'setup-exec-path)
