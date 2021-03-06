(require 'my/file/test)

(akirak/bind-file-extra
  "c" #'revert-buffer-with-coding-system
  ;; "S" #'sudo-find-file
  "S" #'su-mode
  "D" #'crux-delete-file-and-buffer
  "R" #'crux-rename-file-and-buffer
  "M" #'set-file-modes
  "X" #'executable-set-magic
  "!" #'crux-open-with
  "t"
  (defun akirak/find-impl-or-test-file ()
    "An alternative to `projectile-find-implementation-or-test-other-window'."
    (interactive)
    (when-let (file (akirak/select-test-or-impl-file))
      (if current-prefix-arg
          (display-buffer (or (find-buffer-visiting file)
                              (find-file-noselect file)))
        (find-file file)))))

(use-package executable
  :straight nil
  :hook
  (after-save . executable-make-buffer-file-executable-if-script-p))

(use-package trashed
  :straight (:host github :repo "shingo256/trashed"))

;; Prevent from guessing a file
;; See https://www.murilopereira.com/how-to-open-a-file-in-emacs/#opening-a-file
(remove-hook 'file-name-at-point-functions #'ffap-guess-file-name-at-point)

(provide 'setup-files)
