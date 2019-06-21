(use-package projectile
  :init
  (projectile-mode)
  (add-to-list 'projectile-globally-ignored-directories ".cask")
  :config
  (akirak/bind-jump :keymaps 'projectile-mode-map
    ;; Find the file under the cursor
    ;; https://www.reddit.com/r/emacs/comments/7wjyhy/emacs_tip_findfileatpoint/du0ymqx/
    "M-f" 'projectile-find-file-dwim)
  :custom
  (projectile-completion-system (quote ivy))
  (projectile-create-missing-test-files t)
  (projectile-enable-caching t)
  (projectile-require-project-root nil)
  (projectile-ignored-project-function #'akirak/projectile-ignore-project-p)
  (projectile-keymap-prefix (kbd "C-x 9")))

;;;; Function to determine if a directory should be ignored by projectile

(defun akirak/projectile-ignore-project-p (root)
  (or (file-equal-p "~/" root)
      (file-equal-p "~/org/" root)
      (string-prefix-p "/nix/" root)
      (string-prefix-p "/usr/" root)))

(advice-add #'projectile-keep-project-p :after-while
            (lambda (project) (not (akirak/projectile-ignore-project-p project))))

;;;; Delete duplicate projects when the cleanup function is run

(defun akirak/projectile-delete-duplicate-known-projects ()
  (cl-delete-duplicates projectile-known-projects :test #'file-equal-p))

(advice-add 'projectile-cleanup-known-projects :after
            #'akirak/projectile-delete-duplicate-known-projects)

;;;; Prepend exec-path

;; Example usage:
;;
;; Create .dir-locals.el with the following content:
;;
;; ((nil . ((projectile-project-compilation-cmd . "npm run serve")
;;          (projectile-exec-path . ("node_modules/.bin")))))

(defvar projectile-exec-path nil)
(make-variable-buffer-local 'projectile-exec-path)

(defun projectile-with-exec-path (orig &rest args)
  "Run ORIG function with ARGS with  `exec-path' modified."
  (if projectile-exec-path
      (let ((root (projectile-project-root))
            (tmp-exec-path (copy-list exec-path)))
        (dolist (relpath projectile-exec-path)
          (cl-adjoin (expand-file-name relpath root) tmp-exec-path
                     :test #'file-equal-p))
        (let ((exec-path tmp-exec-path))
          (apply orig args)))
    (apply orig args)))

(advice-add 'projectile--run-project-cmd :around #'projectile-with-exec-path)

(provide 'setup-projectile)
