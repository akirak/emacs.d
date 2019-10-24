;; Based on an example in https://github.com/abo-abo/hydra/wiki/Flycheck
(defhydra hydra-flycheck
  (
   ;; :pre (progn (setq hydra-lv t) (flycheck-list-errors))
   ;; :post (progn (setq hydra-lv nil) (quit-windows-on "*Flycheck errors*"))
   :hint nil)
  "
Errors (flycheck): %s`flycheck-checker
"
  ("f"  flycheck-error-list-set-filter                            "Filter")
  ("n"  flycheck-next-error                                       "Next")
  ("p"  flycheck-previous-error                                   "Previous")
  ("<"  flycheck-first-error                                      "First")
  (">"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
  ("s"  flycheck-select-checker "Select checker")
  ("q"  nil))

(defun akirak/hydra-flycheck ()
  (interactive)
  (flycheck-mode 1)
  (hydra-flycheck/body))

(defun akirak/setup-flycheck-shellcheck ()
  (interactive)
  (unless (and flycheck-sh-shellcheck-executable
               (file-executable-p flycheck-sh-shellcheck-executable))
    (setq-default flycheck-sh-shellcheck-executable
                  (concat (string-trim-right
                           (shell-command-to-string
                            "nix-build --no-out-link '<nixpkgs>' -A shellcheck"))
                          "/bin/shellcheck")))
  (add-to-list 'flycheck-checkers 'sh-shellcheck t)
  (setq flycheck-checker 'sh-shellcheck)
  (flycheck-mode 1))

;;;; UI
(use-package flycheck-posframe
  :after flycheck
  :custom-face
  ;; https://www.reddit.com/r/emacs/comments/couaey/how_to_set_color_of_flycheckposframe/ewlbfbz/
  ;; TODO: Refine face settings
  (flycheck-posframe-error-face ((t (:background "DarkSlateBlue"))))
  (flycheck-posframe-warning-face ((t (:background "DarkSlateBlue"))))
  (flycheck-posframe-border-face ((t (:background "DarkBlue"))))
  :hook
  (flycheck-mode . flycheck-posframe-mode))

;;;; Other helper packages

;; Used in dante
(use-package attrap)

(provide 'setup-flycheck)
