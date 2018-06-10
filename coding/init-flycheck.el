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

(provide 'init-flycheck)
