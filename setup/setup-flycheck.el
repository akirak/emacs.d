(use-package flycheck
  :general
  (:keymaps 'flycheck-mode-map
            [remap next-error] #'flycheck-next-error
            [remap previous-error] #'flycheck-previous-error)
  :config
  (akirak/bind-generic :keymaps 'flycheck-mode-map
    "fl" #'flycheck-list-errors)
  :custom
  (flycheck-relevant-error-other-file-show nil))

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

;;;; UI
(use-package quick-peek
  :functions (quick-peek-update
              quick-peek-hide
              quick-peek-overlay-contents
              quick-peek-overlay-ensure-at))

(use-package flycheck-inline
  :after flycheck
  :config
  (setq flycheck-inline-display-function
        (lambda (msg pos err)
          (let* ((ov (quick-peek-overlay-ensure-at pos))
                 (contents (quick-peek-overlay-contents ov)))
            (setf (quick-peek-overlay-contents ov)
                  (concat contents (when contents "\n") msg))
            (quick-peek-update ov)))
        flycheck-inline-clear-function #'quick-peek-hide)
  :general
  (:keymaps 'flycheck-mode-map :prefix "C-c !"
            "SPC" #'flycheck-inline-mode))

(use-package flycheck-posframe
  :after flycheck
  :disabled t
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
