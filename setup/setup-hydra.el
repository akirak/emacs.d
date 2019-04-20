(defcustom akirak/hydra-prevent-posframe-list
  '(akirak/minor-mode-hydra)
  "List of hydras that should prevent from creating a posframe for help.")

(use-package hydra
  :custom
  (hydra-hint-display-type 'posframe))

(defvar akirak/hydra-stack nil)

(defun akirak/ad-around-hydra-show-hint (orig hint caller)
  (push caller akirak/hydra-stack)
  (if (and (eq hydra-hint-display-type 'posframe)
           (cl-member caller akirak/hydra-prevent-posframe-list))
      (let ((hydra-hint-display-type 'lv))
        (funcall orig hint caller))
    (funcall orig hint caller)))

(advice-add 'hydra-show-hint :around 'akirak/ad-around-hydra-show-hint)

(defun akirak/ad-around-hydra-keyboard-quit (orig)
  (let ((caller (pop akirak/hydra-stack)))
    (if (and (eq hydra-hint-display-type 'posframe)
             (cl-member caller akirak/hydra-prevent-posframe-list))
        (let ((hydra-hint-display-type 'lv))
          (funcall orig))
      (funcall orig))))

(advice-add 'hydra-keyboard-quit :around 'akirak/ad-around-hydra-keyboard-quit)

(provide 'setup-hydra)
