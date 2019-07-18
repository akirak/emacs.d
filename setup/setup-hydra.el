(use-package hydra
  :custom
  (hydra-hint-display-type 'posframe)
  (hydra-posframe-show-params
   (list :internal-border-width 1
         :background-color "salmon4"
         :internal-border-color "red"
         :poshandler 'posframe-poshandler-frame-center)))

(use-package major-mode-hydra)

(defvar akirak/hydra-stack nil)

;;;; Displaying specific hydras in posframe

(defcustom akirak/hydra-posframe-whitelist
  nil
  "List of hydras which should be displayed in posframe.")

(defun akirak/hydra-should-display-in-posframe-p (caller)
  (cl-member caller akirak/hydra-posframe-whitelist))

(defun akirak/ad-around-hydra-show-hint (orig hint caller)
  (push caller akirak/hydra-stack)
  (if (and (eq hydra-hint-display-type 'lv)
           (akirak/hydra-should-display-in-posframe-p caller))
      (let ((hydra-hint-display-type 'hydra))
        (funcall orig hint caller))
    (funcall orig hint caller)))

(advice-add 'hydra-show-hint :around 'akirak/ad-around-hydra-show-hint)

(defun akirak/ad-around-hydra-keyboard-quit (orig)
  (let ((caller (pop akirak/hydra-stack)))
    (if (and (eq hydra-hint-display-type 'lv)
             (akirak/hydra-should-display-in-posframe-p caller))
        (let ((hydra-hint-display-type 'hydra))
          (funcall orig))
      (funcall orig))))

(advice-add 'hydra-keyboard-quit :around 'akirak/ad-around-hydra-keyboard-quit)

;;;; Preventing display in posframe

(defcustom akirak/hydra-prevent-posframe-list
  '(akirak/minor-mode-hydra
    akirak/string-inflection-hydra)
  "List of hydras that should prevent from creating a posframe for help.")

(defun akirak/hydra-should-avoid-posframe-p (caller)
  (or (cl-member caller akirak/hydra-prevent-posframe-list)
      (string-suffix-p "-mode-hydra" (symbol-name caller))))

;; (defun akirak/ad-around-hydra-show-hint (orig hint caller)
;;   (push caller akirak/hydra-stack)
;;   (if (and (eq hydra-hint-display-type 'posframe)
;;            (akirak/hydra-should-avoid-posframe-p caller))
;;       (let ((hydra-hint-display-type 'lv))
;;         (funcall orig hint caller))
;;     (funcall orig hint caller)))

;; (advice-add 'hydra-show-hint :around 'akirak/ad-around-hydra-show-hint)

;; (defun akirak/ad-around-hydra-keyboard-quit (orig)
;;   (let ((caller (pop akirak/hydra-stack)))
;;     (if (and (eq hydra-hint-display-type 'posframe)
;;              (akirak/hydra-should-avoid-posframe-p caller))
;;         (let ((hydra-hint-display-type 'lv))
;;           (funcall orig))
;;       (funcall orig))))

;; (advice-add 'hydra-keyboard-quit :around 'akirak/ad-around-hydra-keyboard-quit)

(provide 'setup-hydra)
