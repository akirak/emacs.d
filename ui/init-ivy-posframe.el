(use-package ivy-posframe
  ;; Child frames are supported by Emacs >= 26.1
  :when (version<= "26.1" emacs-version)
  :after ivy
  :init
  (ivy-posframe-enable)
  :config
  ;; Custom "poshandler" functions
  ;; Set a fixed height for the child frames
  (defcustom akirak/ivy-posframe-frame-height 10
    "Fixed height of ivy-posframe frames.")

  (defun akirak/posframe-poshandler-frame-center-1 (info)
    "Display the posframe slightly above the frame center."
    (cons (/ (- (plist-get info :parent-frame-width)
                (plist-get info :posframe-width))
             2)
          (/ (- (plist-get info :parent-frame-height)
                (plist-get info :posframe-height))
             4)))

  (defun akirak/ivy-posframe-display-center-120 (str)
    (let ((ivy-posframe-width (min 120 (frame-width)))
          (ivy-posframe-height akirak/ivy-posframe-frame-height))
      (ivy-posframe--display str #'akirak/posframe-poshandler-frame-center-1)))

  (defun akirak/ivy-posframe-display-at-point-120 (str)
    (let ((ivy-posframe-width (min 120 (frame-width)))
          (ivy-posframe-height akirak/ivy-posframe-frame-height))
      (ivy-posframe--display str #'posframe-poshandler-point-bottom-left-corner)))

  (defun akirak/ivy-posframe-display-at-point-50 (str)
    (let ((ivy-posframe-width (min 120 (frame-width)))
          (ivy-posframe-height akirak/ivy-posframe-frame-height))
      (ivy-posframe--display str #'posframe-poshandler-point-bottom-left-corner)))

  ;; Set the default display function
  (setq ivy-display-function #'akirak/ivy-posframe-display-center-120)

  ;; Configure commands whose candidates should be displayed at point (wide)
  (dolist (func '(counsel-describe-function
                  counsel-describe-variable
                  counsel-describe-face
                  counsel-faces))
    (add-to-list 'ivy-display-functions-alist
                 `(,func . akirak/ivy-posframe-display-at-point-120)))

  ;; Configure commands whose candidates should be displayed at point (narrow)
  (dolist (func '(counsel-apropos
                  ivy-yasnippet
                  dumb-jump-ivy-jump-to-selected))
    (add-to-list 'ivy-display-functions-alist
                 `(,func . akirak/ivy-posframe-display-at-point-50)))

  (add-to-list 'ivy-display-functions-alist
               '(dumb-jump-ivy-jump-to-selected . ivy-posframe-display-at-point))

  ;; Ensure hiding the posframe frame when existing ivy
  (add-hook 'minibuffer-exit-hook #'ivy-posframe-cleanup))

(provide 'init-ivy-posframe)
