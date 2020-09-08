(use-package ivy-decorator
  :straight (ivy-decorator :host github :repo "akirak/ivy-decorator"))

(use-package ivy
  :config
  (ivy-mode 1)                   ; Use ivy as completing-read-function
  (add-to-list 'ivy-sort-functions-alist
               '(read-file-name-internal . eh-ivy-sort-file-by-mtime))
  (defvar ivy-switch-buffer-2-map
    (let ((map (make-composed-keymap nil ivy-switch-buffer-map)))
      (define-key map (kbd "C-c C-p") 'ivy-switch-buffer-2-toggle-mode)
      map))
  (akirak/bind-register "M-r" #'ivy-resume)
  :general
  (:keymaps 'ivy-occur-mode-map
            "n" #'ivy-occur-next-line
            "p" #'ivy-occur-previous-line
            "SPC" #'ivy-occur-press)
  :custom
  (enable-recursive-minibuffers t)
  (ivy-height 10)
  (ivy-initial-inputs-alist nil "Don't prepend `^' to any of the ivy prompts")
  ;; (projectile-completion-system 'ivy)
  (ivy-ignore-buffers (quote ("\\` " "\\\\*lemonbar\\\\*" "\\\\*i3status\\\\*"))))

;; https://github.com/abo-abo/swiper/wiki/Sort-files-by-mtime#a-simple-version
(defun eh-ivy-sort-file-by-mtime (x y)
  (let* ((x (concat ivy--directory x))
         (y (concat ivy--directory y))
         (x-mtime (nth 5 (file-attributes x)))
         (y-mtime (nth 5 (file-attributes y))))
    (if (file-directory-p x)
        (if (file-directory-p y)
            (time-less-p y-mtime x-mtime)
          t)
      (if (file-directory-p y)
          nil
        (time-less-p y-mtime x-mtime)))))

;;;; ivy-switch-buffer-2

(defun ivy-switch-buffer-2--complete (&rest args)
  (mapcar #'buffer-name (buffer-list)))

(defun ivy-switch-buffer-2-toggle-mode ()
  (interactive)
  (let ((text ivy-text)
        collection)
    (pcase (ivy-state-collection ivy-last)
      ('ivy-switch-buffer-2--complete
       (setq collection 'internal-complete-buffer))
      ('internal-complete-buffer
       (setq collection 'ivy-switch-buffer-2--complete)))
    (setf (ivy-state-collection ivy-last) collection)
    ;; (ivy--set-candidates (cl-union ivy--all-candidates
    ;;                                (akirak/org-buffer-list)
    ;;                                :test #'string-equal))
    (ivy--reset-state ivy-last)
    (setf (ivy-state-text ivy-last) text)))

;; ivy-switch-buffer with frame-purpose support
;; https://github.com/alphapapa/frame-purpose.el/issues/13
(defun ivy-switch-buffer-2 (&optional arg)
  "Switch to another buffer."
  (interactive "P")
  (let ((this-command 'ivy-switch-buffer))
    (ivy-read "Switch to buffer: "
              (if arg
                  #'internal-complete-buffer
                #'ivy-switch-buffer-2--complete)
              :matcher #'ivy--switch-buffer-matcher
              :preselect (buffer-name (other-buffer (current-buffer)))
              :action #'ivy--switch-buffer-action
              :keymap ivy-switch-buffer-2-map
              :caller 'ivy-switch-buffer)))

;; Use counsel-ibuffer for now.
;; (general-def "C-x b" #'ivy-switch-buffer-2)

(defcustom akirak/ivy-posframe-width-alist
  nil
  "Alist of height.")

(use-package ivy-posframe
  :unless (akirak/windows-subsystem-for-linux-p)
  :after ivy
  ;; Use posframe to display candidates in ivy commands.
  ;;
  ;; 1. The default display function is ivy-posframe-display-at-frame-center.
  ;;    However, if there is an EXWM window in the frame, use
  ;;    ivy-posframe-display-at-window-center instead.
  ;; 2. For swiper commands, display the posframe at the window bottom.
  ;; 3. There may be specific situations where I prefer other display functions.
  ;;
  ;; If the current focus is on an EXWM window, ivy-posframe is never used.
  ;; Instead, the default display function is used. This is configured in setup-exwm.el.
  :config
  (general-add-hook 'ivy-height-alist akirak/ivy-posframe-height-alist)
  (ivy-posframe-mode 1)
  (defun akirak/ivy-posframe-window-bottom-left-size ()
    (list
     :height ivy-posframe-height
     :width (window-body-width)
     ;; :min-height (or ivy-posframe-min-height (+ ivy-height 1))
     ;; :min-width (or ivy-posframe-min-width (round (* (frame-width) 0.62)))
     ))
  (defun akirak/ivy-posframe-default-size ()
    "The default functon used by `ivy-posframe-size-function'."
    (let ((caller (ivy-state-caller ivy-last)))
      (list
       :height (or (cdr (assoc caller akirak/ivy-posframe-height-alist))
                   ivy-posframe-height)
       :width (or (cdr (assoc caller akirak/ivy-posframe-width-alist))
                  ivy-posframe-width)
       :min-height (or ivy-posframe-min-height (+ ivy-height 1))
       :min-width (unless (member caller '(ivy-omni-org
                                           all-the-icons-ivy))
                    (or ivy-posframe-min-width
                        (round (* (frame-width) 0.62)))))))
  (defun akirak/ivy-posframe-display-smart-center (str)
    (ivy-posframe--display str #'akirak/posframe-poshandler-smart-center))
  (defun akirak/ivy-decorator-width ()
    (let ((caller (ivy-state-caller ivy-last)))
      (cdr (assoc caller akirak/ivy-posframe-width-alist))))
  (when (akirak/exwm-session-p)
    (setq ivy-posframe-parameters '((parent-frame nil))))
  :config/el-patch
  (el-patch-defun ivy-posframe-display-at-window-bottom-left (str)
    (el-patch-wrap 1
      (let ((ivy-posframe-size-function #'akirak/ivy-posframe-window-bottom-left-size))
        (ivy-posframe--display str #'posframe-poshandler-window-bottom-left-corner))))
  :custom
  (ivy-decorator-width #'akirak/ivy-decorator-width)
  (ivy-posframe-height 12)
  (ivy-posframe-width 100)
  (akirak/ivy-posframe-width-alist
   `((counsel-ibuffer . 120)
     (ivy-omni-org . 100)
     (all-the-icons-ivy . 50)
     ,@(--map (cons it 130)
              '(counsel-describe-function
                counsel-describe-variable
                counsel-faces
                counsel-M-x))))
  (akirak/ivy-posframe-height-alist
   '((ivy-omni-org . 30)
     (all-the-icons-ivy . 30)
     (ivy-clipurl . 15)
     (counsel-yank-pop . 20)))
  (ivy-posframe-size-function #'akirak/ivy-posframe-default-size)
  (org-starter-swiper-width-function (lambda () (- (window-body-width) 5)))
  (ivy-posframe-display-functions-alist
   `(,@(--map (cons it nil)
              '(swiper swiper-all swiper-multi org-starter-swiper-config-files
                       counsel-locate counsel-rg))
     (counsel-minibuffer-history . nil)
     (counsel-yank-pop . ivy-posframe-display-at-point)
     (all-the-icons-ivy . ivy-posframe-display-at-point)
     (t . akirak/ivy-posframe-display-smart-center))))

(provide 'setup-ivy)
