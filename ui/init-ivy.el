(require 'init-smex)          ; Install smex for sorting M-x candidates
(require 'akirak-counsel-extras)

;;;; Custom sorting functions

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

;;;; Functions for the mini buffer

(defun ivy-backward-kill-sentence ()
  (interactive)
  (if ivy--directory
      (progn (ivy--cd "/")
             (ivy--exhibit))
    (if (bolp)
        (kill-region (point-min) (point))
      (backward-kill-sentence))))

;;;; Loading packages

(use-package counsel                 ; Configure all of ivy, swiper, and counsel
  :diminish (counsel-mode ivy-mode)
  :init
  (ivy-mode 1)               ; Use ivy as completing-read-function
  (counsel-mode 1)           ; Remap built-in functions with counsel equivalents
  :config
  (add-to-list 'ivy-sort-functions-alist
               '(read-file-name-internal . eh-ivy-sort-file-by-mtime))
  (global-set-key [remap recentf-open-files] 'counsel-recentf)
  (global-set-key [remap insert-char] 'counsel-unicode-char)
  (add-to-list 'swiper-font-lock-exclude 'emacs-lisp-mode)
  (add-to-list 'swiper-font-lock-exclude 'org-mode)
  :general
  ("C-s" #'swiper
   "C-r" #'swiper)
  (:keymaps 'ivy-occur-mode-map
            "n" #'ivy-occur-next-line
            "p" #'ivy-occur-previous-line
            "SPC" #'ivy-occur-press)
  :custom
  (enable-recursive-minibuffers t)
  (ivy-height 20)
  (ivy-initial-inputs-alist nil "Don't prepend `^' to any of the ivy prompts")
  (projectile-completion-system 'ivy)
  (ivy-ignore-buffers (quote ("\\` " "\\\\*lemonbar\\\\*" "\\\\*i3status\\\\*"))))

;; ivy-switch-buffer with frame-purpose support
;; https://github.com/alphapapa/frame-purpose.el/issues/13
(defun ivy-switch-buffer-2 ()
  "Switch to another buffer."
  (interactive)
  (let ((this-command 'ivy-switch-buffer))
    (ivy-read "Switch to buffer: " (lambda (&rest args)
                                     (mapcar #'buffer-name (buffer-list)))
              :matcher #'ivy--switch-buffer-matcher
              :preselect (buffer-name (other-buffer (current-buffer)))
              :action #'ivy--switch-buffer-action
              :keymap ivy-switch-buffer-map
              :caller 'ivy-switch-buffer)))

(general-def "C-x b" #'ivy-switch-buffer-2)

;; Deprecated. Use ivy-filthy-rich instead
(use-package ivy-rich
  :after ivy
  :disabled t
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer))

(use-package ivy-filthy-rich
  :straight (ivy-filthy-rich :host github :repo "casouri/ivy-filthy-rich")
  :diminish 'ivy-filthy-rich-mode
  :after ivy
  :config
  (ivy-filthy-rich-mode t)
  :custom
  (ivy-filthy-rich-max-length 120))

(provide 'init-ivy)
