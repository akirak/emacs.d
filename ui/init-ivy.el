(require 'init-smex)          ; Install smex for sorting M-x candidates

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

(defun ivy-backward-kill-sentence ()
  (interactive)
  (if ivy--directory
      (progn (ivy--cd "/")
             (ivy--exhibit))
    (if (bolp)
        (kill-region (point-min) (point))
      (backward-kill-sentence))))

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
  :general
  ("C-s" #'swiper
   "C-r" #'swiper)
  :custom
  (enable-recursive-minibuffers t)
  (ivy-height 20)
  (ivy-initial-inputs-alist nil "Don't prepend `^' to any of the ivy prompts")
  (projectile-completion-system 'ivy))

(use-package ivy-rich
  :after ivy
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer))

(provide 'init-ivy)
