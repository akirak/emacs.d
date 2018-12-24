(use-package ivy
  :diminish (ivy-mode)
  :init
  (ivy-mode 1)               ; Use ivy as completing-read-function
  :config
  (add-to-list 'ivy-sort-functions-alist
               '(read-file-name-internal . eh-ivy-sort-file-by-mtime))
  :general
  (:keymaps 'ivy-occur-mode-map
            "n" #'ivy-occur-next-line
            "p" #'ivy-occur-previous-line
            "SPC" #'ivy-occur-press)
  :custom
  (enable-recursive-minibuffers t)
  (ivy-height 10)
  (ivy-initial-inputs-alist nil "Don't prepend `^' to any of the ivy prompts")
  (projectile-completion-system 'ivy)
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

(defun ivy-switch-to-org-buffer ()
  "Switch to an open Org buffer."
  (interactive)
  (ivy-read "Org buffer: "
            (cl-remove-if-not
             (lambda (bufname)
               (with-current-buffer (get-buffer bufname)
                 (derived-mode-p 'org-mode)))
             (internal-complete-buffer "" nil t))
            :caller #'ivy-switch-buffer
            :action #'switch-to-buffer))

(provide 'setup-ivy)
