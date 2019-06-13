(use-package ivy-decorator
  :straight (ivy-decorator :host github :repo "akirak/ivy-decorator"))

(use-package ivy
  ;; :diminish (ivy-mode)
  :init
  (ivy-mode 1)               ; Use ivy as completing-read-function
  :config
  (add-to-list 'ivy-sort-functions-alist
               '(read-file-name-internal . eh-ivy-sort-file-by-mtime))
  (ivy-decorator-set-intermediate 'ivy-switch-buffer
      #'get-buffer
    (original 25)
    (buffer-major-mode 15)
    (buffer-directory))
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

;;;; ivy-switch-buffer-2

(defvar ivy-switch-buffer-2-map
  (let ((map (make-composed-keymap nil ivy-switch-buffer-map)))
    (define-key map (kbd "C-c C-p") 'ivy-switch-buffer-2-toggle-mode)
    map))

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

(general-def "C-x b" #'ivy-switch-buffer-2)

;;;; ivy-switch-to-org-buffer
;; Deprecated. I will use ivy-omni-org instead:
;; https://github.com/akirak/ivy-omni-org

(defvar ivy-switch-to-org-buffer-map
  (let ((map (make-composed-keymap nil ivy-switch-buffer-map)))
    (define-key map (kbd "C-l") 'ivy-switch-to-org-buffer--load)
    map))

(defun ivy-switch-to-org-buffer--load ()
  (interactive)
  (let ((text ivy-text)
        (index ivy--index))
    (org-starter-load-all-known-files)
    (message nil)
    (ivy--set-candidates (cl-union ivy--all-candidates
                                   (akirak/org-buffer-list)
                                   :test #'string-equal))
    (ivy--reset-state ivy-last)
    (setf (ivy-state-text ivy-last) text)
    ;; TODO: Restore the proper position of the last item
    (setq ivy--index index)))

(defun akirak/org-buffer-list (&rest _args)
  (cl-remove-if-not
   (lambda (bufname)
     (with-current-buffer (get-buffer bufname)
       (derived-mode-p 'org-mode)))
   (internal-complete-buffer "" nil t)))

(defun ivy-switch-to-org-buffer ()
  "Switch to an open Org buffer."
  (interactive)
  (ivy-read "Org buffer: "
            'akirak/org-buffer-list
            :caller #'ivy-switch-buffer
            :keymap ivy-switch-to-org-buffer-map
            :action #'switch-to-buffer))

(provide 'setup-ivy)
