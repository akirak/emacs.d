(use-package vterm
  ;; Use the package installed using nix
  :straight (vterm :type built-in)
  :commands (vterm)
  :general
  (:keymaps 'vterm-mode-map
            "<S-prior>" #'scroll-down-command
            "<S-next>" #'scroll-up-command
            "C-c C-t" #'vterm-copy-mode)
  :config/el-patch
  ;; fzy doesn't seem to work with the default implementation.
  (el-patch-defun vterm-send-return ()
    (interactive)
    (el-patch-swap (vterm-send-key "<return>")
                   (process-send-string vterm--process "\C-m")))
  :config
  (mapc (lambda (key)
          (define-key vterm-mode-map (kbd key) nil))
        '("M-r"
          "M-g"
          "M-s"
          "<f1>"))
  (defun akirak/vterm-quit-window (&optional buf _event)
    (if-let ((window (get-buffer-window buf)))
        (quit-window nil window)
      (bury-buffer buf)))
  (add-hook 'vterm-exit-functions #'akirak/vterm-quit-window))

(cl-defun akirak/run-interactive-shell-command (command &optional name
                                                        &key root compilation)
  (declare (indent 1))
  (interactive "s")
  (require 'vterm)
  (let* ((default-directory (or root default-directory))
         (buffer (generate-new-buffer (or name (format "*%s*" command)))))
    (with-current-buffer buffer
      (let ((vterm-shell command))
        (vterm-mode))
      (when compilation
        (compilation-minor-mode t))
      (pop-to-buffer buffer)
      (remove-hook 'vterm-exit-functions #'akirak/vterm-quit-window :local))))

(use-package vterm-toggle
  :commands (vterm-toggle)
  :custom
  ;; vterm-toggle uses pop-to-buffer to display the buffer, but it
  ;; deletes all the other windows by default.
  ;; To disable the behaviour, you have to set this variable to nil.
  (vterm-toggle-fullscreen-p nil)
  (vterm-toggle-use-dedicated-buffer t))

(defun akirak/vterm-find-window ()
  (catch 'done
    (walk-window-tree
     (lambda (w)
       (when (eq 'vterm-mode
                 (buffer-local-value 'major-mode (window-buffer w)))
         (throw 'done w))))))

(defun akirak/vterm-cleanup ()
  (mapc (lambda (buf)
          (when (and (eq 'vterm-mode
                         (buffer-local-value 'major-mode buf))
                     (not (process-live-p (get-buffer-process buf))))
            (kill-buffer buf)))
        (buffer-list)))

(defun akirak/vterm-toggle-cd (&optional arg)
  "Toggle the vterm window, create a new buffer, or visit an existing buffer."
  (interactive "P")
  (akirak/vterm-cleanup)
  (pcase arg
    ('(16)
     (if-let ((buffer-list (--> (buffer-list)
                                (cl-remove-if-not
                                 (lambda (buf)
                                   (eq 'vterm-mode
                                       (buffer-local-value 'major-mode buf)))
                                 it)
                                (mapcar (lambda (buf)
                                          (cons (buffer-name buf) buf))
                                        it)))
              (buffer (frog-menu-read "Select a vterm buffer" buffer-list)))
         (if-let ((window (akirak/vterm-find-window)))
             (progn
               (select-window window)
               (switch-to-buffer buffer))
           (pop-to-buffer buffer))
       (vterm-toggle-cd)))
    ('(4)
     (let ((cwd default-directory))
       (when-let ((window (akirak/vterm-find-window)))
         (select-window window))
       (let ((default-directory cwd))
         (vterm))))
    (_ (vterm-toggle-cd))))


(provide 'setup-vterm)
