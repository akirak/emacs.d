(use-package desktop
  :init
  (cl-defun akirak/add-desktop-settings (&key clear-preserve-buffers
                                              globals-to-save
                                              globals-to-clear
                                              locals-to-save)
    "Add items to variables for `desktop-mode'."
    (general-add-hook 'desktop-clear-preserve-buffers
                      clear-preserve-buffers)
    (general-add-hook 'desktop-globals-to-save
                      globals-to-save)
    (general-add-hook 'desktop-globals-to-save
                      globals-to-clear)
    (general-add-hook 'desktop-locals-to-save
                      locals-to-save))
  :config
  ;; Save the desktop when Emacs exits or switch to another desktop
  ;; directory.
  (unless (daemonp)
    (desktop-save-mode t))
  :custom
  ;; Always ask if `desktop-save-mode' should save the session.
  (desktop-save 'ask)
  ;; (desktop-restore-in-current-display)
  (desktop-restore-reuses-frames 'keep))

;;;; Desktop hydra

(defhydra akirak/desktop-hydra (:hint nil)
  "
desktop-dir: %s(when desktop-dirname (abbreviate-file-name desktop-dirname))
%s(if (file-exists-p (desktop-full-file-name)) \
  (concat \"(exists, updated: \" (akirak/format-relative-filetime (desktop-full-file-name) t) \")\")\
\"(does not exist)\")

[_s_]: Save (_w_ to save to another dir)
[_R_]: Reload (_L_ to change the dir)
[_D_]: Purge
[_F_]: Clear
"
  ("s" desktop-save-in-desktop-dir)
  ("w" desktop-save)
  ("R" desktop-read)
  ("L" desktop-change-dir)
  ("D" desktop-remove)
  ("F" desktop-clear))

(defun akirak/format-relative-filetime (file &optional verbose type)
  (let* ((attrs (file-attributes file))
         (internal (pcase type
                     ;; TODO: Add support for other time attributes, e.g. visited time
                     (_ (file-attribute-modification-time attrs))))
         (file-time (make-ts :unix (float-time internal)))
         (abs-format "%Y-%m-%d %H:%M"))
    (if verbose
        (format "%s (%s)"
                (ts-format abs-format file-time)
                (ts-human-format-duration
                 (ts-difference (ts-now) file-time)
                 'abbreviate))
      (ts-format abs-format file-time))))

(defun akirak/desktop-command ()
  "My main entry point to commands provided by desktop.el."
  (interactive)
  (unless (bound-and-true-p desktop-dirname)
    (require 'desktop)
    (call-interactively desktop-change-dir))
  (akirak/desktop-hydra/body))

(general-def "C-z" #'akirak/desktop-command)

(provide 'setup-desktop)
