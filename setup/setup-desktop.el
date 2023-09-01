(use-package desktop
  :config
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
  ;; Save the desktop when Emacs exits or switch to another desktop
  ;; directory.
  (unless (daemonp)
    (desktop-save-mode t))
  :custom
  (desktop-save 'ask-if-exists)
  ;; (desktop-restore-in-current-display)
  (desktop-restore-reuses-frames 'keep))

(pretty-hydra-define akirak/desktop-hydra
  (:title (concat "Desktop\n Directory: "
                  (or (ignore-errors
                        (and desktop-dirname
                             (format "%s %s"
                                     (abbreviate-file-name desktop-dirname)
                                     (if (file-exists-p (desktop-full-file-name))
                                         (format "(exists, updated: %s)"
                                                 (akirak/context-hydra-format-relative-filetime
                                                  (desktop-full-file-name) t))
                                       "(does not exist)"))))
                      "nil"))
          :quit-key "C-g"
          :foreign-keys run)
  ("Load"
   (("R" desktop-read "Reload from the dir")
    ("L" desktop-change-dir "Change the dir"))
   "Save"
   (("s" desktop-save-in-desktop-dir "Save to the dir")
    ("w" desktop-save "Save to another dir"))
   "Others"
   (("D" desktop-remove "Purge")
    ("F" desktop-clear "Clear"))))

(akirak/bind-admin "es" '(akirak/desktop-hydra/body :wk "session (desktop)"))

(provide 'setup-desktop)
