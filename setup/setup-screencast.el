(use-package gif-screencast
  :general
  (:keymaps 'gif-screencast-mode-map
            "<S-f8>" 'gif-screencast-toggle-pause
            "<f8>" 'gif-screencast-stop)
  :custom
  (gif-screencast-output-directory "~/tmp/emacs-screencast"))

;; (defvar akirak/x-screenshot-xephyr-process nil)

(defcustom akirak/x-screenshot-xephyr-display ":3"
  "X display for taking screenshots using X.org programs."
  :type 'string)

(defcustom akirak/x-screenshot-xephyr-screen-geometry "860x500"
  "Screen geometry"
  :type 'string)

(defcustom akirak/x-screenshot-xephyr-systemd-unit "xephyr-screenshot"
  "Systemd service unit name for the Xephyr process."
  :type 'string)

(cl-defun akirak/x-screenshot-start-xephyr (&key screen)
  (interactive)
  (call-process "systemd-run" nil nil nil
                "--user"
                (format "--unit=%s" akirak/x-screenshot-xephyr-systemd-unit)
                "Xephyr"
                "-br" "-ac" "-noreset" "-screen"
                (or screen akirak/x-screenshot-xephyr-screen-geometry)
                akirak/x-screenshot-xephyr-display)
  (sleep-for 0 500))

(defun akirak/x-screenshot-stop-xephyr ()
  (interactive)
  (call-process "systemctl" nil nil nil "--user"
                "stop" akirak/x-screenshot-xephyr-systemd-unit))

(defvar akirak/x-screenshot-frame nil)

(defmacro akirak/with-x-screenshot-xephyr (&rest progn)
  `(unwind-protect
       (progn
         (akirak/x-screenshot-start-xephyr)
         ,@progn)
     (akirak/x-screenshot-stop-xephyr)))

(cl-defun akirak/gif-screencast (&key screen)
  (interactive (when (equal current-prefix-arg '(4))
                 (list :screen (read-string "Screen size [WxH]: "))))
  (pcase current-prefix-arg
    ('(16)
     (dired-other-window gif-screencast-output-directory))
    (_
     (condition-case _
         (let ((display akirak/x-screenshot-xephyr-display)
               (dir gif-screencast-output-directory))
           (akirak/x-screenshot-start-xephyr :screen screen)
           (x-open-connection display)
           (let ((frame (make-frame-on-display display)))
             (setq akirak/x-screenshot-frame frame)
             (select-frame frame)
             (add-hook 'gif-screencast-mode-hook
                       'akirak/gif-screencast-finalize-frame)
             (advice-add 'gif-screencast-capture
                         :around #'akirak/ad-around-gif-screencast-capture)
             (unless (file-directory-p dir)
               (make-directory dir t))
             (setq akirak/gif-screencast-file-list
                   (directory-files dir t))
             (file-notify-add-watch dir '(change) 'akirak/gif-screencast-handle-change)
             (with-selected-frame frame
               (gif-screencast))))
       (error (akirak/gif-screencast-finalize-frame))))))

(defun akirak/gif-screencast-handle-change (event)
  (pcase event
    (`(,descriptor created ,file)
     (file-notify-rm-watch descriptor)
     (sleep-for 0 800)
     (split-window-below)
     (other-window 1)
     (dired-jump nil file)
     (message "Displaying the generated screencast")
     (start-process "mpv" nil "mpv" "--loop" file))))

(defun akirak/ad-around-gif-screencast-capture (orig &rest args)
  (let ((process-environment (cons (concat "DISPLAY=" akirak/x-screenshot-xephyr-display)
                                   (cl-remove-if (lambda (env)
                                                   (string-prefix-p "DISPLAY=" env))
                                                 process-environment)))
        (gif-screencast-args (cl-remove "--focused" gif-screencast-args :test #'equal)))
    (apply orig args)))

(defun akirak/gif-screencast-finalize-frame ()
  (unless gif-screencast-mode
    (remove-hook 'gif-screencast-mode-hook
                 'akirak/gif-screencast-finalize-frame)
    (advice-remove 'gif-screencast-capture
                   #'akirak/ad-around-gif-screencast-capture)
    (let ((display akirak/x-screenshot-xephyr-display)
          (frame akirak/x-screenshot-frame))
      (when (and frame (frame-live-p frame))
        (delete-frame frame))
      (x-close-connection display)
      (akirak/x-screenshot-stop-xephyr))))

(provide 'setup-screencast)
