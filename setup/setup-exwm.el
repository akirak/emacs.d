;; Install exwm using Nix
(straight-use-package '(exwm :type built-in))

(use-package exwm
  :commands (exwm-enable)
  :config
  (general-add-hook 'exwm-input-prefix-keys
                    '(?\M-o
                      ?\M-r
                      ?\M-s
                      ?\M-g))
  :custom
  (exwm-floating-border-width 3)
  (exwm-floating-border-color "orange")
  :hook
  (exwm-mode . akirak/disable-posframe-from-this-buffer)
  (exwm-update-title . akirak/exwm-rename-buffer)
  (exwm-manage-finish . akirak/exwm-manage-finish))

(defun akirak/disable-posframe-from-this-buffer ()
  (set (make-local-variable 'ivy-posframe-display-functions-alist) nil)
  (set (make-local-variable 'hydra-hint-display-type) 'lv))

(advice-add 'exwm-input-toggle-keyboard
            :after #'akirak/ad-after-exwm-input-toggle-keyboard)

(defun akirak/ad-after-exwm-input-toggle-keyboard (&optional id)
  (let ((buffer (if id
                    (exwm--id->buffer id)
                  (current-buffer))))
    (if (eq 'exwm-mode (buffer-local-value 'major-mode buffer))
        (message "Input mode is now %s"
                 (buffer-local-value 'exwm--input-mode buffer))
      (user-error "Not in exwm-mode"))))

(defun akirak/exwm-rename-buffer ()
  "Rename the buffer name after the title is changed."
  (exwm-workspace-rename-buffer (format "*EXWM:%s*" exwm-title)))

(defun akirak/exwm-list-buffers ()
  (internal-complete-buffer "*EXWM:" nil t))

(defvar akirak/exwm-class-name-history nil)
(defvar akirak/exwm-instance-name-history nil)

(defun akirak/exwm-manage-finish ()
  (add-to-list 'akirak/exwm-class-name-history exwm-class-name)
  (add-to-list 'akirak/exwm-instance-name-history exwm-instance-name)
  (cond
   ;; Set char mode on specific window types
   ((member exwm-class-name '("Termite" "Emacs" "Tilix"
                              "Slack"
                              "Code"
                              ;; "Chromium" "Firefox"
                              ))
    (exwm-input-release-keyboard (exwm--buffer->id (window-buffer))))
   ;; Float specific window types
   ((member exwm-instance-name '("keybase"
                                 ".blueman-manager-wrapped"
                                 "GNOME-software"))
    (exwm-floating-toggle-floating))))

(use-package exwm-config
  :straight exwm)

(setq mouse-autoselect-window t)

;; Mouse follows focus
(use-package exwm-mff
  :straight (exwm-mff :host github :repo "ieure/exwm-mff"))

(use-package exwm-edit
  :after exwm
  :commands (exwm-edit-compose)
  :config
  (defun akirak/exwm-edit-setup-compose ()
    (let ((title (exwm-edit--buffer-title (buffer-name))))
      (pcase title
        ((rx "Slack")
         (gfm-mode))
        (_
         (akirak/language-detection-auto-major-mode)))))
  :general
  (:keymaps 'exwm-mode-map
            "C-c '" #'exwm-edit-compose)
  :custom
  (exwm-edit-split-below t)
  :hook
  (exwm-edit-compose . akirak/exwm-edit-setup-compose))

(use-package window-divider
  :disabled t
  :config
  (window-divider-mode 1)
  :custom
  (window-divider-default-right-width 2)
  (window-divider-default-bottom-width 2))

(use-package exwm-systemtray
  :straight exwm
  :config
  (exwm-systemtray-enable)
  :custom
  (exwm-systemtray-height 16))

;;;; xrandr

(pretty-hydra-define akirak/exwm-xrandr-hydra
  (:title (format "RandR
  output plist: %s" exwm-randr-workspace-output-plist))
  ("xrandr"
   (("o" akirak/exwm-xrandr-set-output-plist "Set output list")
    ("c" akirak/exwm-configure-screens "Configure screens"))))

(defun akirak/exwm-xrandr-get-connected-monitors ()
  (->> (process-lines "xrandr" "-q")
       (-filter (lambda (s) (string-match-p (rx " connected ") s)))
       (-map (lambda (s) (car (split-string s))))))

(defun akirak/exwm-xrandr-set-output-plist ()
  (interactive)
  (let* ((monitors (split-string (read-string "List of monitors separated by space: "
                                              (string-join (akirak/exwm-xrandr-get-connected-monitors)
                                                           " "))))
         (output-plist (cl-loop for monitor being the elements of monitors
                                using (index i)
                                append (list i monitor))))
    (princ (setq exwm-randr-workspace-output-plist output-plist))))

(defcustom akirak/exwm-xrandr-command-list
  '("xrandr --output HDMI2 --auto && xrandr --output eDP1 --off"
    "xrandr --output HDMI2 --auto && xrandr --output eDP1 --right-of HDMI2 --auto")
  "List of xrandr commands to configure the screens."
  :type '(repeat string))

(defun akirak/exwm-configure-screens ()
  (interactive)
  (let ((command (completing-read "Command: "
                                  akirak/exwm-xrandr-command-list
                                  nil nil
                                  (string-join (akirak/exwm-xrandr-get-connected-monitors)
                                               " "))))
    (shell-command command)
    (message command)))

;;;; Exwm-Specific commands
(defcustom akirak/web-app-browser-program "chromium"
  "Web browse used to open web apps."
  :type 'string)

(defvar akirak/exwm-web-app-history nil)

(cl-defun akirak/exwm-raise-web-app (url &optional (select t))
  (interactive (list (read-string "URL: " nil akirak/exwm-web-app-history)))
  (or (akirak/exwm-find-web-app-by-url url select)
      (progn
        (message "Opening %s" url)
        (async-start-process "webapp" akirak/web-app-browser-program nil
                             (concat "--app=" url)))))

(cl-defun akirak/exwm-raise-desktop-app (desktop-name &optional (select t))
  (let ((desktop-file (akirak/locate-xdg-desktop-file desktop-name)))
    (if desktop-file
        (let ((class (akirak/get-xdg-desktop-window-class desktop-file)))
          (or (and class
                   (akirak/exwm-find-by-class-name class select))
              (akirak/run-desktop-file desktop-file)))
      (user-error "Not found in application directories: %s" desktop-file))))

(defun akirak/exwm-find-web-app-by-url (url &optional select)
  ;; TODO: Handle path properly
  (let ((instance-name (string-remove-prefix "https://" url)))
    (akirak/exwm-find-by-instance-name instance-name select)))

(defun akirak/exwm-find-by-class-name (name &optional select)
  (akirak/exwm-find-by-predicate
   (lambda (buf) (equal name (buffer-local-value 'exwm-class-name buf)))
   select))

(defun akirak/exwm-find-by-instance-name (name &optional select)
  (akirak/exwm-find-by-predicate
   (lambda (buf) (equal name (buffer-local-value 'exwm-instance-name buf)))
   select))

(defun akirak/exwm-find-by-predicate (predicate &optional select)
  (let ((buf (-find predicate (akirak/real-buffer-list))))
    (when buf
      (message "Find an existing buffer %s" buf)
      (cond
       ((functionp select)
        (funcall select buf))
       (select
        (akirak/exwm-select-buffer-window buf)))
      buf)))

(cl-defun akirak/exwm-select-buffer-window (buf &key all-frames)
  (let ((window (get-buffer-window buf all-frames)))
    (cond
     ((and window (window-live-p window))
      (select-window window))
     (t
      (window-go-split-sensibly)
      (switch-to-buffer buf)))))

(cl-defun akirak/exwm-select-buffer-window-all-frames (buf)
  (akirak/exwm-select-buffer-window buf :all-frames t))

(defun akirak/real-buffer-list ()
  (mapcar #'get-buffer (internal-complete-buffer "" nil t)))

;;;; Keybindings

;;;;; Global keybindings
(use-package window-go
  :straight (window-go :host github :repo "akirak/emacs-window-go"))

(use-package exwm-window-go
  :straight window-go)

(let* ((char-bindings '(("b" akirak/helm-web-browser)
                        ("i" exwm-input-toggle-keyboard)
                        ("j" other-window)
                        ("k" (lambda () (interactive) (other-window -1)))
                        ;; (exwm-layout-shrink-window)
                        ;; (exwm-layout-enlarge-window)
                        ;; You can't use s-l on Chrome OS since it locks the screen
                        ;; It locks the screen anyway
                        ("l" offtime-lock)
                        ("p" (lambda () (interactive) (select-frame (next-frame))))
                        ("P" (lambda () (interactive) (select-frame (previous-frame))))
                        ("f" exwm-layout-toggle-fullscreen)
                        ("s" ace-swap-window)
                        ("x" counsel-linux-app)
                        ("m" window-go-master)
                        ("n" window-go-split-sensibly)
                        ("z" akirak/select-minibuffer-window)
                        ("w" akirak/raise-browser)
                        ("[" exwm-window-go-previous-hidden-workspace)
                        ("]" exwm-window-go-next-hidden-workspace)
                        ("," frame-workflow-last-frame)
                        ("=" exwm-workspace-add)
                        ("-" exwm-workspace-delete)))
       (keybindings (append (cl-loop for (char cmd) in char-bindings
                                     collect (cons (kbd (concat "s-" char))
                                                   cmd))
                            (cl-loop for num in (number-sequence 0 9)
                                     append (list (cons (kbd (format "s-%d" num))
                                                        `(lambda ()
                                                           (interactive)
                                                           (exwm-workspace-switch ,num)))
                                                  (cons (kbd (format "S-s-%d" num))
                                                        `(lambda ()
                                                           (interactive)
                                                           (exwm-workspace-move-window ,num))))))))
  (setq exwm-input-global-keys keybindings)
  (cl-loop for (key . cmd) in keybindings
           do (exwm-input--set-key key cmd))
  (exwm-input--update-global-prefix-keys))

(defun akirak/select-minibuffer-window ()
  (interactive)
  (when-let ((window (active-minibuffer-window)))
    (select-window window)))

;;;;; Local keybindings available in line-mode
;; (general-def :keymaps 'exwm-mode-map)

;;;; Simulation keys
(let* ((bindings `(([?\C-f] [right])
                   ([?\C-b] [left])
                   ([?\M-f] [C-right])
                   ([?\M-b] [C-left])
                   ([?\C-n] [down])
                   ([?\C-p] [up])
                   ([?\C-a] [home])
                   ([?\C-e] [end])
                   ([?\C-d] [delete])
                   ;; Since I use M-d to select the address bar on chromium,
                   ;; I will disable this simulation key.
                   ;; ([?\M-d] [C-S-right delete])
                   ([?\C-k] [S-end delete])
                   ([?\C-m] [return])
                   ([?\C-i] [tab])
                   ([?\C-w] [?\C-x])
                   ([?\M-w] [?\C-c])
                   ([?\C-y] [?\C-v])
                   ([?\C-s] [?\C-f])
                   (,(kbd "C-x k") [?\C-w])))
       (keybindings (cl-loop for (key key2) in bindings
                             collect (cons key key2))))
  (setq exwm-input-simulation-keys keybindings)
  (dolist (binding bindings)
    (exwm-input--set-simulation-keys bindings t)))

(provide 'setup-exwm)
