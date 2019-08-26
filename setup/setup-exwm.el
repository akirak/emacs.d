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

(defun akirak/exwm-manage-finish ()
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
                                 "GNOME-software"))
    (exwm-floating-toggle-floating))))

(use-package exwm-config
  :straight exwm)

(setq mouse-autoselect-window t)

;; Mouse follows focus
(use-package exwm-mff
  :straight (exwm-mff :host github :repo "ieure/exwm-mff"))

(use-package exwm-edit
  :config
  (defun akirak/exwm-edit-setup-compose ()
    (let ((title (exwm-edit--buffer-title (buffer-name))))
      (cond
       ;; Add customization
       )))
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

(defun akirak/exwm-xrandr-get-connected-monitors ()
  (->> (process-lines "xrandr" "-q")
       (-filter (lambda (s) (string-match-p (rx " connected ") s)))
       (-map (lambda (s) (car (split-string s))))))

(defcustom akirak/exwm-xrandr-command-alist
  '(
    ;; My work laptop.
    (("eDP1" "HDMI2") . "xrandr --output HDMI2 --auto && xrandr --output eDP1 --right-of HDMI2 --auto"))
  "Alist of a set of monitor names and xrandr command line to configure the screens."
  :type '(alist :key-type (repeat string)
                :value-type string))

(defun akirak/exwm-configure-screens ()
  (interactive)
  (setq exwm-randr-workspace-output-plist
        (cl-loop for monitor being the elements of
                 (akirak/exwm-xrandr-get-connected-monitors)
                 using (index i)
                 append (list i monitor)))
  (let* ((monitors (akirak/exwm-xrandr-get-connected-monitors))
         (command (or (cdr (assoc monitors akirak/exwm-xrandr-command-alist
                                  'seq-set-equal-p))
                      "xrandr --auto")))
    (shell-command command)
    (message command)))

(defcustom akirak/exwm-xrandr-enabled nil
  "Enable exwm-xrandr if non-nil."
  :type 'boolean
  :set (lambda (sym value)
         (set sym value)
         (when value
           (require 'exwm-randr)
           (exwm-randr-enable)
           (add-hook 'exwm-randr-screen-change 'akirak/exwm-configure-screens))))

;;;; Keybindings

;;;;; Global keybindings
(use-package exwm-window-go
  :straight (:host github :repo "akirak/emacs-window-go"))

(let* ((char-bindings '(("i" exwm-input-toggle-keyboard)
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
                        ("=" exwm-workspace-add)
                        ("-" exwm-workspace-delete)))
       (keybindings (cl-loop for (char cmd) in char-bindings
                             collect (cons (kbd (concat "s-" char))
                                           cmd))))
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
