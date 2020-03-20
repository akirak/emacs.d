(defgroup akirak/system nil "System constants.")

;;;; Window systems
(defconst akirak/window-system
  (cond
   ((and (getenv "WAYLAND_DISPLAY")
         ;; I use :2 for Xephyr sessions
         (not (equal x-display-name ":2")))
    'wayland)
   ((eq window-system 'x)
    'x)
   (t
    (bound-and-true-p window-system))))

;;;; Platforms
(defconst akirak/linux-type
  (and (eq system-type 'gnu/linux)
       (cond
        ((stringp (getenv-internal "SOMMELIER_VERSION"))
         'crostini)
        ((with-temp-buffer
           (insert-file-contents "/proc/sys/kernel/osrelease")
           (insert-file-contents "/proc/version")
           (string-match-p (rx (or "Microsoft" "WSL"))
                           (buffer-string)))
         'wsl)
        (t t))))

(defun akirak/windows-subsystem-for-linux-p ()
  (eq akirak/linux-type 'wsl))

(defun akirak/running-on-crostini-p ()
  "Return non-nil if Emacs is running on Crostini of Chrome OS."
  (eq akirak/linux-type 'crostini))

;;;;  Distributions
(defconst akirak/os-release-like
  (when (file-exists-p "/etc/os-release")
    (with-temp-buffer
      (insert-file-contents "/etc/os-release")
      (goto-char (point-min))
      (cond
       ((or (re-search-forward (rx bol "ID=" (?  "\"") "debian" (?  "\"") eol)
                               nil t)
            (re-search-forward (rx bol "ID_LIKE=" (?  "\"") (* anything) "debian")
                               nil t))
        'debian)))))

(defun akirak/os-like-debian-p ()
  (eq akirak/os-release-like 'debian))

(provide 'my/const/system)
