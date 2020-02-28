;;; setup-web-browser.el --- Integration with external web browsers -*- lexical-binding: t -*-


(defcustom akirak/web-browser-application-list
  '(("chromium-browser.desktop" :key ?c)
    ("chromium.desktop" :key ?c)
    ("firefox.desktop" :key ?f)
    ("qutebrowser.desktop" :key ?q))
  "File names of desktop files."
  :type '(repeat (cons string (plist))))

(defcustom akirak/localhost-browser-executable "chromium"
  "Browser program for localhost.")

(defvar akirak/web-browser-available-desktop-files nil)

(defun akirak/web-browser-available-desktop-files ()
  (or akirak/web-browser-available-desktop-files
      (setq akirak/web-browser-available-desktop-files
            (->> akirak/web-browser-application-list
                 (mapcar #'car)
                 (mapcar #'akirak/locate-xdg-desktop-file)
                 (delq nil)))))

(defcustom akirak/web-browser-default-program
  (let ((desktop (cl-find-if #'akirak/locate-xdg-desktop-file
                             (mapcar #'car akirak/web-browser-application-list))))
    (if desktop
        `(desktop ,desktop)
      'eww))
  "Default web browser used to read web pages.")

(defun akirak/web-browser-default-function (url &rest args)
  (interactive)
  (pcase akirak/web-browser-default-program
    (`(desktop ,desktop)
     ;; TODO: Run a desktop file with argument
     )
    ('eww
     (eww url))))

(defun akirak/web-browser-x-class-names ()
  (->> (akirak/web-browser-available-desktop-files)
       (mapcar #'akirak/get-xdg-desktop-window-class)
       (delq nil)
       (mapcar #'s-capitalize)))

(defun akirak/web-browser-buffers ()
  (-filter (if (akirak/exwm-session-p)
               (lambda (buffer)
                 (or (member (buffer-local-value 'exwm-class-name buffer)
                             (akirak/web-browser-x-class-names))
                     (eq 'eww-mode (buffer-local-value 'major-mode buffer))))
             (lambda (buffer)
               (eq 'eww-mode (buffer-local-value 'major-mode buffer))))
           (akirak/real-buffer-list)))

(defun akirak/helm-web-browser ()
  (interactive)
  (helm :prompt (format "Web browser [%s]: "
                        (pcase akirak/web-browser-default-program
                          (`(desktop ,file) file)
                          ('eww "eww")))
        :sources
        (list (helm-build-sync-source "Browser buffers"
                :candidates
                (--map (cons (buffer-name it) it)
                       (akirak/web-browser-buffers))
                :action
                (quote (("Select window or pop to buffer" .
                         (lambda (buffer)
                           (if-let ((window (get-buffer-window buffer t)))
                               (select-window window)
                             (pop-to-buffer buffer))))
                        ("Switch to buffer other window" . switch-to-buffer-other-window)
                        ("Switch to buffer this window" . switch-to-buffer)
                        ("Kill buffer" . kill-buffer))))
              (helm-build-dummy-source "Search"))))

;;;; Deprecated

(defun akirak/display-url-for-referencing (url)
  (interactive "sUrl: ")
  (let ((orig-win (selected-window)))
    (split-window-sensibly)
    (other-window 1)
    (eww url)
    (select-window orig-win)))

(defcustom akirak/browser-class-names '("Chromium"
                                        "Chromium-browser"
                                        "Firefox")
  "List of X class names of web browsers."
  :type '(repeat string))

(defun akirak/exwm-browser-buffer-p (buffer)
  ;; (and (eq 'exwm-mode (buffer-local-value 'major-mode buffer))
  ;;      (member (buffer-local-value 'exwm-class-name buffer)
  ;;              akirak/browser-class-names))
  (when (stringp buffer)
    (setq buffer (get-buffer buffer)))
  (unless buffer
    (user-error "BUFFER cannot be nil"))
  (member (buffer-local-value 'exwm-class-name buffer)
          akirak/browser-class-names))

(defun akirak/exwm-browser-windows ()
  (-filter (lambda (w)
             (akirak/exwm-browser-buffer-p (window-buffer w)))
           (window-list)))

(defun akirak/select-exwm-browser-window ()
  (when-let ((browser-windows (akirak/exwm-browser-windows)))
    (if (= 1 (length browser-windows))
        (car browser-windows)
      (get-buffer-window (get-buffer
                          (completing-read "Browsers"
                                           (mapcar (lambda (w)
                                                     (buffer-name (window-buffer w)))
                                                   browser-windows)))))))

(defun akirak/exwm-browser-buffers ()
  (-filter #'akirak/exwm-browser-buffer-p (akirak/exwm-list-buffers)))

(defun akirak/select-exwm-browser-buffer ()
  (when-let ((buffers (akirak/exwm-browser-buffers)))
    (if (= 1 (length buffers))
        (get-buffer (car buffers))
      (get-buffer (completing-read "Browsers"
                                   (mapcar #'buffer-name buffers))))))

(defun akirak/start-web-browser ()
  (interactive)
  (start-process-shell-command "browser" nil
                               (or browse-url-generic-program
                                   "chromium")))

(defun akirak/raise-browser (&optional arg)
  (interactive "P")
  (cond
   ((featurep 'exwm)
    (cl-case arg
      ('(4)
       (akirak/start-web-browser))
      (otherwise
       (if-let ((w (akirak/select-exwm-browser-window)))
           (select-window w)
         (if-let ((b (akirak/select-exwm-browser-buffer)))
             (switch-to-buffer-other-window b)
           (akirak/start-web-browser))))))))

(provide 'setup-web-browser)
;;; setup-web-browser.el ends here
