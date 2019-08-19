(use-package feebleline
  :init
  (feebleline-mode 1)
  :config
  (setq feebleline-msg-functions
        '(((lambda () (format-mode-line "%e")))
          ;; ((lambda () (frame-parameter nil 'name)) :post " " :face font-lock-function-name-face)
          (akirak/feebleline-time-string :post " " :face font-lock-comment-face)
          (akirak/feebleline-input-method :post " " :face font-lock-constant-face)
          (akirak/feebleline-file-directory :face feebleline-dir-face :post "")
          (akirak/feebleline-file-or-buffer-name :face font-lock-keyword-face :post "")
          (akirak/feebleline-vc :face font-lock-string-face)
          ;; ((lambda () (when (and buffer-file-name (require 'magit nil t))
          ;;               (magit-get-current-branch))) :face font-lock-string-face :post " ")
          ((lambda () mode-name) :post " " :face font-lock-comment-face)
          (akirak/feebleline-buffer-group :post " " :face akirak/feebleline-buffer-group-face)
          (akirak/feebleline-buffer-size :post " " :face font-lock-comment-face)
          (akirak/org-clock-summary-for-feebleline :face font-lock-builtin-face :pre " :: "))))

(defface akirak/feebleline-buffer-group-face
  '((t :weight bold :inherit font-lock-comment-face))
  "Face for buffer group identification in the feeble line.")

(defvar-local akirak/feebleline-buffer-group nil)

(defun akirak/feebleline-buffer-group ()
  (or akirak/feebleline-buffer-group
      (when (featurep 'centaur-tabs)
        (when-let ((groups (centaur-tabs-buffer-groups)))
          (setq-local akirak/feebleline-buffer-group (string-join groups "|"))))))

(byte-compile #'akirak/feebleline-buffer-group)

(defvar akirak/feebleline-last-buffer nil)

(advice-add #'feebleline--insert
            :around #'akirak/ad-around-feebleline-insert)

(defun akirak/ad-around-feebleline-insert (orig)
  (let ((buffer (window-buffer)))
    (unless (and akirak/feebleline-last-buffer
                 (eq buffer akirak/feebleline-last-buffer))
      (run-hooks 'akirak/feebleline-buffer-changed-hook))
    (funcall orig)))

(byte-compile #'akirak/ad-around-feebleline-insert)

(defvar akirak/feebleline-buffer-changed-hook nil)

(byte-compile #'akirak/ad-around-feebleline-insert)

(defsubst akirak/feebleline-input-method ()
  current-input-method-title)

(defvar akirak/feebleline-time-string nil)

(defsubst akirak/feebleline-time-string ()
  akirak/feebleline-time-string)

(defun akirak/feebleline-update-time-string ()
  (setq akirak/feebleline-time-string
        (format-time-string "%b %d %a W%U %H:%M")))

(run-with-timer 0 20 #'akirak/feebleline-update-time-string)

(byte-compile #'akirak/feebleline-update-time-string)

(add-hook 'akirak/feebleline-buffer-changed-hook
          #'akirak/feebleline-update-buffer-size)

(defvar akirak/feebleline-buffer-size nil)

(defun akirak/feebleline-update-buffer-size ()
  (setq akirak/feebleline-buffer-size
        (format-mode-line "%I")))

(byte-compile #'akirak/feebleline-update-buffer-size)

(defsubst akirak/feebleline-buffer-size ()
  akirak/feebleline-buffer-size)

(defvar akirak/feebleline-file-or-buffer-name nil)

(defun akirak/feebleline-file-or-buffer-name ()
  akirak/feebleline-file-or-buffer-name)

(defun akirak/update-feebleline-file-or-buffer-name ()
  (setq akirak/feebleline-file-or-buffer-name
        (let ((s (feebleline-file-or-buffer-name))
              (max-length 30))
          (if (> (length s) max-length)
              (substring s 1 max-length)
            s))))

(byte-compile #'akirak/update-feebleline-file-or-buffer-name)

(add-hook 'akirak/feebleline-buffer-changed-hook
          #'akirak/update-feebleline-file-or-buffer-name)

(defvar akirak/feebleline-file-directory nil)

(defun akirak/feebleline-file-directory ()
  akirak/feebleline-file-directory)

(defun akirak/feebleline-update-file-directory ()
  (setq akirak/feebleline-file-directory
        (feebleline-file-directory)))

(byte-compile #'akirak/feebleline-update-file-directory)

(add-hook 'akirak/feebleline-buffer-changed-hook
          #'akirak/feebleline-update-file-directory)

(setq akirak/feebleline-vc-modeline nil)

(defun akirak/feebleline-vc ()
  (let ((buffer (window-buffer)))
    (if (eq akirak/feebleline-last-buffer buffer)
        akirak/feebleline-vc-modeline
      (setq akirak/feebleline-last-buffer buffer
            akirak/feebleline-vc-modeline
            (format-mode-line '((vc-mode vc-mode)))))))

(byte-compile #'akirak/feebleline-vc)

;;;; Clock summary

(defvar akirak/org-clock-summary-for-feebleline nil)

(defvar akirak/org-clock-current-duration-seconds nil)

(add-hook 'org-clock-in-hook #'akirak/feebleline-org-clock-in)
(add-hook 'org-clock-out-hook #'akirak/feebleline-org-clock-out)
(add-hook 'org-clock-cancel-hook #'akirak/feebleline-org-clock-out)

(defun akirak/org-clock-summary-for-feebleline ()
  (when akirak/org-clock-current-duration-seconds
    (-let (((s . v) akirak/org-clock-summary-for-feebleline))
      (if (eq s akirak/org-clock-current-duration-seconds)
          v
        (let* ((seconds akirak/org-clock-current-duration-seconds)
               (fmt akirak/feebleline-org-clock-timer-format)
               (v (format fmt
                          (cond
                           ((< seconds 3600)
                            (format-time-string "%-M:%S"
                                                (seconds-to-time seconds)))
                           ((< seconds 86400)
                            (format "%dh %dm"
                                    (/ seconds 3600)
                                    (mod (/ seconds 60) 60)))
                           (t (org-minutes-to-clocksum-string
                               (floor seconds 60))))
                          (if (string-empty-p org-clock-current-task)
                              (with-current-buffer (marker-buffer org-clock-marker)
                                (org-with-wide-buffer
                                 (goto-char org-clock-marker)
                                 (nth 4 (org-heading-components))))
                            org-clock-current-task))))
          (setq akirak/org-clock-summary-for-feebleline (cons seconds v))
          v)))))

(defvar akirak/feebleline-org-clock-timer nil)

(defvar akirak/feebleline-org-clock-timer-format nil)

(defsubst akirak/feebleline-org-clock-in ()
  (setq akirak/org-clock-current-duration-seconds 0
        akirak/org-clock-summary-for-feebleline '(0 . "")
        akirak/feebleline-org-clock-timer-format
        (format "%%s on %%s (in %s)"
                (buffer-name (marker-buffer org-clock-marker)))
        akirak/feebleline-org-clock-timer
        (run-with-timer 1 1 #'akirak/feebleline-org-clock-update)))

(defsubst akirak/feebleline-org-clock-out ()
  (cancel-timer akirak/feebleline-org-clock-timer)
  (setq akirak/org-clock-current-duration-seconds nil
        akirak/feebleline-org-clock-timer nil
        akirak/feebleline-org-clock-timer-format nil
        akirak/org-clock-summary-for-feebleline nil))

(defun akirak/feebleline-org-clock-update ()
  (pcase akirak/org-clock-current-duration-seconds
    ((pred (> 3600))
     (cl-incf akirak/org-clock-current-duration-seconds))
    (3600
     (cl-incf akirak/org-clock-current-duration-seconds)
     (cancel-timer akirak/feebleline-org-clock-timer)
     (setq akirak/feebleline-org-clock-timer
           (run-with-timer 60 60 #'akirak/feebleline-org-clock-update)))
    ((pred (< 3600))
     (cl-incf akirak/org-clock-current-duration-seconds 60))))

(provide 'setup-feebleline)
