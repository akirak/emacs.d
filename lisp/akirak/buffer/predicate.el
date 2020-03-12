;;;; Machinery
(defsubst akirak/buffer-derived-mode-p (buffer &rest modes)
  (declare (indent 1))
  (apply #'provided-mode-derived-p (buffer-local-value 'major-mode buffer)
         modes))

(defcustom akirak/exwm-browser-class-names
  '("Chromium" "Brave-browser" "Chromium-browser")
  "List of class names of browser windows.")
;;;; Predicates
(defun akirak/reference-buffer-p (buffer)
  ;; Based on the implementation of `derived-mode-p'.
  (or (akirak/buffer-derived-mode-p buffer
        'Info-mode 'help-mode 'helpful-mode 'eww-mode)
      (and (akirak/buffer-derived-mode-p buffer
             'exwm-mode)
           (member (buffer-local-value 'exwm-class-name buffer)
                   akirak/exwm-browser-class-names))))

(defun akirak/indirect-org-buffer-p (buffer)
  (and (akirak/buffer-derived-mode-p buffer 'org-mode)
       (buffer-base-buffer buffer)))

;; TODO: Add a predicate for terminals and interactive shells

(defun akirak/scratch-buffer-p (buffer)
  "A predicate for scratch buffers"
  (or (equal "*scratch*" (buffer-file-name buffer))
      ;; scratch.el
      (buffer-local-value 'scratch-buffer buffer)))

(provide 'akirak/buffer/predicate)
