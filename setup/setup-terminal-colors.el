(use-package ansi-color
  :ensure nil
  :config
  (add-hook 'comint-preoutput-filter-functions 'ansi-color-filter-apply)
  :hook
  (compilation-filter . colorize-compilation-buffer)
  :preface
  (autoload 'ansi-color-apply-on-region "ansi-color")
  (defun colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))

(use-package xterm-color
  :straight (xterm-color :host github :repo "atomontage/xterm-color")
  :functions (xterm-color-filter)
  :config
  ;; Configuration for shell-mode
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))
  (defun akirak/xterm-color-shell-mode-hook ()
    ;; Disable font-locking in this buffer to improve performance
    (font-lock-mode -1)
    ;; Prevent font-locking from being re-enabled in this buffer
    (make-local-variable 'font-lock-function)
    (setq font-lock-function (lambda (_) nil))
    (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t))
  ;; Configuration for compilation-mode
  (add-to-list 'compilation-environment "TERM=xterm-256color")
  (defun akirak/xterm-color-compilation-start (proc)
    ;; We need to differentiate between compilation-mode buffers
    ;; and running as part of comint (which at this point we assume
    ;; has been configured separately for xterm-color)
    (when (eq (process-filter proc) 'compilation-filter)
      ;; This is a process associated with a compilation-mode buffer.
      ;; We may call `xterm-color-filter' before its own filter function.
      (set-process-filter
       proc
       (lambda (proc string)
         (funcall 'compilation-filter proc
                  (xterm-color-filter string))))))
  :hook
  (shell-mode . akirak/xterm-color-shell-mode-hook)
  (compilation-start . akirak/xterm-color-compilation-start))

;; Render unicode characters properly.
;;
;; https://stackoverflow.com/questions/6820051/unicode-characters-in-emacs-term-mode
(defadvice ansi-term (after advise-ansi-term-coding-system)
  (set-process-coding-system 'utf-8-unix 'utf-8-unix))
(ad-activate 'ansi-term)

(provide 'setup-terminal-colors)
