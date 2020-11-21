;; Basically run
;; gpg-connect-agent /bye
;; export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)

(defvar akirak/gpg-ssh-auth-sock-set nil
  "Non-nil if an ssh auth sock by the GPG agent is set.")

(defun akirak/ensure-gpg-ssh-auth-sock ()
  (unless akirak/gpg-ssh-auth-sock-set
    (let ((agent-buffer (generate-new-buffer "*gpg-connect-agent*")))
      (unless (= 0 (call-process "gpg-connect-agent"
                                 nil
                                 agent-buffer
                                 nil
                                 "/bye"))
        (display-buffer agent-buffer)
        (error "Failed to run 'gpg-connect-agent /bye'"))
      (if-let (sock (car (process-lines "gpgconf" "--list-dirs" "agent-ssh-socket")))
          (if (file-exists-p sock)
              (setq akirak/gpg-ssh-auth-sock-set
                    (setenv "SSH_AUTH_SOCK" sock))
            (error "Invalid socket: %s" sock))
        (error "gpgconf didn't return an agent socket")))))

(provide 'setup-gpg)
