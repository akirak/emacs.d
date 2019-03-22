;; You'll need mozc_emacs_helper executable to use mozc.el.
;; If you are using Debian or its variant, it is available as
;; emacs-mozc-bin package. I may add it to nixpkgs in the future.

(defun akirak/os-like-debian-p ()
  (with-temp-buffer
    (insert-file-contents "/etc/os-release")
    (goto-char (point-min))
    (or (re-search-forward (rx bol "ID=" (?  "\"") "debian" (?  "\"") eol)
                           nil t)
        (re-search-forward (rx bol "ID_LIKE=" (? "\"") (* anything) "debian")
                           nil t))))

;; mozc-mode lets you write Japanese
(use-package mozc
  :straight nil
  :load-path "contrib/mozc"
  ;; If you want to disable this package, set :disabled to t.
  :if (or (executable-find "mozc_emacs_helper")
          (cond
           ((akirak/os-like-debian-p)
            (async-start-process "apt" "sudo" nil
                                 "apt" "install" "emacs-mozc-bin")))))

;; Better display of candidates
(use-package mozc-popup
  :after mozc
  :config
  (setq mozc-candidate-style 'popup))

(use-package mozc-temp
  :after mozc)

(provide 'setup-japanese-mozc)
