;; You'll need mozc_emacs_helper executable to use mozc.el.
;; If you are using Debian or its variant, it is available as
;; emacs-mozc-bin package. I may add it to nixpkgs in the future.

;; mozc-mode lets you write Japanese
(use-package mozc
  :straight nil
  :load-path "contrib/mozc"
  ;; If you want to disable this package, set :disabled to t.
  :if (or (executable-find "mozc_emacs_helper")
          (cond
           ((akirak/os-like-debian-p)
            (let ((default-directory "/sudo::/"))
              (= 0 (call-process "apt" nil "*apt install emacs-mozc-bin" t
                                 "install" "--yes" "emacs-mozc-bin")))))
          (progn
            (message "Disabled mozc as mozc_emacs_helper is not installed")
            nil)))

;; Better display of candidates
(use-package mozc-popup
  :after mozc
  :config
  (setq mozc-candidate-style 'popup))

(use-package mozc-temp
  :commands (mozc-temp-convert mozc-temp-convert-dwim))

(provide 'setup-japanese-mozc)
