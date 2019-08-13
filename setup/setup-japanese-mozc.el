;; You'll need mozc_emacs_helper executable to use mozc.el.
;; If you are using Debian or its variant, it is available as
;; emacs-mozc-bin package. I may add it to nixpkgs in the future.

(add-to-list 'load-path (expand-file-name "contrib/mozc"
                                          user-emacs-directory))

(straight-use-package '(mozc :type built-in))

;; Lock the version as mozc.el is local
(straight-use-package '(mozc-popup :host github :repo "d5884/mozc-popup"
                                   :branch "f0684b875a7427ec08f8df13939a486e5d5cf420"))

;; mozc-mode lets you write Japanese
(use-package mozc
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

(use-package mozc-cand-posframe
  :straight (mozc-cand-posframe :host github :repo "akirak/mozc-posframe")
  :after mozc
  :if (posframe-workable-p)
  :config
  (setq mozc-candidate-style 'posframe))

(use-package mozc-temp
  :commands (mozc-temp-convert mozc-temp-convert-dwim))

(provide 'setup-japanese-mozc)
