;; Mostly based on an example in https://github.com/tumashu/pyim/blob/master/README.md
(use-package pyim-basedict
  :config (pyim-basedict-enable))

;; Install librime using nix-env -i librime
(use-package liberime-config
  ;; TODO: Use nix for installation
  ;; Install librime-data and librime-data-luna-pinyin from the Debian repo
  :straight (liberime-config :host gitlab :repo "liberime/liberime"
                             :files ("CMakeLists.txt" "Makefile" "src" "liberime-config.el"))
  :config/el-patch
  (el-patch-defun liberime--build ()
    (let ((default-directory liberime--root))
      (set-process-sentinel
       (el-patch-swap (start-process "liberime-build" "*liberime build*"
                                     "make")
                      (start-process "liberime-build" "*liberime build*"
                                     "nix-shell" "-p" "librime" "cmake"
                                     "--command" "make"))
       (lambda (proc _event)
         (when (eq 'exit (process-status proc))
           (if (= 0 (process-exit-status proc))
               (liberime--load)
             (pop-to-buffer "*liberime build*")
             (error "liberime: building failed with exit code %d" (process-exit-status proc))))))))
  :init
  (add-hook 'after-liberime-load-hook
            (lambda ()
              (liberime-select-schema "luna_pinyin_simp"))))

(use-package pyim
  :custom
  (pyim-default-scheme 'rime)
  (pyim-page-tooltip 'popup)
  (pyim-page-length 9)
  (pyim-auto-select t)
  (pyim-english-input-switch-functions '(pyim-probe-isearch-mode
                                         pyim-probe-program-mode
                                         pyim-probe-org-structure-template))
  (pyim-punctuation-half-width-functions '(pyim-probe-punctuation-line-beginning
                                           pyim-probe-punctuation-after-punctuation))
  :config
  (pyim-isearch-mode 1))

(provide 'setup-chinese-pyim)
