;; I use SKK mostly for writing English words denotated in katakana.
(use-package skk
  :straight ddskk
  :config
  (require 'skk-leim)
  ;; There is skk-dicts package in Nixpkgs, but it is poorly packaged
  ;; in that all files are directory under share directory.
  ;;
  ;; I don't want to install it, so I will build the expression directly.
  (setq skk-large-jisyo
        (f-join (string-trim-right
                 (call-process-with-args "nix-build"
                   "--no-out-link"
                   "-E" "(import <nixpkgs> {}).skk-dicts"))
                "share"
                "SKK-JISYO.L"))

  (defvar akirak/skk-setup-done nil)

  (add-hook 'skk-mode-hook
            (defun akirak/skk-setup ()
              (unless akirak/skk-setup-done
                (general-def :keymaps 'skk-j-mode-map :package 'skk-vars
                  ";" #'akirak/insert-japanese-from-english)
                (setq akirak/skk-setup-done t))))

  :config/el-patch
  (el-patch-defun skk-setup-modeline ()
    ;; ("" skk-modeline-input-mode)
    (setq skk-indicator-alist (skk-make-indicator-alist)))

  :custom
  ;; It's better to set `skk-user-directory' in your `custom-file'.
  ;; The directory will be used to the user dictionary and other files.
  ;; I prefer ~/local/emacs/skk/ as the location.
  ;; I back up ~/local/, so it will be restored.
  (skk-jisyo-code 'utf-8-unix)
  (skk-jisx0213-prohibit t))

(provide 'setup-japanese-skk)
