;; I use SKK mostly for writing English words denotated in katakana.
(use-package skk
  :straight ddskk
  :config
  (require 'skk-leim)
  ;; There is skk-dicts package in Nixpkgs, but it is poorly packaged
  ;; in that all files are directory under share directory.
  ;;
  ;; I don't want to install it, so I will build the expression directly.
  (setq skk-large-jisyo (f-join (string-trim-right
                                 (call-process-with-args "nix-build" "-E"
                                   "(import <nixpkgs> {}).skk-dicts"))
                                "share"
                                "SKK-JISYO.L"))

  :general
  (:keymaps 'skk-j-mode-map :package 'skk
            ";" #'akirak/insert-japanese-from-english)

  :custom
  ;; It's better to set `skk-user-directory' in your `custom-file'.
  ;; The directory will be used to the user dictionary and other files.
  ;; I prefer ~/local/emacs/skk/ as the location.
  ;; I back up ~/local/, so it will be restored.
  (skk-jisyo-code 'utf-8-unix)
  (skk-jisx0213-prohibit t))

(provide 'setup-japanese-skk)
