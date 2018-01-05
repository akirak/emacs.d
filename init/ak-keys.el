;;; ak-keys.el --- Keybiding configuration -*- lexical-binding: t no-byte-compile: t -*-

;; Copyright (C) 2018 by Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/akirak/

;;; Commentary:

;; 

;;; Code:

;;;; general.el

;; This use-package directive for general should precede any other use-package
;; directives, as they require support for :general keyword.
(use-package general :straight t)

;;;; Override keybindings which are part of Emacs

(general-define-key
 :keymaps 'minibuffer-local-map
 "C-u" 'backward-kill-sentence
 "C-w" 'backward-kill-word
 "C-h" (lambda () (interactive) (delete-char -1))
 )

;;;; which-key

(use-package which-key :straight t
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom) ; Display a popup window at bottom
  :custom
  ;; (which-key-use-C-h-commands nil)
  (which-key-side-window-max-height 20) ; Set the maximum height of a popup
  ;; TODO: What which-key-sort-order should I use?
  ;; (which-key-sort-order 'which-key-prefix-then-key-order)
  ;; TODO: Is there a better way to cycle through pages?
  )

(provide 'ak-keys)

;;; ak-keys.el ends here
