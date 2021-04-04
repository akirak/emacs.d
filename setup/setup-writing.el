;;;; Sentences
;; Based on https://gitlab.com/rycee/configurations/blob/d6dcf6480e29588fd473bd5906cd226b49944019/user/emacs.nix#L73
(setq-default sentence-end
              (rx (or (and (any ".?!")
                           (optional (any "]\"')}"))
                           (or bol (any space))
                           (* space))))
              sentence-end-double-space nil)

;;;; Spell checking
(use-package flyspell
  :if (executable-find "ispell")
  :config
  (general-unbind :keymaps 'flyspell-mode-map :package 'flyspell
    "C-," "C-." "C-M-i" "C-c $" "C-;")
  :custom
  (flyspell-abbrev-p t)
  (flyspell-use-global-abbrev-table-p t))

;;;;; User interface (flyspell-correct)
(use-package flyspell-correct
  :commands
  (flyspell-correct-wrapper
   flyspell-correct-at-point)
  :config
  ;; Based on an example in https://github.com/clemera/frog-menu#example
  (defun akirak/flyspell-correct-frog-menu (candidates word)
    "Run `frog-menu-read' for the given CANDIDATES.

List of CANDIDATES is given by flyspell for the WORD.

Return selected word to use as a replacement or a tuple
of (command . word) to be used by `flyspell-do-correct'."
    (let* ((corrects (if flyspell-sort-corrections
                         (sort candidates 'string<)
                       candidates))
           (actions `(("C-s" "Save word" (save . ,word))
                      ("C-a" "Accept (session)" (session . ,word))
                      ("C-b" "Accept (buffer)" (buffer . ,word))
                      ("C-c" "Skip" (skip . ,word))))
           (prompt (format "Dictionary: [%s]" (or ispell-local-dictionary
                                                  ispell-dictionary
                                                  "default")))
           (res (frog-menu-read prompt corrects actions)))
      (unless res
        (error "Quit"))
      res))
  :custom
  (flyspell-correct-interface #'akirak/flyspell-correct-frog-menu))

;;;; Quotation
(use-package typo)

;;;; Counting words
(use-package wc-mode
  ;; TODO: Configure modelines
  :disabled t)

(use-package org-wc
  :after org)

;;;; Writegood
(use-package writegood-mode
  :disabled t)

(provide 'setup-writing)
