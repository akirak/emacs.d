(use-package nov
  :mode (("\\.epub\\'" . nov-mode))
  :general
  (:keymaps 'nov-mode-map :package 'nov
            "?" #'akirak/nov-mode-hydra/body))

(defhydra akirak/nov-mode-hydra (:exit nil
                                       :hint nil
                                       :foreign-keys 'run)
  "nov-mode"
  ("[" nov-previous-document "previous")
  ("]" nov-next-document "next")
  ("t" nov-goto-toc "toc")
  ("q" nil "quit hydra" :exit t))

(defun akirak/epub-metadata (file)
  (message "Running...")
  (json-parse-string
   (call-process-with-args "nix" "run"
     "github:akirak/epub2json"
     "--" (expand-file-name file))
   :object-type 'alist
   :array-type 'list
   :null-object nil))

(defun akirak/epub-metadata-pp-display (file)
  (interactive "fFile: ")
  (pp-display-expression (akirak/epub-metadata file)
                         (format "*epub metadata %s*" file)))

(provide 'setup-epub)
