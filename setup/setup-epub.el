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
  (message "Reading metadata from %s" file)
  (json-parse-string
   (call-process-with-args "nix" "run"
     "github:akirak/epubinfo"
     "--" (expand-file-name file))
   :object-type 'alist
   :array-type 'list
   :null-object nil))

(defun akirak/epub-metadata-pp-display (file)
  (interactive "fFile: ")
  (pp-display-expression (akirak/epub-metadata file)
                         (format "*epub metadata %s*" file)))

(defvar akirak/epub-file-list nil)

(defun akirak/scan-epub-files-recursively (dir)
  (interactive "DRoot: ")
  (->> (directory-files-recursively dir (rx ".epub" eol) nil nil t)
       (-filter #'f-exists-p)
       (--map (ignore-errors (akirak/epub-metadata it)))
       (-filter #'-non-nil)
       (setq akirak/epub-file-list)))

(provide 'setup-epub)
