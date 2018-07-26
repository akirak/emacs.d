;;;; Custom variables
(defun akirak/update-file-extensions ()
  (setq dired-open-extensions
        (append
         ;; Use mpv to play both video and sound files
         (--map `(,it . "mpv")
                (append (bound-and-true-p akirak/video-file-extensions)
                        (bound-and-true-p akirak/sound-file-extensions))))))

(defcustom akirak/video-file-extensions
  '("mkv" "mp4")
  "List of file extensions of video files."
  :set (lambda (symbol value)
         (set-default symbol value)
         (akirak/update-file-extensions))
  :type '(repeat string))

(defcustom akirak/sound-file-extensions
  '("ogg" "mp3" "aac" "flac" "wav")
  "List of file extensions of sound files."
  :set (lambda (symbol value)
         (set-default symbol value)
         (akirak/update-file-extensions))
  :type '(repeat string))

;;;; Basic configuration (without external dependencies)

(setq-default dired-dwim-target t
              dired-recursive-copies 'always
              dired-recursive-deletes 'top)

;;;; Formatting

(setq-default dired-listing-switches "-alh")

(use-package ls-lisp
  :after dired
  :straight nil
  :custom
  (ls-lisp-use-insert-directory-program nil))

;;;;; File size format
(defun akirak/ls-lisp-format-file-size (file-size human-readable)
  (if human-readable
      (format " %6s" (file-size-human-readable file-size))
    (format (if (floatp file-size)
                ls-lisp-filesize-f-fmt
              ls-lisp-filesize-d-fmt)
            file-size)))

(advice-add #'ls-lisp-format-file-size :override
            #'akirak/ls-lisp-format-file-size)

;;;;; Time format
(defun akirak/ls-lisp-format-time (file-attr time-index)
  (let ((time (nth (or time-index 5) file-attr)))
    (format-time-string "%F %R" time)))

(advice-add #'ls-lisp-format-time :override
            #'akirak/ls-lisp-format-time)

;;;; Packages for enhancing dired
(use-package dired-x
  :straight nil
  :after dired
  :config
  (setq-default dired-omit-files-p t))

(use-package dired+
  ;; Disable it for now as it alters the default behavior of dired
  ;; in many aspects
  :disabled t
  :custom
  (diredp-hide-details-initially-flag nil))

(use-package dired-hacks-utils
  :after dired
  :general
  ;; Remap 'n' and 'p' in dired-mode
  (:keymaps 'dired-mode-map
            "n" 'dired-hacks-next-file
            "p" 'dired-hacks-previous-file))

(use-package joseph-single-dired
  :disabled t
  :after dired)

(use-package ivy-dired-history
  :after dired
  :init
  (add-to-list 'savehist-additional-variables 'ivy-dired-history-variable))

;;;; Control display

(use-package dired-hide-dotfiles
  :after dired
  :commands (dired-hide-dotfiles-mode))

(use-package dired-filter
  :after dired
  :config
  (setq dired-filter-group-saved-groups
        `(("default"
           ("Directories"
            (directory . t))
           ("Dotfiles"
            (name . "^\\."))
           ("Books and papers"
            (extension "pdf" "mobi" "epub" "azw"))
           ("Text"
            (or (name . "README")
                (name . "TODO")
                (name . "LICENSE")
                (extension "txt" "md" "mkd" "markdown" "rst")))
           ("Org"
            (extension "org" "bib"))
           ("Archives"
            (extension "zip" "rar" "gz" "bz2" "tar"))
           ("Images and graphics"
            (extension "jpg" "jpeg" "png" "gif" "svg"))
           ("Disk images"
            (extension "iso" "ova"))
           ("Data files"
            (extension "csv" "json" "sql"))
           ("Office docs"
            (extension "xlsx" "xls" "docx" "doc"))
           ("Config"
            (or (name . "Makefile")
                (name . "Dockerfile")
                (extension "yml" "yaml" "cabal"
                           "dockerfile" "mk")))
           ("Programs"
            (extension "exe" "run" "deb"))
           ("Objects and binary files"
            (extension "o" "elc"))
           ("Meta data"
            (extension "torrent" "acsm"))
           ("Emacs Lisp"
            (extension "el")))))
  :hook
  (dired-mode . dired-filter-mode)
  (dired-mode . dired-filter-group-mode))

(use-package dired-collapse
  :after dired
  :hook
  (dired-mode . dired-collapse-mode))

(use-package dired-subtree
  :after dired
  :general
  (:keymaps 'dired-mode-map
            "i" #'dired-subtree-insert)
  :custom
  (dired-subtree-line-prefix " - "))

;;;;; Appearances of directory entries

(use-package all-the-icons-dired
  :after dired
  :diminish 'all-the-icons-dired-mode
  :hook
  (dired-mode . all-the-icons-dired-mode))

;;;;;; Colorizing

;; Use diredful to detect malformed file names
(use-package diredful
  :after dired
  :config
  (diredful-mode 1)
  :custom
  ;; Make diredful machine-specific
  (diredful-init-file (expand-file-name "~/.diredful")))

(use-package diredfl
  :disabled t
  :after dired
  :config
  (diredfl-global-mode 1))

(use-package dired-k
  :disabled t
  :after dired
  :hook
  (dired-mode . dired-k))

(use-package dired-rainbow
  :disabled t
  :after dired)

;;;;; External tools

(use-package dired-open
  :after dired
  :custom
  (dired-open-functions '(dired-open-by-extension
                          dired-open-subdir)))

;; TODO: Integrate ivy-dired-history with dired-rsync
(use-package dired-rsync
  :after dired)

;;;; Additional keybindings in dired-mode
(general-def dired-mode-map
  "h" #'dired-up-directory
  "z h" #'dired-hide-dotfiles-mode
  "<S-return>" #'dired-open-xdg)

(general-def :keymaps 'dired-mode-map
  :prefix "C-z"
  "r" #'dired-rsync)

(define-key dired-mode-map (kbd "/") dired-filter-map)

(provide 'init-dired)
