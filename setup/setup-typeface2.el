;;; setup-typeface2.el --- Typography configuration -*- lexical-binding: t -*-
;;
;; Note: =unpackaged/font-compare= will be convenient for picking a
;; font for a specific type of text.

;;;; Setting variables related to type setting
;;;;; General
;; Enable faces for contents inside blocks
(setq-default org-fontify-quote-and-verse-blocks t)

;;;;; Line spacing
;; Line spacing for writing
(setq-mode-local org-mode line-spacing 0.3)
(setq-mode-local markdown-mode line-spacing 0.3)

;; Line spacing for reading
(setq-mode-local Info-mode jline-spacing 0.5)

;; Browser
;; This does not seem to effect
;; (setq-mode-local eww-mode line-spacing 0.3)

;;;; Utilities for fonts
(defun akirak/check-fonts (list)
  "Check availablity of fonts in LIST and transform it into an alist."
  (let ((all-fonts (font-family-list)))
    (cl-loop for (key family) in list
             do (unless (member family all-fonts)
                  (message "The preferred font '%s' for %s is unavailable"
                           family (symbol-name key)))
             collect (cons key family))))

;;;; Faces excluding font familiy settings

(set-face-attribute 'fixed-pitch-serif nil :foreground "gold" :inherit 'default)

(set-face-attribute 'italic nil :underline nil :slant 'italic)
(with-eval-after-load 'org
  (set-face-attribute 'org-verbatim nil :inherit 'org-code))

;;;;; Header line
(when (featurep 'akirak/setup-header-line)
  (set-face-attribute 'header-line nil
                      :inherit 'italic)
  (set-face-attribute 'akirak/header-line-buffer-name nil
                      :slant 'normal
                      :height 1.3)
  (set-face-attribute 'akirak/header-line-outline nil
                      :height 1.3)
  (set-face-attribute 'info-title-4 nil :slant 'italic))

;;;;; Headings
(set-face-attribute 'org-document-title nil :height 1.6
                    :weight 'normal
                    :inherit 'default)
(set-face-attribute 'org-level-1 nil :height 1.75 :inherit 'default)
(set-face-attribute 'org-level-2 nil :height 1.6 :inherit 'default)
(set-face-attribute 'org-level-3 nil :height 1.5 :inherit 'default)
(set-face-attribute 'org-level-4 nil :height 1.35 :inherit 'default)
(set-face-attribute 'org-level-5 nil :height 1.25 :inherit 'default)
(set-face-attribute 'org-level-6 nil :height 1.2 :inherit 'default)
(set-face-attribute 'org-level-7 nil :height 1.15 :inherit 'default)
(set-face-attribute 'org-level-8 nil :height 1.1 :inherit 'default)

;; Other headings (for reading)
(with-eval-after-load 'helpful
  (set-face-attribute 'helpful-heading nil :height 1.2))
(with-eval-after-load 'dired-filter
  (set-face-attribute 'dired-filter-group-header nil :height 1.2 :inherit 'default))
(with-eval-after-load 'info
  (set-face-attribute 'info-menu-header nil :height 1.2 :inherit 'default))
(with-eval-after-load 'markdown-mode
  (set-face-attribute 'markdown-header-face-1 nil :height 1.7 :inherit 'markdown-header-face)
  (set-face-attribute 'markdown-header-face-2 nil :height 1.5 :inherit 'markdown-header-face)
  (set-face-attribute 'markdown-header-face-3 nil :height 1.4 :inherit 'markdown-header-face)
  (set-face-attribute 'markdown-header-face-4 nil :height 1.3 :inherit 'markdown-header-face)
  (set-face-attribute 'markdown-header-face-5 nil :height 1.2 :inherit 'markdown-header-face)
  (set-face-attribute 'markdown-header-face-6 nil :height 1.1 :inherit 'markdown-header-face))

;;;;; Documents, e.g. Org and Markdown
(set-face-attribute 'org-quote nil :inherit 'default
                    :slant 'normal)
(set-face-attribute 'org-todo nil
                    :foreground "grey"
                    :background nil
                    :inherit 'default)
(set-face-attribute 'org-tag nil
                    :foreground "grey"
                    :background nil
                    :inherit 'default)

;;;; Default height
(defcustom akirak/font-height 105
  "Default height of the font."
  :group 'akirak
  :type 'integer
  :set (lambda (sym value)
         (set sym value)
         (set-face-attribute 'default nil :height value)
         (set-face-attribute 'org-tag nil
                             :height (ceiling (* value 1.1)))
         (set-face-attribute 'org-todo nil
                             :height (ceiling (* value 1.1)))))

(general-setq akirak/font-height akirak/font-height)

(defvar akirak/font-height-scale 8)

(defun akirak/font-height-increase ()
  (interactive)
  (akirak/set-font-height (+ akirak/font-height akirak/font-height-scale)))

(defun akirak/font-height-decrease ()
  (interactive)
  (akirak/set-font-height (- akirak/font-height akirak/font-height-scale)))

(defun akirak/set-font-height (new-value)
  (interactive (list (read-number (format "Font height [%d]: "
                                          akirak/font-height))))
  (general-setq akirak/font-height new-value)
  (message "Font height set to %d" new-value))

;;;; Font families

(defvar akirak/font-family-list nil)

(cl-defun akirak/set-font-family-if-existing (family &rest faces)
  (declare (indent 1))
  (if (member family (or akirak/font-family-list (font-family-list)))
      (dolist (face faces)
        (set-face-attribute face nil :family family))
    (message "Font family %s does not exist, so the following faces won't have expected settings: %s"
             family
             (mapconcat #'symbol-name faces " "))))

(cl-defun akirak/set-fontset-font (fontset family)
  (declare (indent 1))
  (if (member family (or akirak/font-family-list (font-family-list)))
      (set-fontset-font "fontset-default" fontset family)
    (message "Font family %s does not exist, so %s fontset won't get a proper font setting."
             family fontset)))

(defvar akirak/maybe-duospace-font nil)
(defvar akirak/paragraph-font nil)

(defun akirak/set-local-text-fonts ()
  (cond
   ((derived-mode-p 'Info-mode 'eww-mode 'help-mode 'helpful-mode)
    (face-remap-add-relative 'default `(:family ,akirak/paragraph-font)))
   ((derived-mode-p 'org-mode 'markdown-mode)
    (face-remap-add-relative 'default `(:family ,akirak/maybe-duospace-font)))))

;; Set the default font
(defcustom akirak/default-font-family
  ;; Available from my agave package
  "agave"
  "Font family used as the default font."
  :type 'string
  :set (lambda (sym default)
         (set sym default)
         (akirak/set-font-family-if-existing default
           'default
           ;; text in Info-Mode
           'fixed-pitch-serif
           'org-code
           'org-block)
         (with-eval-after-load 'info
           (let ((ref-faces (let (faces)
                              (mapatoms
                               (lambda (sym)
                                 (when (and (face-documentation sym)
                                            (string-prefix-p "info-colors-ref-item-"
                                                             (symbol-name sym)))
                                   (push sym faces))))
                              faces)))
             (apply #'akirak/set-font-family-if-existing default
                    ref-faces)))))

;; Set the other font families
(let (;; Monospace or duospace of any type
      ;; used for writing
      (monospace-or-duospace
       ;; Nerd font variant of Go Mono
       "GoMono Nerd Font Mono")
      ;; Variable-pitch handwriting font.
      ;; Used for tags in org-mode
      (handwriting
       ;; Brushy italic script font.
       ;; https://fonts.google.com/specimen/Courgette
       ;; Deployed as part of Google Fonts
       "Courgette")
      ;; Used for headings in org-mode, Info-mode, etc.
      (heading
       ;; Humanist sans serif typeface
       ;; https://fonts.google.com/specimen/Belleza
       ;; Deployed as part of Google Fonts
       "Belleza")
      ;; A fixed pitch font for text body.
      (info-paragraph
       "Hack NF")
      ;; A fixed/variable pitch font for text body.
      (paragraph
       "Tinos Nerd Font")
      (kana "HannariMincho")
      (han "Adobe Fangsong Std"))
  (setq akirak/font-family-list (font-family-list))
  (unwind-protect
      (progn

        (akirak/set-font-family-if-existing paragraph
          'variable-pitch)

        ;; Header line
        ;; (when (featurep 'akirak/setup-header-line)
        ;;   (set-face-attribute 'header-line nil
        ;;                       :family (or header-line heading default)
        ;;                       :inherit 'italic)
        ;;   (set-face-attribute 'akirak/header-line-buffer-name nil
        ;;                       :family (or header-line heading default)
        ;;                       :height 1.3)
        ;;   (set-face-attribute 'akirak/header-line-outline nil
        ;;                       :family (or header-line heading default))
        ;;   (set-face-attribute 'info-title-4 nil
        ;;                       :family (or heading default)))

        ;; Headings
        (with-eval-after-load 'org
          (apply #'akirak/set-font-family-if-existing heading
                 'org-document-title
                 (mapcar (lambda (level) (intern (format "org-level-%d" level)))
                         (number-sequence 1 8))))
        (with-eval-after-load 'helpful
          (akirak/set-font-family-if-existing heading
            'helpful-heading))
        (with-eval-after-load 'dired-filter
          (akirak/set-font-family-if-existing heading
            'dired-filter-group-header))
        (with-eval-after-load 'info
          (akirak/set-font-family-if-existing heading
            'info-menu-header
            'info-title-1
            'info-title-2
            'info-title-3
            'info-title-4))
        (with-eval-after-load 'markdown-mode
          (akirak/set-font-family-if-existing heading
            'markdown-header-face))

        ;; Other faces
        (with-eval-after-load 'org
          (akirak/set-font-family-if-existing handwriting
            'org-todo
            'org-checkbox-statistics-done
            'org-tag)
          (akirak/set-font-family-if-existing info-paragraph
            'org-link))


        (setq akirak/paragraph-font info-paragraph)
        (setq akirak/maybe-duospace-font monospace-or-duospace)
        (add-hook 'after-change-major-mode-hook 'akirak/set-local-text-fonts)

        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (akirak/set-local-text-fonts)))

        (akirak/set-fontset-font 'kana kana)
        (akirak/set-fontset-font 'han han))
    (setq akirak/font-family-list nil)))

;;;; Packages for extra face settings

;; Use Hasklig in haskell-mode and enable ligatures.
(use-package hasklig-mode
  :straight (:host github :repo "minad/hasklig-mode")
  :config
  (defun akirak/use-hasklig-font-locally ()
    (face-remap-add-relative 'default '(:family "Hasklig")))
  :hook
  (haskell-mode . hasklig-mode)
  (hasklig-mode . akirak/use-hasklig-font-locally))

(use-package info-colors
  :hook
  (Info-selection . info-colors-fontify-node))

(provide 'setup-typeface2)
