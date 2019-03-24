;;; setup-typography.el --- Typography configuration -*- lexical-binding: t -*-
;;
;; Note: =unpackaged/font-compare= will be convenient for picking a
;; font for a specific type of text.

;;;; Line spacing
;; Writing
(setq-mode-local org-mode line-spacing 0.3)
(setq-mode-local markdown-mode line-spacing 0.3)

;; Reading
(setq-mode-local Info-mode line-spacing 0.5)

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

;;;; Faces

;; Enable faces for contents inside blocks
(setq-default org-fontify-quote-and-verse-blocks t)

(cl-defun akirak/use-face-fonts (&key default
                                      variable-pitch
                                      writing
                                      writing-italic
                                      heading
                                      reading)
  (unless default
    (user-error "Default font is nil"))

  ;; Default fonts
  (set-face-attribute 'default nil :height 105 :family default)
  (set-face-attribute 'fixed-pitch-serif nil
                      ;; Change the color for info-mode
                      :foreground "gold"
                      :family default
                      :inherit 'default)
  (set-face-attribute 'org-code nil :family default)
  (set-face-attribute 'org-block nil :family default)

  ;; Variable pitch
  (when variable-pitch
    (set-face-attribute 'variable-pitch nil :family variable-pitch))

  ;; Header line
  (set-face-attribute 'header-line nil
                      :family (or heading default)
                      :inherit 'italic)
  (set-face-attribute 'akirak/header-line-buffer-name nil
                      :family (or heading default)
                      :height 1.3
                      :slant 'italic)
  (set-face-attribute 'akirak/header-line-outline nil
                      :family (or heading default)
                      :height 1.3)
  (set-face-attribute 'info-title-4 nil :slant 'italic
                      :family (or heading default))

  ;; Org headings
  (set-face-attribute 'org-level-1 nil :height 1.75 :inherit 'italic)
  (set-face-attribute 'org-level-2 nil :height 1.6 :inherit 'italic)
  (set-face-attribute 'org-level-3 nil :height 1.5 :inherit 'italic)
  (set-face-attribute 'org-level-4 nil :height 1.35 :inherit 'italic)
  (set-face-attribute 'org-level-5 nil :height 1.25 :inherit 'italic)
  (set-face-attribute 'org-level-6 nil :height 1.2 :inherit 'italic)
  (set-face-attribute 'org-level-7 nil :height 1.15 :inherit 'italic)
  (set-face-attribute 'org-level-8 nil :height 1.1 :inherit 'italic)
  (dolist (level (number-sequence 1 8))
    (set-face-attribute (intern (format "org-level-%d" level)) nil
                        :family (or heading default)))

  ;; Other headings (for reading)
  (with-eval-after-load 'helpful
    (set-face-attribute 'helpful-heading nil :height 1.2
                        :family (or heading reading default)))
  (with-eval-after-load 'dired-filter
    (set-face-attribute 'dired-filter-group-header nil :height 1.2 :inherit 'default
                        :family (or heading default)))
  (with-eval-after-load 'info
    (set-face-attribute 'info-menu-header nil :height 1.2 :inherit 'default
                        :family (or heading reading default)))

  ;; Other Org faces
  (set-face-attribute 'org-quote nil :inherit 'italic
                      :family (or writing-italic default))
  (set-face-attribute 'org-tag nil
                      :foreground "grey"
                      :background nil
                      :underline t
                      :height 115
                      :family (or writing-italic default)
                      :inherit 'italic))

(defun akirak/set-local-text-fonts ()
  (when (derived-mode-p 'text-mode)
    (cond
     ((derived-mode-p 'Info-mode 'eww-mode)
      (when-let ((reading (plist-get akirak/face-fonts :reading)))
        (face-remap-add-relative 'default `(:family ,reading))))
     ;; TODO: Set :height
     ;; (face-remap-add-relative 'default
     ;;                          `(:height 1.3))
     ((derived-mode-p 'org-mode 'markdown-mode)
      (when-let ((writing (plist-get akirak/face-fonts :writing)))
        (face-remap-add-relative 'default `(:family ,writing)))
      (when-let ((writing-italic (plist-get akirak/face-fonts :writing-italic)))
        (face-remap-add-relative 'italic `(:family ,writing-italic)))))))

(add-hook 'change-major-mode-hook 'akirak/set-local-text-fonts)

(defcustom akirak/face-fonts
  (let ((family-list (font-family-list))
        (the-list (list :default '("Overpass Mono"
                                   "Hack"
                                   "Noto Sans Mono"
                                   "Monofur"
                                   "Meslo LG S"
                                   "mononoki"
                                   ;; Fallback for Windows
                                   "Consolas")
                        :writing '("Monaco"
                                   "Fantasque Sans Mono"
                                   "Iosevka"
                                   "MMCedar")
                        :writing-italic '("Fantasque Sans Mono")
                        ;; Font for heading (primarily in org-mode)
                        :heading '("Futura LT"
                                   "Overpass")
                        ;; Monospace font for tables.
                        ;; (table "Overpass Mono")
                        :reading '("Droid Sans Mono"
                                   "Fira Code"
                                   "Droid Sans"
                                   "Merriweather"
                                   "Gotham")
                        :variable-pitch '("Overpass"
                                          "Droid Sans"))))
    (cl-loop for (key families) on the-list by #'cddr
             append (list key (--find (member it family-list) families))))
  "Plist of fonts to use in Emacs."
  :group 'akirak
  :set (lambda (symbol value)
         (let* ((family-list (font-family-list))
                (value (cl-loop for (key family . _) on value by 'cddr with new-value
                                do (cond
                                    ((member family family-list)
                                     (setq new-value `(,key ,family ,@new-value)))
                                    (family
                                     (message "Font family \"%s\" is unavailable" family))
                                    (t
                                     (message "Font family for %s is not set" key)))
                                finally return new-value)))
           ;; (cl-loop for )
           (set symbol value)
           (apply 'akirak/use-face-fonts value)
           (dolist (buf (buffer-list))
             (with-current-buffer buf
               (akirak/set-local-text-fonts))))))

;;;; Fonts for specific fontsets

;; TODO: Define fallback for Japanese kanjis that are unavailble in Chinese fonts
(defcustom akirak/fontset-fonts
  `(
    ;; Alternatives
    ;; 消費生活用製品の製造又は輸入事業者は、重大な製品事故が発生したことを知ったときは10日以内に消費者庁に報告しなければなりません。
    (kana . "HarenosoraMincho")
    ;; 2日早上，海关关员在办理“广州东至香港红磡”直通列车的通关手续时，发现6名中国籍旅客结伴走“无申报通道”出境，且神色慌张、眼神躲闪。
    (han . "AR PL UKai CN")
    ;; 藝人吳建豪與新加坡百億千金石貞善(Arissa)2013年結婚，兩人屢屢傳出婚變消息，
    (bopomofo . "AR PL UKai TW")
    ;; NOTE: "cwTeX Q Fangsong" supports both simplified and traditional Chinese
    )
  "Fonts for specific fontsets."
  :group 'akirak
  :set (lambda (symbol value)
         (set symbol value)
         (cl-loop for (fontset . family) in value
                  do (set-fontset-font "fontset-default" fontset family)))
  :type '(alist :key-type (symbol :tag "Fontset")
                :value-type (string :tag "Family")
                :options
                (((const :tag "Japanese Kana" kana)
                  (choice (const "Harenosoramincho")
                          (const "Source Han Sans JP")
                          (const "Noto Sans Mono CJK JP")
                          (const "Ume Gothic C4")
                          (string :tag "Other family")))
                 ((const :tag "Simplified Chinese" han)
                  (choice (const "AR PL UKai CN")
                          (const "Adobe Heiti Std R")
                          (const "Noto Sans Mono CJK SC")
                          (const "WenQuanYi Micro Hei Mono")
                          (string :tag "Other family")))
                 ((const :tag "Traditional Chinese" bopomofo)
                  (choice (const "AR PL UKai TW")
                          (const "Source Han Sans TW")
                          (const "Noto Sans Mono CJK TC")
                          (string :tag "Other family"))))))

(provide 'setup-typography)
