;;; setup-typography.el --- Typography configuration
;;;; Options

(setq-default org-fontify-quote-and-verse-blocks t)

;;;; Variables

(defvar akirak/heading-font)

;;;; Setting the typeface for writing

(defvar akirak/writing-font)
(defvar akirak/writing-italic-font nil)

(defun akirak/set-typeface-for-writing ()
  "Configure the typography of the current buffer for writing."
  (setq line-spacing 0.3)
  (toggle-word-wrap 1)
  (visual-line-mode 1)
  (face-remap-add-relative 'default
                           `(:family ,akirak/writing-font))
  (when akirak/writing-italic-font
    (face-remap-add-relative 'italic
                             `(:family ,akirak/writing-italic-font))))

;; Use the writing typography in these modes
(add-hook 'org-mode-hook 'akirak/set-typeface-for-writing)
(add-hook 'markdown-mode-hook 'akirak/set-typeface-for-writing)

;; Suppress a message by `toggle-word-wrap' unless the function is
;; called interactively. The message is annoying as the option is set
;; on Org files by default.
(defun akirak/ad-around-toggle-word-wrap (orig &optional arg)
  (unwind-protect
      (progn
        (unless (called-interactively-p)
          (advice-add 'message :override #'akirak/ignore-toggle-word-wrap-message))
        (funcall orig arg))
    (advice-remove 'message #'akirak/ignore-toggle-word-wrap-message)))
(advice-add #'toggle-word-wrap :around #'akirak/ad-around-toggle-word-wrap)

(defun akirak/ignore-toggle-word-wrap-message (&rest args)
  (apply 'ignore args))

;;;; Setting the typeface for reading

(defvar akirak/reading-font)

(defun akirak/set-typeface-for-reading ()
  (setq line-spacing 0.5)
  (face-remap-add-relative 'default
                           `(:family ,akirak/reading-font)))

(add-hook 'Info-mode-hook 'akirak/set-typeface-for-reading)

(defun akirak/set-typeface-for-browser ()
  (setq line-spacing 0.3)
  (face-remap-add-relative 'default
                           `(:height 1.3)))

(add-hook 'eww-mode-hook 'akirak/set-typeface-for-browser)

;;;; Configuring fonts

(defun akirak/check-fonts (list)
  "Check availablity of fonts in LIST and transform it into an alist."
  (let ((all-fonts (font-family-list)))
    (cl-loop for (key family) in list
             do (unless (member family all-fonts)
                  (message "The preferred font '%s' for %s is unavailable"
                           family (symbol-name key)))
             collect (cons key family))))

(let ((default-font-size 10.5))
  (let-alist
      (akirak/check-fonts
       '(;; Default monospace font.
         ;; Alternatives: Hack, Noto Sans Mono, Monofur, Meslo LG S, mononoki
         (default "Overpass Mono")
         ;; Font for body text when writing documents
         ;; Alternatives: Fantasque Sans Mono, Iosevka, MMCedar
         (writing "Monaco")
         ;; Font specific for italic
         (writing-italic "Fantasque Sans Mono")
         ;; Font for heading (primarily in org-mode)
         (heading "Futura LT")
         ;; Monospace font for tables.
         ;; (table "Overpass Mono")
         ;; Font for reading.
         ;; Alternatives: Fira Code, Droid Sans, Merriweather, Gotham.
         (reading "Droid Sans Mono")

         ;; Proportional font. Used mostly for browser (e.g. eww).
         ;; Alternatives: Droid Sans, Overpass
         (proportional "Overpass")))
    ;; Set default fonts
    (set-default-font (format "%s-%.1f" .default default-font-size))
    ;; Set text body font to Duospace if any
    (setq akirak/writing-font .writing)
    (setq akirak/writing-italic-font .writing-italic)
    (setq akirak/reading-font .reading)

    ;; Standard faces with local family
    (set-face-attribute 'fixed-pitch nil :family .default)
    (set-face-attribute 'fixed-pitch-serif nil
                        ;; Change the color for info-mode
                        :foreground "gold"
                        :family .default :inherit 'default)
    (set-face-attribute 'variable-pitch nil
                        :family .proportional)

    ;; Header lines
    ;; Titillium may be a sensible choice
    (set-face-attribute 'header-line nil
                        :family .writing-italic
                        :inherit 'italic)
    (set-face-attribute 'akirak/header-line-buffer-name nil
                        :height 1.3
                        :slant 'italic)
    (set-face-attribute 'akirak/header-line-outline nil
                        :height 1.3)

    ;; Headings
    ;; This face is inherited by all the other info title faces
    (set-face-attribute 'info-title-4 nil :family .heading :slant 'italic)
    (setq akirak/heading-font .heading)
    (with-eval-after-load 'helpful
      (set-face-attribute 'helpful-heading nil :family akirak/heading-font :height 1.2))
    (with-eval-after-load 'dired-filter
      (set-face-attribute 'dired-filter-group-header nil
                          :family akirak/heading-font :height 1.2 :inherit 'default))
    (with-eval-after-load 'info
      (set-face-attribute 'info-menu-header nil
                          :family akirak/heading-font :height 1.2 :inherit 'default))

    ;; Org headings
    (dolist (level (number-sequence 1 8))
      (set-face-attribute (intern (format "org-level-%d" level))
                          nil :family .heading :inherit 'italic))
    ;; Configure other face attributes for headings
    (set-face-attribute 'org-level-1 nil :height 1.75)
    (set-face-attribute 'org-level-2 nil :height 1.6)
    (set-face-attribute 'org-level-3 nil :height 1.5)
    (set-face-attribute 'org-level-4 nil :height 1.35)
    (set-face-attribute 'org-level-5 nil :height 1.25)
    (set-face-attribute 'org-level-6 nil :height 1.2)
    (set-face-attribute 'org-level-7 nil :height 1.15)
    (set-face-attribute 'org-level-8 nil :height 1.1)

    (set-face-attribute 'org-code nil :family .default)
    (set-face-attribute 'org-block nil :family .default)
    ;; (set-face-attribute 'org-verbatim nil :family default)
    (set-face-attribute 'org-quote nil :family .writing-italic :inherit 'italic)
    (set-face-attribute 'org-tag nil
                        :family .writing-italic
                        :foreground "grey"
                        :background nil
                        :underline t
                        :height (floor (* 10 default-font-size 1.1))
                        :inherit 'italic)
    ;; (set-face-attribute 'org-priority nil :family "ETBookOT" :slant 'normal)
    ;; (set-face-attribute 'org-checkbox-statistics-todo nil :family "ETBookOT" :slant 'italic :height 1.3)
    ;; (set-face-attribute 'org-todo nil :family "Overpass")
    ))

;;;; Configure fonts for specific natural languages

(let-alist
    (akirak/check-fonts
     '(
       ;; Japanese font
       ;; Alternatives: Source Han Sans JP, Noto Sans Mono CJK JP, Ume Gothic C4
       (japanese-font "HarenosoraMincho")
       ;; Example:
       ;; 消費生活用製品の製造又は輸入事業者は、重大な製品事故が発生したことを知ったときは
       ;; 10日以内に消費者庁に報告しなければなりません。消費者庁は、当該事故情報を迅速に
       ;; 公表するなどの措置を行います。

       ;; Font that supports both simplified and traditional Chinese
       ;; (unified-chinese-font "cwTeX Q Fangsong")

       ;; Simplified Chinese font
       ;; Alternatives: Adobe Heiti Std R, Noto Sans Mono CJK SC, WenQuanYi Micro Hei Mono
       (simplified-chinese-font "AR PL UKai CN")
       ;; Example:
       ;; 2日早上，海关关员在办理“广州东至香港红磡”直通列车的通关手续时，
       ;; 发现6名中国籍旅客结伴走“无申报通道”出境，且神色慌张、眼神躲闪。
       ;; Cite: http://www.ting-yi-ting.com/entry/2018/05/09/213152

       ;; Traditional Chinese font
       ;; Alternatives: Source Han Sans TW, Noto Sans Mono CJK TC
       (traditional-chinese-font "AR PL UKai TW")
       ;; Example:
       ;; 藝人吳建豪與新加坡百億千金石貞善(Arissa)2013年結婚，兩人屢屢傳出婚變消息，
       ;; 婚後也鮮少公開露面，夫妻兩甚至還一度在IG上公開互嗆，讓外界一頭霧水，先前再被爆出，
       ;; 吳建豪早就主動和老婆提出離婚要求，但石貞善不願放手；據悉，一名自稱兩人密友爆料，
       ;; 表示男方已連寄2封離婚協書，女方雖然試圖挽回，不過最終仍決心離婚。

       ;; CJK-misc font
       ;; Alternatives: WenQuanYi Micro Hei Mono
       ;; (cjk-misc-font )
       ))
  (set-fontset-font "fontset-default" 'kana .japanese-font)
  ;; TODO: Define fallback for Japanese kanjis that are unavailble in Chinese fonts
  (set-fontset-font "fontset-default" 'han .simplified-chinese-font)
  (set-fontset-font "fontset-default" 'bopomofo .traditional-chinese-font))

(provide 'setup-typography)
