;;;; Utilities
(defun akirak/find-available-font (fonts)
  (let ((all-fonts (font-family-list)))
    (cl-loop for font in fonts
             when (member font all-fonts)
             return font)))

(defsubst akirak/set-available-font-for-charset (charset fonts)
  (when-let ((font (akirak/find-available-font fonts)))
    (set-fontset-font "fontset-default" charset (font-spec :family font))))

;;;; Setting the default font
(defvar akirak/default-font nil)

;; This solution seem to work for now, but may be changed in the future.
;; https://superuser.com/questions/692173/emacs-default-font-does-not-work-with-new-frames
(defun akirak/set-default-font (&optional family)
  (let ((family (or family akirak/default-font)))
    (set-default-font (format "%s-%f" family akirak/default-font-size)
                      t t)
    (message "Default font set to %s" family)))

(defcustom akirak/default-font-size 10.5
  "Default font size in pixels."
  :set (lambda (symbol value)
         (set symbol value)
         (akirak/set-default-font)))

(defcustom akirak/default-fonts
  '("Hack"
    "Noto Sans Mono"
    "Monofur"
    "Meslo LG S"
    "mononoki")
  "List of preferred English monospace fonts."
  :set-after '(akirak/default-font-size)
  :set (lambda (symbol value)
         (set symbol value)
         (when-let ((default-font (akirak/find-available-font value)))
           (setq akirak/default-font default-font)
           (akirak/set-default-font))))

(add-hook 'emacs-startup-hook 'akirak/set-default-font)

(defun akirak/set-random-default-font ()
  (interactive)
  (akirak/set-default-font (elt akirak/default-fonts
                                (random (length akirak/default-fonts)))))

;;;; International fonts

;;;;; Simplified Chinese (han)
(defcustom akirak/chinese-han-fonts
  '("Adobe Heiti Std R"
    "Noto Sans Mono CJK SC"
    "WenQuanYi Micro Hei Mono")
  "List of preferred Chinese monospace fonts.

Example:
  2日早上，海关关员在办理“广州东至香港红磡”直通列车的通关手续时，
  发现6名中国籍旅客结伴走“无申报通道”出境，且神色慌张、眼神躲闪。

Cite: http://www.ting-yi-ting.com/entry/2018/05/09/213152"
  :set (lambda (_symbol value)
         (akirak/set-available-font-for-charset 'han value)))

;;;;; Japanese (kana)
(defcustom akirak/japanese-kana-fonts
  '("Source Han Sans JP"
    "Noto Sans Mono CJK JP"
    "Ume Gothic C4")
  "List of preferred Japanese fonts.

Example:
  消費生活用製品の製造又は輸入事業者は、重大な製品事故が発生したことを知ったときは
  10日以内に消費者庁に報告しなければなりません。消費者庁は、当該事故情報を迅速に
  公表するなどの措置を行います。"
  :set (lambda (_symbol value)
         (akirak/set-available-font-for-charset 'kana value)))

;;;;; Traditional Chinese (bopomofo)
(defcustom akirak/chinese-bopomofo-fonts
  '("Source Han Sans TW"
    "Noto Sans Mono CJK TC")
  "List of preferred traditional Chinese fonts.

Example:
  藝人吳建豪與新加坡百億千金石貞善(Arissa)2013年結婚，兩人屢屢傳出婚變消息，
  婚後也鮮少公開露面，夫妻兩甚至還一度在IG上公開互嗆，讓外界一頭霧水，先前再被爆出，
  吳建豪早就主動和老婆提出離婚要求，但石貞善不願放手；據悉，一名自稱兩人密友爆料，
  表示男方已連寄2封離婚協書，女方雖然試圖挽回，不過最終仍決心離婚。"
  :set (lambda (_symbol value)
         (akirak/set-available-font-for-charset 'bopomofo value)))

;;;;; CJK Misc
(defcustom akirak/cjk-misc-fonts
  '("WenQuanYi Micro Hei Mono")
  "List of preferred fonts for cjk-misc."
  :set (lambda (_symbol value)
         (akirak/set-available-font-for-charset 'cjk-misc value)))

;;;;; Symbols
(defcustom akirak/symbol-fonts
  '("EmojiOne Color"
    "EmojiOne"
    "Noto Color Emoji")
  "List of preferred fonts for cjk-misc."
  :set (lambda (_symbol value)
         (akirak/set-available-font-for-charset 'symbol value)))

(provide 'init-font)
