(use-package katawa
  :straight (katawa :host github :repo "akirak/katawa.el")
  :functions (katawa-get-some-candidates katawa-google--request))

(require 'init-web-search)

(use-package katawa-ivy
  :straight katawa
  :after katawa
  :commands (katawa-ivy katawa-ivy-fix katawa-ivy-fix-at-point)
  :config
  (ivy-add-actions 'katawa-ivy
                   '(("sg" akirak/surfraw/google "Google")
                     ("sm" akirak/google-maps-search
                      "Find the location in Google Maps")
                     ("ss" akirak/helm-search "Choose a search engine")
                     ("st" akirak/search-twitter-accounts
                      "Search accounts in twitter")
                     ("te" (lambda (query)
                             (google-translate-translate "ja" "en" query))
                      "Translate into English")
                     ("tc" (lambda (query)
                             (google-translate-translate "ja" "zh" query))
                      "Translate into Chinese")
                     ("dc" akirak/weblio-japanese-chinese-dictionary
                      "Look up the word in Weblio Chinese dictionary")
                     ("db" akirak/baidu-baike-search
                      "Define in Baidu Baike (zh)")
                     ("dw" akirak/wikipedia-japanese
                      "Define in Wikipedia (ja)"))))

(use-package katawa-ivy-exwm
  :straight katawa
  :after exwm
  :commands (katawa-ivy-exwm)
  :custom
  ;; Share the action list with katawa-ivy
  (katawa-ivy-exwm-as-katawa-ivy t))

(provide 'init-katawa)
