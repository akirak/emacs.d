;;; akirak/org-cpb.el

(org-starter-def "cpb.org"
  :key "c")

(defun akirak/org-refile-to-cpb (arg)
  (interactive "P")
  (org-reverse-datetree-refile-to-file
   (org-starter-locate-file "cpb.org" nil t) nil
   :ask-always arg :prefer '("CREATED_TIME" "CREATED_AT" "CLOSED")))

(add-to-list 'org-starter-extra-refile-map
             '("c" akirak/org-refile-to-cpb "cpb"))

;;;; Capture templates

;; These capture templates are mostly based on ones by alphapapa:
;; https://www.reddit.com/r/emacs/comments/9ycgoe/do_you_guys_have_a_personal_wiki_using_orgmode_or/ea0j8nw/

;; TODO: Simplify these template definitions

(org-starter-def-capture "c" "cpb.org: Plain entry"
  entry (file+function "cpb.org" org-reverse-datetree-goto-date-in-file)
  "* %^{Heading}
:PROPERTIES:
:CREATED_TIME: %U
:END:

%(unless (string-empty-p \"%i\") \"%i\n\n\")%?"
  :clock-in t :clock-resume t :empty-lines 1)

;; (org-starter-def-capture "cq" "cpb.org: Quote"
;;   entry (file+function "cpb.org" org-reverse-datetree-goto-date-in-file)
;;   "* %(if (string-empty-p \"%a\") \"%^{Heading}\" \"%a\")
;; :PROPERTIES:
;; :CREATED_TIME: %U
;; :END:

;; %?%(unless (string-empty-p \"%x\") \"

;; #+BEGIN_QUOTE
;; %x
;; #+END_QUOTE
;; \")"
;;   :clock-in t :clock-resume t :empty-lines 1)

;; (org-starter-def-capture "cl" "cpb.org: Link to web page"
;;   entry (file+function "cpb.org" org-reverse-datetree-goto-date-in-file)
;;   "* %(org-web-tools--org-link-for-url) :@link:
;; :PROPERTIES:
;; :CREATED_TIME: %U
;; :END:

;; %?"
;;   :clock-in t :clock-resume t :empty-lines 1)

;; (org-starter-def-capture "cr" "cpb.org: Readable content of web page"
;;   entry (file+function "cpb.org" org-reverse-datetree-goto-date-in-file)
;;   "%(org-web-tools--url-as-readable-org)"
;;   :clock-in t :clock-resume t :empty-lines 1)

(provide 'akirak/org-cpb)
