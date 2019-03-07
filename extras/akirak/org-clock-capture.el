;; TODO: Reduce the number of templates

(org-starter-def-capture "a" "Append to the current clock")

(org-starter-def-capture "as" "Source block"
  plain (clock)
  "#+BEGIN_SRC %(akirak/buffer-mode-name \"%F\")
%i
#+END_SRC" :empty-lines 1 :immediate-finish t :no-save t)

(org-starter-def-capture "al" "Link"
  plain (clock)
  "%A"
  :empty-lines 1 :immediate-finish t :no-save t)

(org-starter-def-capture "ai" "Item"
  item (clock)
  "%i%?"
  :unnarrowed t :no-save t)

(org-starter-def-capture "ac" "Check item"
  checkitem (clock)
  "[ ] %i%?"
  :unnarrowed t :no-save t)

(org-starter-def-capture "at" "Entry"
  entry (clock)
  "* %?"
  :unnarrowed t :clock-in t :clock-resume t :empty-lines 1 :no-save t)

(org-starter-def-capture "ae" "Example"
  plain (clock)
  "#+begin_example
%i
#+end_example"
  :empty-lines 1 :immediate-finish t :no-save t)

(org-starter-def-capture "aa" "Text"
  plain (clock)
  "%?"
  :empty-lines 1 :unnarrowed t :no-save t)

(org-starter-def-capture "aw" "Web page")

(org-starter-def-capture "aww" "Plain link"
  plain (clock)
  "%(org-web-tools--org-link-for-url)"
  :immediate-finish t :no-save t)

(org-starter-def-capture "awi" "Link as item"
  item (clock)
  "%(org-web-tools--org-link-for-url)"
  :immediate-finish t :no-save t :empty-lines 1)

(provide 'akirak/org-clock-capture)
