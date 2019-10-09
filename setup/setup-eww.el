(use-package eww
  :straight (:type built-in)
  :config

  ;; From https://github.com/andreasjansson/language-detection.el#eww-syntax-highlighting
  (defcustom eww-language-mode-map
    '((ada ada-mode)
      (awk awk-mode)
      (c c-mode)
      (cpp c++-mode)
      (clojure clojure-mode lisp-mode)
      (csharp csharp-mode java-mode)
      (css css-mode)
      (dart dart-mode)
      (delphi delphi-mode)
      (emacslisp emacs-lisp-mode)
      (erlang erlang-mode)
      (fortran fortran-mode)
      (fsharp fsharp-mode)
      (go go-mode)
      (groovy groovy-mode)
      (haskell haskell-mode)
      (html html-mode)
      (java java-mode)
      (javascript javascript-mode)
      (json json-mode javascript-mode)
      (latex latex-mode)
      (lisp lisp-mode)
      (lua lua-mode)
      (matlab matlab-mode octave-mode)
      (objc objc-mode c-mode)
      (perl perl-mode)
      (php php-mode)
      (prolog prolog-mode)
      (python python-mode)
      (r r-mode)
      (ruby ruby-mode)
      (rust rust-mode)
      (scala scala-mode)
      (shell shell-script-mode)
      (smalltalk smalltalk-mode)
      (sql sql-mode)
      (swift swift-mode)
      (visualbasic visual-basic-mode)
      (xml sgml-mode))
    "Alist of language names and major modes.")

  (defun eww-tag-pre (dom)
    (let ((shr-folding-mode 'none)
          (shr-current-font 'default))
      (shr-ensure-newline)
      (insert (eww-fontify-pre dom))
      (shr-ensure-newline)))

  (defun eww-fontify-pre (dom)
    (with-temp-buffer
      (shr-generic dom)
      (let ((mode (when (featurep 'language-detection)
                    (eww-buffer-auto-detect-mode))))
        (when mode
          (eww-fontify-buffer mode)))
      (buffer-string)))

  (defun eww-fontify-buffer (mode)
    (delay-mode-hooks (funcall mode))
    (font-lock-default-function mode)
    (font-lock-default-fontify-region (point-min)
                                      (point-max)
                                      nil))

  ;; Requires language-detection package
  (defun eww-buffer-auto-detect-mode ()
    (let* ((language (language-detection-string
                      (buffer-substring-no-properties (point-min) (point-max))))
           (modes (alist-get language eww-language-mode-map)))
      (cl-find #'fboundp modes)))
  (add-to-list 'shr-external-rendering-functions '(pre . eww-tag-pre)))

(provide 'setup-eww)