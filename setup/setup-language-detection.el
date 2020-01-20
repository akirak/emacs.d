(use-package language-detection
  :preface
  ;; From https://github.com/andreasjansson/language-detection.el#eww-syntax-highlighting
  (defcustom akirak/language-detection-modes-alist
    '((ada ada-mode)
      ;; I never write awk, so it is likely to be python
      (awk python-mode)
      ;; (awk awk-mode)
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
  :config
  (defun akirak/language-detection-buffer-mode ()
    (when-let* ((language (language-detection-buffer))
                (modes (or (alist-get language akirak/language-detection-modes-alist)
                           (progn
                             (message "language-detection: Cannot find mode for %s"
                                      language)
                             nil))))
      (cl-find-if #'fboundp modes)))
  (defun akirak/language-detection-auto-major-mode ()
    (interactive)
    (when-let ((mode (akirak/language-detection-buffer-mode )))
      (funcall mode))))

(provide 'setup-language-detection)
