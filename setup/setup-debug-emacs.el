(defun akirak/run-emacs-with-debug-init ()
  "Run Emacs with \"--debug-init\" option."
  (interactive)
  (if current-prefix-arg
      (async-shell-command
       (read-string "Command line: "
                    (format "%s --debug-init "
                            (shell-quote-argument (car command-line-args)))))
    (async-start-process "emacs-debug" "emacs" nil "--debug-init")))

(use-package restart-emacs)

(use-package profiler
  :straight (:type built-in)
  :config
  (defun akirak/profiler-stop-and-report ()
    (interactive)
    (profiler-report)
    (profiler-stop)
    (profiler-reset)))

(akirak/bind-admin
  "<f12>" #'toggle-debug-on-error
  "e" '(nil :wk "emacs")
  "ed" #'akirak/run-emacs-with-debug-init
  "ep" (general-predicate-dispatch #'profiler-start
         :docstring "profiler"
         (profiler-running-p) #'akirak/profiler-stop-and-report)
  "er" #'restart-emacs)

(provide 'setup-debug-emacs)
