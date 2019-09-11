;;;; General settings

(use-package company
  :general
  ("C-M-/" #'company-complete)
  (:keymaps 'company-mode-map
            "M-/" #'company-complete)
  (:keymaps 'company-active-map :package 'company
            "C-f" #'company-complete-selection
            "C-n" #'company-select-next
            "C-p" #'company-select-previous
            "M-/" #'company-other-backend
            "<f1>" #'company-show-doc-buffer)
  :hook
  (prog-mode . company-mode)
  (company-mode . akirak/setup-company-backend)
  :custom
  (company-idle-delay nil)
  (company-auto-complete (lambda ()
                           (and (company-tooltip-visible-p)
                                (company-explicit-action-p))))
  (company-continue-commands '(not save-buffer
                                   save-some-buffers
                                   save-buffers-kill-terminal
                                   save-buffers-kill-emacs
                                   comint-previous-matching-input-from-input
                                   comint-next-matching-input-from-input
                                   completion-at-point))
  (company-dabbrev-other-buffers 'all "Search all buffers for company-dabbrev")
  (company-tooltip-align-annotations nil))

(use-package company-posframe
  :hook
  (company-mode . company-posframe-mode))

(use-package company-quickhelp
  :after company
  :disabled t
  :hook
  (after-init . company-quickhelp-mode))

;;;; Specific backend configurations

(defvar akirak/company-major-backend-alist
  (mapcar (lambda (sym)
            (let ((name (symbol-name sym)))
              (cons (intern (concat name "-mode"))
                    (intern (concat "company-" name)))))
          '(shell
            restclient)))

(defun akirak/setup-company-backend ()
  (setq-local company-backends
              (let ((default-backends
                      '(company-capf
                        company-dabbrev
                        company-files
                        company-keywords)))
                (cond
                 ((bound-and-true-p lsp-mode)
                  (cons 'company-lsp default-backends))
                 (t
                  (if-let ((mode (apply #'derived-mode-p
                                        (mapcar #'car akirak/company-major-backend-alist))))
                      (list (alist-get mode akirak/company-major-backend-alist)
                            'company-capf)
                    default-backends))))))

;;;; Completion backends based on a minor mode

(use-package company-lsp
  ;; For company completion and snippets
  ;;
  ;; If the language server for the language doesn't support returning
  ;; snippets, you can define one one your own by customizing the
  ;; variable `company-lsp--snippet-functions'.
  ;; See the following document for details.
  ;; https://github.com/tigersoldier/company-lsp#defining-completion-snippet-for-a-certain-language
  :after (company lsp))

;;;; Completion backends based on a major mode

(use-package company-ansible
  :after (company ansible))

(use-package company-shell
  :after (company shell))

(use-package company-restclient
  :after (company restclient))

(provide 'setup-company)
