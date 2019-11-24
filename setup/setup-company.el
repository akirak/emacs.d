;;;; General settings

(use-package company
  :general
  (:keymaps 'company-mode-map
            "M-/" #'company-complete)
  (:keymaps 'company-active-map :package 'company
            "C-f" #'company-complete-selection
            "C-n" #'company-select-next
            "C-p" #'company-select-previous
            "M-/" #'company-other-backend
            "<f1>" #'company-show-doc-buffer)
  :config
  (defun akirak/set-default-company-backends ()
    (set (make-local-variable 'company-backends)
         ;; TODO: Refine the default backend configuration
         (cond
          ((derived-mode-p 'prog-mode)
           '(company-capf
             (company-dabbrev-code
              company-gtags
              company-etags
              company-keywords)
             company-files
             company-dabbrev))
          ((and (fboundp 'text-mode)
                (derived-mode-p 'text-mode))
           '((company-capf
              company-dabbrev)
             company-keywords
             company-files))
          ((derived-mode-p 'git-commit-mode)
           '((company-dabbrev company-yankpad company-yasnippet)
             (company-dabbrev-code company-gtags company-etags)))
          (t
           (company-capf
            company-keywords)
           company-dabbrev
           company-files))))
  (cl-delete 'company-echo-metadata-frontend
             (default-value 'company-frontends))
  :hook
  ((prog-mode text-mode git-commit-mode) . company-mode)
  (company-mode . akirak/set-default-company-backends)
  :custom
  (company-idle-delay nil)
  (company-require-match nil)
  (company-dabbrev-downcase nil)
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
  (company-dabbrev-other-buffers t "Search all buffers for company-dabbrev")
  (company-dabbrev-code-other-buffers 'code)
  (company-tooltip-align-annotations nil))

(use-package company-posframe
  :config/el-patch
  ;; Override company-posframe-show for displaying the list of active
  ;; backends in the mode line.
  (defface company-posframe-active-backend-name
    '((nil :inherit mode-line-emphasis))
    "Face for the active backend name in the header line.")

  (defun company-posframe-format-backend-name (backend)
    (cl-typecase backend
      (symbol (string-remove-prefix "company-" (symbol-name backend)))
      (list (format "[%s]" (mapconcat #'company-posframe-format-backend-name backend "|")))
      (otherwise "-")))

  (el-patch-defun company-posframe-show ()
    "Show company-posframe candidate menu."
    (let* ((height (min company-tooltip-limit company-candidates-length))
           (meta (company-fetch-metadata))
           (length (+ company-candidates-length
                      (if meta 1 0)))
           (lines (mapcar (lambda (x)
                            (let ((meta (get-text-property 0 'meta x)))
                              (concat x "  " (or meta ""))))
                          (company--create-lines company-selection height)))
           (backend-names (mapconcat
                           (lambda (backend)
                             (if (equal backend company-backend)
                                 (propertize (company-posframe-format-backend-name backend)
                                             'face 'company-posframe-active-backend-name)
                               (company-posframe-format-backend-name backend)))
                           company-backends "|"))
           (width (length (car lines)))
           (contents (concat (mapconcat #'identity lines "\n")
                             (if meta
                                 (format "\n%s"
                                         (propertize (if (> (length meta) width)
                                                         (substring meta 0 width)
                                                       meta)
                                                     'face 'font-lock-comment-face))
                               "")))
           (buffer (get-buffer-create company-posframe-buffer)))
      ;; FIXME: Do not support mouse at the moment, so remove mouse-face
      (setq contents (copy-sequence contents))
      (remove-text-properties 0 (length contents) '(mouse-face nil) contents)
      (with-current-buffer buffer
        (setq-local overriding-local-map company-posframe-active-map)
        (setq-local mode-line-format `(,backend-names)))
      (posframe-show buffer
                     :string contents
                     :position (- (point) (length company-prefix))
                     :height (1+ height) :width width
                     :x-pixel-offset (* -1 company-tooltip-margin (default-font-width))
                     ;; :respect-header-line t
                     :respect-mode-line t
                     :font company-posframe-font
                     :min-width company-tooltip-minimum-width
                     :background-color (face-attribute 'company-tooltip :background))))
  :hook
  (company-mode . company-posframe-mode))

(use-package company-quickhelp
  :straight (:host github :repo "akirak/company-quickhelp"
                   :branch "dedicated-window")
  :after company
  :custom
  (company-quickhelp-margin 15)
  :hook
  (company-mode . company-quickhelp-local-mode))

;;;; Specific backends

;;;;; General
(use-package company-emoji
  :after company
  :company text-mode)

(use-package company-dict
  :disabled t)

;;;;; Mode-specific backends

(use-package company-lsp
  ;; For company completion and snippets
  ;;
  ;; If the language server for the language doesn't support returning
  ;; snippets, you can define one one your own by customizing the
  ;; variable `company-lsp--snippet-functions'.
  ;; See the following document for details.
  ;; https://github.com/tigersoldier/company-lsp#defining-completion-snippet-for-a-certain-language
  :after (company lsp)
  :company lsp-mode)

(use-package company-ansible
  :after (company ansible)
  :company ansible)

(use-package company-shell
  :after (company shell)
  :company shell-script-mode)

(use-package company-restclient
  :after (company restclient)
  :company restclient-mode)

(use-package company-nixos-options
  :after (company nix-mode)
  :straight (:host github :repo "travisbhartwell/nix-emacs")
  :company nix-mode)

(use-package readline-complete
  :after (company comint)
  :company
  ((comint-mode shell-mode) . company-readline))

(provide 'setup-company)
