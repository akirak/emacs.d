(use-package reformatter
  :config

  (reformatter-define yapf
    :program "yapf")

  ;; Experimental.
  (reformatter-define golint
    :program "golint"
    :stdin nil
    :stdout nil
    :args (list (buffer-file-name)))

  (reformatter-define nixfmt
    :program "nixfmt"
    :mode t)

  (reformatter-define prettier
    :program "prettier"
    :args (list "--stdin-filepath" (buffer-file-name)))

  (reformatter-define deno-fmt
    :program "deno"
    :args '("fmt" "-"))

  (reformatter-define mix-format
    :program "mix"
    :mode t
    ;; In "mix format", if any of the files is -, then the output is
    ;; read from stdin and written to stdout.
    :args '("format" "-"))
  ;; Advice the formatting function for looking at .formatter.exs, if
  ;; any. I'm not sure who invented this first, but many people do a
  ;; similar thing, see
  ;; <https://github.com/keer2345/keer-emacs/blob/130cf6af2c12fd3e6c99c7c039fdb5ae40628120/lisp/init-format.el>
  ;; for example.
  (advice-add 'mix-format-region
              :around
              (defun akirak/ad-around-for-elixir-format-directory (orig &rest args)
                (let ((default-directory (or (locate-dominating-file default-directory
                                                                     ".formatter.exs")
                                             default-directory)))
                  (apply orig args)))))

(use-package purty
  :after purescript-mode
  :straight (:host gitlab :repo "joneshf/purty"))

(provide 'setup-reformatter)
