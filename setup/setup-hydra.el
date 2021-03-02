(use-package hydra
  :custom
  (hydra-hint-display-type 'lv)
  (hydra-posframe-show-params
   (list
    ;; :internal-border-width 1
    :background-color "salmon4"
    ;; :internal-border-color "red"
    :poshandler 'akirak/posframe-poshandler-smart-center))
  )

(use-package pretty-hydra
  :config/el-patch
  (defun pretty-hydra--generate (name body heads-plist)
    "Helper function to generate expressions with given NAME, BODY, HEADS-PLIST.
See `pretty-hydra-define' and `pretty-hydra-define+'."
    (let* ((separator (or (plist-get body :separator) "─"))
           (title (plist-get body :title))
           (formatter (or (plist-get body :formatter)
                          #'identity))
           (quit-key (plist-get body :quit-key))
           (docstring (->> heads-plist
                           (pretty-hydra--gen-body-docstring separator)
                           (pretty-hydra--maybe-add-title title)
                           (funcall formatter)
                           (s-prepend "\n")
                           ;; Work around for an issue
                           ;; that`fit-frame-to-buffer' seems to
                           ;; ignore trailing newlines.
                           ;;
                           ;; I don't know why this doesn't happen for
                           ;; other people, but this is necessary for
                           ;; displaying a hydra in a posframe.
                           (el-patch-add (s-append "\n "))))
           (heads (pretty-hydra--get-heads heads-plist))
           (heads (if quit-key
                      (if (listp quit-key)
                          (append heads (--map (list it nil) quit-key))
                        (append heads `((,quit-key nil))))
                    heads))
           (body (lax-plist-put body :hint nil)))
      `(progn
         (eval-and-compile
           (set (defvar ,(intern (format "%S/heads-plist" name))
                  nil
                  ,(format "heads-plist of %S." name))
                (quote ,heads-plist))
           (set (defvar ,(intern (format "%S/pretty-body" name))
                  nil
                  ,(format "pretty-body of %S." name))
                (quote ,body)))
         (defhydra ,name ,(pretty-hydra--remove-custom-opts body)
           ,docstring
           ,@heads)))))

(use-package major-mode-hydra)

(defvar akirak/hydra-stack nil)

;;;; Displaying specific hydras in posframe

(defcustom akirak/hydra-posframe-whitelist
  nil
  "List of hydras which should be displayed in posframe.")

(defun akirak/hydra-should-display-in-posframe-p (caller)
  (cl-member caller akirak/hydra-posframe-whitelist))

(defun akirak/ad-around-hydra-show-hint (orig hint caller)
  (push caller akirak/hydra-stack)
  (if (and (eq hydra-hint-display-type 'lv)
           (akirak/hydra-should-display-in-posframe-p caller))
      (let ((hydra-hint-display-type 'hydra))
        (funcall orig hint caller))
    (funcall orig hint caller)))

(advice-add 'hydra-show-hint :around 'akirak/ad-around-hydra-show-hint)

(defun akirak/ad-around-hydra-keyboard-quit (orig)
  (let ((caller (pop akirak/hydra-stack)))
    (if (and (eq hydra-hint-display-type 'lv)
             (akirak/hydra-should-display-in-posframe-p caller))
        (let ((hydra-hint-display-type 'hydra))
          (funcall orig))
      (funcall orig))))

(advice-add 'hydra-keyboard-quit :around 'akirak/ad-around-hydra-keyboard-quit)

;;;; Preventing display in posframe

(defcustom akirak/hydra-prevent-posframe-list
  '(akirak/minor-mode-hydra
    akirak/string-inflection-hydra)
  "List of hydras that should prevent from creating a posframe for help.")

(defun akirak/hydra-should-avoid-posframe-p (caller)
  (or (cl-member caller akirak/hydra-prevent-posframe-list)
      (string-suffix-p "-mode-hydra" (symbol-name caller))))

;; (defun akirak/ad-around-hydra-show-hint (orig hint caller)
;;   (push caller akirak/hydra-stack)
;;   (if (and (eq hydra-hint-display-type 'posframe)
;;            (akirak/hydra-should-avoid-posframe-p caller))
;;       (let ((hydra-hint-display-type 'lv))
;;         (funcall orig hint caller))
;;     (funcall orig hint caller)))

;; (advice-add 'hydra-show-hint :around 'akirak/ad-around-hydra-show-hint)

;; (defun akirak/ad-around-hydra-keyboard-quit (orig)
;;   (let ((caller (pop akirak/hydra-stack)))
;;     (if (and (eq hydra-hint-display-type 'posframe)
;;              (akirak/hydra-should-avoid-posframe-p caller))
;;         (let ((hydra-hint-display-type 'lv))
;;           (funcall orig))
;;       (funcall orig))))

;; (advice-add 'hydra-keyboard-quit :around 'akirak/ad-around-hydra-keyboard-quit)

;;;; Define some (but not all) major mode hydras
;; These major-mode hydras are mostly for intended for mnemonics.
(major-mode-hydra-define 'Info-mode
  (:title "Info navigation")
  ("Navigation"
   (("n" Info-next "next")
    ("p" Info-previous "previous")
    ("[" Info-backward-node "backward")
    ("]" Info-forward-node "forward")
    ("h" Info-up "up")
    ("m" Info-menu "menu")
    ("T" Info-toc "toc"))
   "History"
   (("l" Info-history-back "back")
    ("r" Info-history-forward "forward")
    ("L" Info-history "display"))
   "In this file"
   (("<" Info-top-node "beginning")
    (">" Info-final-node "end"))
   "Cross reference"
   (("f" Info-follow-reference "Follow"))))

(major-mode-hydra-define 'help-mode
  (:title "Help navigation")
  ("History"
   (("l" help-go-back "back")
    ("r" help-go-forward "forward"))
   "Button"
   (("f" help-follow-symbol "Follow"))))

(major-mode-hydra-define 'helpful-mode
  (:title "Helpful navigation")
  ("Buttons"
   (("n" forward-button "Forward")
    ("p" backward-button "Backward"))))

(provide 'setup-hydra)
