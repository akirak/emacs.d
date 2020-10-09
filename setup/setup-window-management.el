(setq split-height-threshold 50
      split-width-threshold nil)

(use-package windmove
  :config
  (windmove-default-keybindings 'control))

(use-package windswap
  :config
  (windswap-default-keybindings 'control 'shift))

;;;; display-buffer configuration

(use-package shackle
  :init
  (shackle-mode 1)
  :custom
  (shackle-default-rules '(:select t))
  (shackle-default-ratio 0.4)
  (shackle-default-alignment 'below)
  (shackle-rules '(
                   ("\\*ivy-occur counsel-projectile " :regexp t :align left :ratio 0.15)
                   ;; Shackle rules for org-mode
                   ;; org-mks should be substituted with the menu function in org-starter.
                   ("*Org Select*" :align below :ratio 0.3)
                   ("*org clocking*" :other t)
                   ("*Org Edna Edit Blocker/Trigger*" :align below :ratio 0.3)
                   ("*Org Note*" :align below :ratio 0.3)
                   ("*compilation*" :align below :ratio 0.4)
                   ("*Occur*" :align below :ratio 0.25)
                   ("*lispy-message*" :align below :ratio 0.4)
                   ;; This rule does not work since org 9.3.6.
                   ;; I will disable them for now.
                   ;; ("*Capture*" :inhibit-window-quit t :custom akirak/display-org-capture-temp-buffer)
                   ("^CAPTURE-" :inhbit-window-quit t :custom akirak/display-org-capture-buffer)
                   ;; This should precede the generic helm rule
                   ("*helm top*" :same t)
                   ("*helm-descbinds*" :other t)
                   ("\\*helm.*\\*" :regexp t :ratio 0.25 :align below)
                   ("*Messages*" :align below :ratio 0.3 :noselect t)
                   ("*Warnings*" :align below :ratio 0.3 :noselect t)
                   ("*Backtrace*" :align below :ratio 0.4 :noselect t)
                   (" *Agenda Commands*" :align below :ratio 0.3)
                   ("*Calendar*" :align below :ratio 0.3)
                   ("*Google Translate*" :align below :ratio 0.3)
                   ("*Org Links*" :ratio 0.1 :align below)
                   ("*Help*" :other t)
                   ("\\*Org Agenda" :regexp t :other t))))

(with-eval-after-load 'org
  (advice-add 'org-switch-to-buffer-other-window
              :override #'switch-to-buffer-other-window))

(defun akirak/display-buffer-split-below (buf alist)
  "Split the current window below and display the buffer in the new window.

Based on `display-buffer-split-below-and-attach' in pdf-utils.el."
  (let ((window (selected-window))
        (height (cdr (assq 'window-height alist)))
        newwin)
    (when height
      (when (floatp height)
        (setq height (round (* height (frame-height)))))
      (setq height (- (max height window-min-height))))
    (setq newwin (window--display-buffer
                  buf
                  (split-window-below height)
                  'window alist))
    (set-window-dedicated-p newwin t)
    newwin))

(defun akirak/display-org-todo-buffer (buffer &rest _args)
  ;; Don't use posframe from inside org-agenda
  (unless (with-current-buffer (window-buffer) (derived-mode-p 'org-agenda-mode))
    ;; Delete an existing window for the buffer
    (delete-window (get-buffer-window buffer))
    (posframe-show buffer
                   :width
                   (- (window-width) (car (posn-actual-col-row (posn-at-point (point)))))
                   :height
                   (1+ (length org-todo-sets))
                   :poshandler #'posframe-poshandler-point-bottom-left-corner)
    (set-buffer buffer)))

;; (advice-add #'org-fast-todo-selection :around
;;             (lambda (orig &optional _arg)
;;               (unwind-protect
;;                   (funcall orig _arg)
;;                 (posframe-delete " *Org todo*"))))

(defun akirak/display-org-capture-temp-buffer (buffer-or-name &rest _args)
  (let ((bottom-left-window (cl-find-if (lambda (w)
                                          (and (window-at-side-p w 'bottom)
                                               (window-at-side-p w 'left)))
                                        (window-list))))
    (if (and bottom-left-window
             (not (eq 'exwm-mode (buffer-local-value
                                  'major-mode
                                  (window-buffer bottom-left-window)))))
        (posframe-show buffer-or-name
                       :poshandler #'posframe-poshandler-frame-bottom-left-corner
                       ;; TODO: Fix the border color of the posframe
                       :internal-border-color "White"
                       :internal-border-width 2
                       :width 80
                       :height 20)
      (display-buffer-in-side-window buffer-or-name
                                     '((side . bottom))))))

(defun akirak/display-org-capture-buffer (buffer &rest args)
  (cond
   ((one-window-p)
    (split-window-sensibly)
    (switch-to-buffer buffer))
   ((> (window-height) 30)
    (akirak/display-buffer-split-below buffer args))
   (t
    (switch-to-buffer-other-window buffer))))

;;;; Window management rules that can't be configured by shackle
;;;;; org-mode
(setq-default org-agenda-window-setup 'current-window
              org-src-window-setup 'split-window-below
              org-indirect-buffer-display 'current-window)

(advice-add 'delete-other-windows
            :before-while #'akirak/allow-delete-other-windows-p)

(defun akirak/allow-delete-other-windows-p (&rest _args)
  (not (memq this-command '(org-todo
                            org-agenda-todo
                            org-agenda))))

;; Use el-patch to prevent the other windows from being deleted.
(el-patch-defun org-capture-place-template (&optional inhibit-wconf-store)
  "Insert the template at the target location, and display the buffer.
When `inhibit-wconf-store', don't store the window configuration, as it
may have been stored before."
  (unless inhibit-wconf-store
    (org-capture-put :return-to-wconf (current-window-configuration)))
  (el-patch-remove (delete-other-windows))
  (org-switch-to-buffer-other-window
   (org-capture-get-indirect-buffer (org-capture-get :buffer) "CAPTURE"))
  (widen)
  (org-show-all)
  (goto-char (org-capture-get :pos))
  (setq-local outline-level 'org-outline-level)
  (pcase (org-capture-get :type)
    ((or `nil `entry) (org-capture-place-entry))
    (`table-line (org-capture-place-table-line))
    (`plain (org-capture-place-plain-text))
    (`item (org-capture-place-item))
    (`checkitem (org-capture-place-item)))
  (org-capture-mode 1)
  (setq-local org-capture-current-plist org-capture-plist))

(el-patch-defun org-completing-read (prompt &optional collection predicate require-match
                                            initial-input hist def inherit-input-method
                                            &rest args)
  "Completing-read with SPACE being a normal character."
  (let ((enable-recursive-minibuffers t)
        (candidates (cadr args))
        (minibuffer-local-completion-map
         (copy-keymap minibuffer-local-completion-map)))
    (define-key minibuffer-local-completion-map " " 'self-insert-command)
    (define-key minibuffer-local-completion-map "?" 'self-insert-command)
    (define-key minibuffer-local-completion-map (kbd "C-c !")
      'org-time-stamp-inactive)
    (if (or candidates
            (not (eq hist 'org-capture--prompt-history)))
        (apply #'completing-read prompt collection predicate require-match
               initial-input hist def inherit-input-method
               args)
      (read-string prompt initial-input hist def inherit-input-method))))

;; This is a big function, but it hasn't been modified for several years.
;; I think it is relatively safe to patch the function.
;; (el-patch-defun org-agenda-get-restriction-and-command (prefix-descriptions)
;;   "The user interface for selecting an agenda command."
;;   (catch 'exit
;;     (let* ((bfn (buffer-file-name (buffer-base-buffer)))
;;            (restrict-ok (and bfn (derived-mode-p 'org-mode)))
;;            (region-p (org-region-active-p))
;;            (custom org-agenda-custom-commands)
;;            (selstring "")
;;            restriction second-time
;;            c entry key type match prefixes rmheader header-end custom1 desc
;;            line lines left right n n1)
;;       (save-window-excursion
;;         (el-patch-remove (delete-other-windows))
;;         (org-switch-to-buffer-other-window " *Agenda Commands*")
;;         (erase-buffer)
;;         (insert (eval-when-compile
;;                   (let ((header
;;                          "Press key for an agenda command:
;; --------------------------------        <   Buffer, subtree/region restriction
;; a   Agenda for current week or day      >   Remove restriction
;; t   List of all TODO entries            e   Export agenda views
;; m   Match a TAGS/PROP/TODO query        T   Entries with special TODO kwd
;; s   Search for keywords                 M   Like m, but only TODO entries
;; /   Multi-occur                         S   Like s, but only TODO entries
;; ?   Find :FLAGGED: entries              C   Configure custom agenda commands
;; *   Toggle sticky agenda views          #   List stuck projects (!=configure)
;; ")
;;                         (start 0))
;;                     (while (string-match
;;                             "\\(^\\|   \\|(\\)\\(\\S-\\)\\( \\|=\\)"
;;                             header start)
;;                       (setq start (match-end 0))
;;                       (add-text-properties (match-beginning 2) (match-end 2)
;;                                            '(face bold) header))
;;                     header)))
;;         (setq header-end (point-marker))
;;         (while t
;;           (setq custom1 custom)
;;           (when (eq rmheader t)
;;             (org-goto-line 1)
;;             (re-search-forward ":" nil t)
;;             (delete-region (match-end 0) (point-at-eol))
;;             (forward-char 1)
;;             (looking-at "-+")
;;             (delete-region (match-end 0) (point-at-eol))
;;             (move-marker header-end (match-end 0)))
;;           (goto-char header-end)
;;           (delete-region (point) (point-max))

;;           ;; Produce all the lines that describe custom commands and prefixes
;;           (setq lines nil)
;;           (while (setq entry (pop custom1))
;;             (setq key (car entry) desc (nth 1 entry)
;;                   type (nth 2 entry)
;;                   match (nth 3 entry))
;;             (if (> (length key) 1)
;;                 (cl-pushnew (string-to-char key) prefixes :test #'equal)
;;               (setq line
;;                     (format
;;                      "%-4s%-14s"
;;                      (org-add-props (copy-sequence key)
;;                          '(face bold))
;;                      (cond
;;                       ((string-match "\\S-" desc) desc)
;;                       ((eq type 'agenda) "Agenda for current week or day")
;;                       ((eq type 'agenda*) "Appointments for current week or day")
;;                       ((eq type 'alltodo) "List of all TODO entries")
;;                       ((eq type 'search) "Word search")
;;                       ((eq type 'stuck) "List of stuck projects")
;;                       ((eq type 'todo) "TODO keyword")
;;                       ((eq type 'tags) "Tags query")
;;                       ((eq type 'tags-todo) "Tags (TODO)")
;;                       ((eq type 'tags-tree) "Tags tree")
;;                       ((eq type 'todo-tree) "TODO kwd tree")
;;                       ((eq type 'occur-tree) "Occur tree")
;;                       ((functionp type) (if (symbolp type)
;;                                             (symbol-name type)
;;                                           "Lambda expression"))
;;                       (t "???"))))
;;               (cond
;;                ((not (org-string-nw-p match)) nil)
;;                (org-agenda-menu-show-matcher
;;                 (setq line
;;                       (concat line ": "
;;                               (cond
;;                                ((stringp match)
;;                                 (propertize match 'face 'org-warning))
;;                                ((listp type)
;;                                 (format "set of %d commands" (length type)))))))
;;                (t
;;                 (org-add-props line nil 'help-echo (concat "Matcher: " match))))
;;               (push line lines)))
;;           (setq lines (nreverse lines))
;;           (when prefixes
;;             (mapc (lambda (x)
;;                     (push
;;                      (format "%s   %s"
;;                              (org-add-props (char-to-string x)
;;                                  nil 'face 'bold)
;;                              (or (cdr (assoc (concat selstring
;;                                                      (char-to-string x))
;;                                              prefix-descriptions))
;;                                  "Prefix key"))
;;                      lines))
;;                   prefixes))

;;           ;; Check if we should display in two columns
;;           (if org-agenda-menu-two-columns
;;               (progn
;;                 (setq n (length lines)
;;                       n1 (+ (/ n 2) (mod n 2))
;;                       right (nthcdr n1 lines)
;;                       left (copy-sequence lines))
;;                 (setcdr (nthcdr (1- n1) left) nil))
;;             (setq left lines right nil))
;;           (while left
;;             (insert "\n" (pop left))
;;             (when right
;;               (if (< (current-column) 40)
;;                   (move-to-column 40 t)
;;                 (insert "   "))
;;               (insert (pop right))))

;;           ;; Make the window the right size
;;           (goto-char (point-min))
;;           (if second-time
;;               (when (not (pos-visible-in-window-p (point-max)))
;;                 (org-fit-window-to-buffer))
;;             (setq second-time t)
;;             (org-fit-window-to-buffer))

;;           ;; Ask for selection
;;           (message "Press key for agenda command%s:"
;;                    (if (or restrict-ok org-agenda-overriding-restriction)
;;                        (if org-agenda-overriding-restriction
;;                            " (restriction lock active)"
;;                          (if restriction
;;                              (format " (restricted to %s)" restriction)
;;                            " (unrestricted)"))
;;                      ""))
;;           (setq c (read-char-exclusive))
;;           (message "")
;;           (cond
;;            ((assoc (char-to-string c) custom)
;;             (setq selstring (concat selstring (char-to-string c)))
;;             (throw 'exit (cons selstring restriction)))
;;            ((memq c prefixes)
;;             (setq selstring (concat selstring (char-to-string c))
;;                   prefixes nil
;;                   rmheader (or rmheader t)
;;                   custom (delq nil (mapcar
;;                                     (lambda (x)
;;                                       (if (or (= (length (car x)) 1)
;;                                               (/= (string-to-char (car x)) c))
;;                                           nil
;;                                         (cons (substring (car x) 1) (cdr x))))
;;                                     custom))))
;;            ((eq c ?*)
;;             (call-interactively 'org-toggle-sticky-agenda)
;;             (sit-for 2))
;;            ((and (not restrict-ok) (memq c '(?1 ?0 ?<)))
;;             (message "Restriction is only possible in Org buffers")
;;             (ding) (sit-for 1))
;;            ((eq c ?1)
;;             (org-agenda-remove-restriction-lock 'noupdate)
;;             (setq restriction 'buffer))
;;            ((eq c ?0)
;;             (org-agenda-remove-restriction-lock 'noupdate)
;;             (setq restriction (if region-p 'region 'subtree)))
;;            ((eq c ?<)
;;             (org-agenda-remove-restriction-lock 'noupdate)
;;             (setq restriction
;;                   (cond
;;                    ((eq restriction 'buffer)
;;                     (if region-p 'region 'subtree))
;;                    ((memq restriction '(subtree region))
;;                     nil)
;;                    (t 'buffer))))
;;            ((eq c ?>)
;;             (org-agenda-remove-restriction-lock 'noupdate)
;;             (setq restriction nil))
;;            ((and (equal selstring "") (memq c '(?s ?S ?a ?t ?m ?L ?C ?e ?T ?M ?# ?! ?/ ??)))
;;             (throw 'exit (cons (setq selstring (char-to-string c)) restriction)))
;;            ((and (> (length selstring) 0) (eq c ?\d))
;;             (delete-window)
;;             (org-agenda-get-restriction-and-command prefix-descriptions))

;;            ((equal c ?q) (error "Abort"))
;;            (t (user-error "Invalid key %c" c))))))))

;;;; Delete compilation window

;; This configuration does not depend on shackle, but I will put it in this file
;; because it is closely related to window management.

;; Based on https://www.reddit.com/r/emacs/comments/8q5uup/close_popwin_on_successful_compilation/e0h8jbi/
(defun akirak/close-compilation-on-finish (buf status)
  (when (string-match "finished" status)
    (message "Compilation successful")
    (run-with-timer 1 nil #'delete-window (get-buffer-window buf))))

(setq-default compilation-finish-functions #'akirak/close-compilation-on-finish)

;; Workaround for a weird behaviour in `org-src-switch-to-buffer'
;; when `org-src-window-setup' is set to `split-window-below'.
;; It splits the window even when exiting the source buffer,
;; which is not what I expect.

(setq-default org-src-window-setup 'split-window-below)

(defvar-local akirak/org-src-split-window nil)

(defun akirak/ad-around-org-src-switch-to-buffer (orig buffer context)
  (if (eq org-src-window-setup 'split-window-below)
      (if (memq context '(exit save))
          (progn
            (ignore-errors (delete-window))
            (if-let ((window (get-buffer-window buffer)))
                (select-window window)
              (switch-to-buffer buffer)))
        (cond
         ((> (window-total-width) 160)
          (split-window-right))
         (t
          (split-window-below)))
        (other-window 1)
        (switch-to-buffer buffer)
        (set-window-dedicated-p (selected-window) t))
    (funcall orig buffer context)))

(advice-add 'org-src-switch-to-buffer :around
            'akirak/ad-around-org-src-switch-to-buffer)

;; Ignore some windows
(defcustom akirak/skipped-window-buffers
  '(" *LV*")
  "List of buffer names whose windows should never be selected.")

(defcustom akirak/skipped-window-major-modes
  '(treemacs-mode)
  "List of buffer names whose windows should never be selected.")

(defun akirak/ad-around-next-window--for-ignore-window (orig &rest args)
  (let ((window (apply orig args)))
    (if (or (member (buffer-name (window-buffer window)) akirak/skipped-window-buffers)
            (memq (buffer-local-value 'major-mode (window-buffer window)) akirak/skipped-window-major-modes))
        (apply orig window (cdr args))
      window)))

(advice-add 'next-window
            :around #'akirak/ad-around-next-window--for-ignore-window)

;; By default, smart-jump always displays the destination buffer in
;; another window in emacs-lisp-mode, which I really don't like.
;;
;; This function advice is a workaround to alter the buffer switching
;; function used by `find-function'.
(defun akirak/ad-around-find-function-do-it (orig symbol type switch-fn)
  (funcall orig symbol type 'akirak/switch-buffer-maybe-same-window))

(advice-add 'find-function-do-it
            :around 'akirak/ad-around-find-function-do-it)

;;;; Window manipulation commands

(defun akirak/switch-buffer-maybe-same-window (buffer &rest args)
  "Display BUFFER in the same window if the buffer refers to the same file."
  (if (file-equal-p (buffer-file-name (current-buffer))
                    (buffer-file-name buffer))
      (apply 'pop-to-buffer-same-window buffer args)
    (apply 'pop-to-buffer buffer args)))

(defun akirak/split-window-aggressively ()
  (cond
   ((> (akirak/available-width-for-new-window) 80)
    (split-window-horizontally))
   ((and (not (window-dedicated-p))
         (not (window-minibuffer-p))
         (window-splittable-p (selected-window)))
    (split-window-below))))

(defun akirak/available-width-for-new-window (&optional window)
  (let ((window (or (selected-window)))
        (windows (list window))
        (leftw window)
        (rightw window))
    (while (setq leftw (window-in-direction 'left leftw))
      (push leftw windows))
    (while (setq rightw (window-in-direction 'right rightw))
      (push rightw windows))
    (-sum (-map (lambda (wnd)
                  (if (window-dedicated-p wnd)
                      0
                    (- (+ (window-width wnd)
                          ;; perfect-margin.el sets window margins
                          (pcase (window-margins wnd)
                            (`(,_) 0)
                            (`(,left . ,right) (+ left right))))
                       80)))
                windows))))

(defun akirak/split-window-and-select ()
  (interactive)
  (pcase current-prefix-arg
    ('(4)
     (progn
       (delete-window)
       (balance-windows)))
    (_
     (if-let ((window (akirak/split-window-aggressively)))
         (progn
           (select-window window)
           (balance-windows))
       (message "No window was created")))))

(general-def
  "C-4" #'akirak/split-window-and-select)

(provide 'setup-window-management)
