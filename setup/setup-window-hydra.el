(defhydra akirak/window-hydra (:hint nil)
  "
^^Window ^^^^         Layout^^^^       Toggle^^       Frame^^      ^^             Org clock^^
^^-------^^^^-------  ^^^^-----------  ^^-----------  ^^-----------^^-----------  ^^----------------------------
 _k_  ^^ _d_: delete  _s_/_v_: split   _o_: olivetti  _R_: rename  _F_: new       _cj_: goto       _cc_: context
_h_ _l_  _O_: other   _SPC_: layout^^  _zb_: ibuffer  _D_: delete  ^^             _co_: clock out  _ch_: history
 _j_  ^^ _x_: kill b  _=_: balance^^   _zt_: dired    _/_: select  _C-r_: reload  _cq_: cancel     ^^
_a_ce ^^ _m_: cl msgs _r_: to reg^^    _zs_: symbols  _zd_: [%s(if debug-on-error \"X\" \" \")] debug-on-error _C-v_: [%s(if view-mode \"X\" \" \")] view

"
  ("'" avy-goto-char-timer "avy" :exit t)
  ("/" select-frame-by-name :exit t)
  ("=" balance-windows)
  ("D" delete-frame :exit t)
  ("F" make-frame-command :exit t)
  ("O" delete-other-windows)
  ("R" set-frame-name)
  ("a" ace-window :exit t)
  ("b" ivy-switch-buffer "switch buf")
  ("cc" counsel-org-clock-context)
  ("ch" counsel-org-clock-history)
  ("cj" org-clock-goto)
  ("co" org-clock-out)
  ("cq" org-clock-cancel)
  ("d" delete-window)
  ("g" (select-window (frame-first-window)))
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ("m" akirak/close-message-windows)
  ("n" akirak/toggle-note-window "note")
  ("o" olivetti-mode)
  ("p" ivy-push-view "push view" :exit t)
  ("q" quit-window)
  ("r" window-configuration-to-register :exit t)
  ("s" split-window-below)
  ("t" akirak/toggle-terminal-like "term")
  ("u" winner-undo)
  ("U" winner-redo)
  ("v" split-window-right)
  ("x" kill-this-buffer-and-its-window)
  ("y" ivy-pop-view "pop view" :exit t)
  ("zb" ibuffer-sidebar-toggle-sidebar)
  ("zd" toggle-debug-on-error :exit t)
  ("zs" symbol-overlay-remove-all)
  ("zt" dired-sidebar-toggle-sidebar)
  ("{" shrink-window-horizontally)
  ("}" enlarge-window-horizontally)
  
  ("C-r" frame-workflow-reload-layout)
  ("C-v" view-mode)

  ("RET" nil)
  ("SPC" toggle-window-split))

(defun akirak/close-message-windows ()
  (interactive)
  (mapc (lambda (name)
          (when-let ((w (get-buffer-window name)))
            (delete-window w)))
        '("*Messages*" "*Warnings*" "*Backtrace*")))

(defun akirak/toggle-terminal-like ()
  "Toggle a terminal window or something like it."
  (interactive)
  ;; FIXME: Add support for more REPLs
  (akirak/shell-toggle-dedicated))

(defcustom akirak/note-buffer-regexp
  (rx bol "CAPTURE-"
      (0+ anything)
      ".org" eol)
  "Regular expression for note buffers.")

(defcustom akirak/open-note-function
  (lambda () (call-interactively 'org-capture))
  "Function to pop up a new note window.")

(defun akirak/toggle-note-window ()
  (interactive)
  (pcase (remove-if-not (lambda (buf)
                          (string-match-p akirak/note-buffer-regexp
                                          (buffer-name buf)))
                        (buffer-list))
    (`nil (funcall akirak/open-note-function))
    ((and (app (-any 'get-buffer-window) w) (guard w))
     (delete-window w))
    (bufs
     (progn
       (display-buffer-in-side-window (car bufs) '((side . bottom)))
       (select-window (get-buffer-window (car bufs)))))))

(provide 'setup-window-hydra)