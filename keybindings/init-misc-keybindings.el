(general-def
  "<M-prior>" #'git-gutter:previous-hunk
  "<M-next>" #'git-gutter:next-hunk
  "<M-insert>" #'git-gutter:stage-hunk)

(general-def
  "<M-f12>" '((lambda ()
                (interactive)
                (shell-command "setxkbmap -option ctrl:nocaps"))
              :wk "setxkbmap"))

(provide 'init-misc-keybindings)
