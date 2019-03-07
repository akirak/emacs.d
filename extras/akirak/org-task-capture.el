(org-starter-def-capture "t" "Task")

(org-starter-def-capture "tt" "Task without schedule"
  entry (file "taskpool.org")
  "* TODO %^{Heading}
:PROPERTIES:
:CREATED_TIME: %U
:END:

%i%?
"
  :clock-in t :clock-resume t :empty-lines 1)

(org-starter-def-capture "ts" "Scheduled task (with %i as title)"
  entry (file "taskpool.org")
  "* TODO %^{Heading}
DEADLINE: %^{Deadline}T SCHEDULED: %^{When to do}t
:PROPERTIES:
:CREATED_TIME: %U
:END:

%i%?
"
  :clock-in t :clock-resume t :empty-lines 1)

(org-starter-def-capture "tu" "Urgent task (with %i as title)"
  entry (file "taskpool.org")
  "* STARTED %^{Heading} @urgent
DEADLINE: %^{Deadline}T SCHEDULED: %t
:PROPERTIES:
:CREATED_TIME: %U
:END:

%i%?
"
  :clock-in t :clock-resume t)

(provide 'akirak/org-task-capture)
