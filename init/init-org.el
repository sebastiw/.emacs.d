
(provide 'init-org)

(eval-after-load 'org-mode
  (setq
        ;; Don't indent after a new node.
        org-adapt-indentation nil
        ;; Where to keep org agenda files
        org-agenda-files (list "~/.org")
        ;; Stupid yanks, Monday is the first day of the week
        calendar-week-start-day 1
        ;; Do not dim blocked items
        org-agenda-dim-blocked-tasks nil
        ;; How many days to include in overview
        org-agenda-span 'week
        ;; Show all occurrences of a repeating timestamp
        org-agenda-repeating-timestamp-show-all nil
        ;; Don't show deadlines if the item is done
        org-agenda-skip-deadline-if-done t
        ;; Don't show scheduled items in agenda if done
        org-agenda-skip-scheduled-if-done t
        ;; Start agenda on the current day
        org-agenda-start-on-weekday nil
        ;; Unchecked boxes will block switching the parent to DONE
        org-enforce-todo-checkbox-dependencies t
        ;; Provide refile targets as paths
        org-refile-use-outline-path t
        ;; Store new notes at the beginning
        org-reverse-note-order t
        ;; TeX-like sub and superscripts with X^{some} and Y_{thing}
        org-use-sub-superscripts '{}

;;;        org-agenda-tags-todo-honor-ignore-options t
;;;        org-clock-modeline-total 'today
;;;        org-mobile-force-id-on-agenda-items nil
;;;        org-habit-show-habits-only-for-today nil
        )

  (local-set-key (kbd "C-c l") 'org-store-link)
  (local-set-key (kbd "C-c c") 'org-capture)
  (local-set-key (kbd "C-c a") 'org-agenda)
  (local-set-key (kbd "C-c C-c") 'org-todo)
  (local-set-key (kbd "C-c C-k") 'org-ctrl-c-ctrl-c)

  ;; I really like to change windows with C-<tab>
  (local-set-key (kbd "C-<tab>") 'other-window)

  (lambda () (font-lock-add-keywords nil '(("\\<\\(FIXME\\|UNREACHABLE\\|REACHABLE\\|BUG\\)" 1 font-lock-warning-face t)))))
