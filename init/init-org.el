
(provide 'init-org)

(eval-after-load "org"
  '(progn
     (setq
      ;; Indent after a new node?
      org-adapt-indentation t
      ;; Where to keep org agenda files
      org-agenda-files (list "~/ORG/")
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

     (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
        (java . t)
        (dot . t)
        (ditaa . t)
        (R . t)
        (python . t)
        (ruby . t)
        (gnuplot . t)
        (clojure . t)
        (sh . t)
        (ledger . t)
        (org . t)
        (plantuml . t)
        (latex . t)))

     (define-key org-mode-map (kbd "C-c l") 'org-store-link)
     (define-key org-mode-map (kbd "C-c c") 'org-capture)
     (define-key org-mode-map (kbd "C-c a") 'org-agenda)
;     (define-key org-mode-map (kbd "C-c C-c") 'org-todo)
;     (define-key org-mode-map (kbd "C-c C-k") 'org-ctrl-c-ctrl-c)

     ;; I really like to change windows with C-<tab>
     (define-key org-mode-map (kbd "C-<tab>") 'other-window)

     (lambda () (font-lock-add-keywords nil '(("\\<\\(FIXME\\|UNREACHABLE\\|REACHABLE\\|BUG\\)" 1 font-lock-warning-face t))))

     (define-key org-mode-map (kbd "C-#") 'org-begin-template)
     (defun org-begin-template ()
       "Make a template at point."
       (interactive)
       (if (org-at-table-p)
           (call-interactively 'org-table-rotate-recalc-marks)
         (let* ((choices '(("s" . "SRC")
                           ("e" . "EXAMPLE")
                           ("q" . "QUOTE")
                           ("v" . "VERSE")
                           ("c" . "CENTER")
                           ("l" . "LaTeX")
                           ("h" . "HTML")
                           ("a" . "ASCII")))
                (key
                 (key-description
                  (vector
                   (read-key
                    (concat (propertize "Template type: " 'face 'minibuffer-prompt)
                            (mapconcat (lambda (choice)
                                         (concat (propertize (car choice) 'face 'font-lock-type-face)
                                                 ": "
                                                 (cdr choice)))
                                       choices
                                       ", ")))))))
           (let ((result (assoc key choices)))
             (when result
               (let ((choice (cdr result)))
                 (cond
                  ((region-active-p)
                   (let ((start (region-beginning))
                         (end (region-end)))
                     (goto-char end)
                     (insert "\n#+END_" choice)
                     (goto-char start)
                     (insert "#+BEGIN_" choice "\n")))
                  (t
                   (insert "#+BEGIN_" choice "\n")
                   (save-excursion (insert "\n#+END_" choice))))))))))
))
