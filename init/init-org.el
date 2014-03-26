

(defun setup-org ()
  (local-set-key (kbd "C-<tab>") 'other-window)
  (local-set-key (kbd "C-c C-c") 'org-todo)
  (local-set-key (kbd "C-c C-k") 'org-ctrl-c-ctrl-c)

  (lambda () (font-lock-add-keywords nil '(("\\<\\(FIXME\\|UNREACHABLE\\|REACHABLE\\|BUG\\)" 1 font-lock-warning-face t)))))


(add-hook 'org-mode-hook 'setup-org)

(provide 'init-org)
