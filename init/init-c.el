(add-hook 'c-mode-hook
          (lambda () (local-set-key (kbd "M-,") #'pop-tag-mark)))
(add-hook 'c-mode-hook
          (lambda () (local-set-key (kbd "M-*") #'tags-loop-continue)))


(provide 'init-c)
