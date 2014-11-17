
(use-package elpy
;  :pre-load (load-file "~/.emacs.d/elpa/python-20120402/python.el")
  :ensure t
  :mode ("\\.py\\'" . elpy-mode)
  :config
  (progn
    (define-key python-mode-map (kbd "C-c C-z") 'run-python)
    (define-key python-mode-map (kbd "<backtab>") 'python-back-indent)
    (use-package jedi
      :ensure t
      :config
      (progn
        (jedi:setup)
        (jedi:ac-setup)
        (setq jedi:setup-keys t)
        (setq jedi:complete-on-dot t)
        (define-key python-mode-map (kbd "C-c C-d") 'jedi:show-doc)
        (setq jedi:tooltip-method nil)
        (set-face-attribute 'jedi:highlight-function-argument nil
                            :foreground "green")
        (define-key python-mode-map (kbd "C-c C-l") 'jedi:get-in-function-call)))
    (add-hook 'python-mode-hook (lambda () (jedi-mode t)))
    ;(add-hook 'python-mode-hook 'elpy-mode)
    ))


(provide 'init-python)
