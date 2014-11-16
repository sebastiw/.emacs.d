(provide 'init-elisp)

(use-package elisp-slime-nav
  :ensure t
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
    (add-hook 'lisp-interaction-mode-hook 'elisp-slime-nav-mode)))
(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))
