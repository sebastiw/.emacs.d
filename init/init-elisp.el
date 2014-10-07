
(add-hook 'emacs-lisp-mode-hook 'setup-elisp)

(provide 'init-elisp)

(defun setup-elisp ()
  (install-package 'elisp-slime-nav)
  (install-package 'rainbow-delimiters)
  (install-package 'paredit)

  (require 'elisp-slime-nav)
  (require 'paredit)

  (turn-on-elisp-slime-nav-mode)
  (enable-paredit-mode))
