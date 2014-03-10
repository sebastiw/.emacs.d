
(defun setup-elisp ()
  (install-package 'elisp-slime-nav)
  (install-package 'rainbow-delimiters)
  (require 'rainbow-delimiters)
  (rainbow-delimiters-mode t)

  (elisp-slime-nav-mode))

(add-hook 'emacs-lisp-mode-hook 'setup-elisp)

(provide 'init-elisp)
