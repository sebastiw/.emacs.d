
(defun setup-elisp ()
  (install-package 'elisp-slime-nav)
  (install-package 'rainbow-delimiters)
  (require 'elisp-slime-nav)
  (require 'rainbow-delimiters)

  (elisp-slime-nav-mode)
  (rainbow-delimiters-mode))

(add-hook 'emacs-lisp-mode-hook 'setup-elisp)

(provide 'init-elisp)
