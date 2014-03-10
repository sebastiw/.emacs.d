
(defun setup-elisp ()
  (require 'elisp-slime-nav)
  (require 'rainbow-delimiters)

  (elisp-slime-nav-mode)
  (rainbow-delimiters-mode))

(add-hook 'emacs-lisp-mode-hook 'setup-elisp)

(provide 'init-elisp)
