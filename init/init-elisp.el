
(defun setup-elisp ()
  (package-install 'rainbow-delimiters)
  (require 'elisp-slime-nav)
  (require 'rainbow-delimiters)

  (emacs-slime-nav-mode)
  (rainbow-delimiters-mode))

(add-hook 'emacs-lisp-mode-hook 'setup-elisp)

(provide 'init-elisp)
