(install-package 'elisp-slime-nav)
(require 'elisp-slime-nav)

(defun setup-elisp ()
  (install-package 'rainbow-delimiters)
  (require 'rainbow-delimiters)
  (rainbow-delimiters-mode t))

(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
(add-hook 'emacs-lisp-mode-hook 'setup-elisp)

(provide 'init-elisp)
