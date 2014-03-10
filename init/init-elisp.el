
(add-hook 'emacs-lisp-mode-hook 'setup-elisp)

(defun setup-elisp ()
;;  (package-install 'elisp-slime-nav)
;;  (package-install 'rainbow-delimiters)
  (require 'elisp-slime-nav)
  (require 'rainbow-delimiters)

  (rainbow-delimiters-mode))

(provide 'init-elisp)
