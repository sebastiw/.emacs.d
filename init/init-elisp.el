(defmacro after-load (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))

(defun setup-elisp ()
  (install-package 'elisp-slime-nav)
  (install-package 'rainbow-delimiters)
  (require 'elisp-slime-nav)
  (require 'rainbow-delimiters)

  (elisp-slime-nav-mode)
  (rainbow-delimiters-mode))

(after-load 'emacs-lisp-mode
  (add-hook 'emacs-lisp-mode-hook 'setup-elisp))

(provide 'init-elisp)
