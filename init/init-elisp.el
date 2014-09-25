
(provide 'init-elisp)

(eval-after-load 'elisp-mode
  (install-package 'elisp-slime-nav)
  (install-package 'rainbow-delimiters)
  (install-package 'paredit)

  (require 'elisp-slime-nav)
  (require 'rainbow-delimiters)
  (require 'paredit))
