
(defun setup-java ()
  (install-package 'malabar-mode)
  (require 'cedet)
  (semantic-load-enable-minimum-features) ;; or enable more if you wish
  (require 'malabar-mode)
  (add-hook 'after-save-hook 'malabar-compile-file-silently nil t)

  (define-key 'malabar-mode-map (kbd "C-c C-k") 'malabar-compile-file))

(provide 'init-java)
