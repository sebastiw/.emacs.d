
(defun setup-java ()
  (install-package 'malabar-mode)

  (require 'cedet)
  (require 'malabar-mode)

  (setq c-basic-offset 2
        tab-width 2)
  (semantic-load-enable-minimum-features) ;; or enable more if you wish
  (add-hook 'after-save-hook 'malabar-compile-file-silently nil t)

  (define-key 'malabar-mode-map (kbd "C-c C-k") 'malabar-compile-file))

(provide 'init-java)
