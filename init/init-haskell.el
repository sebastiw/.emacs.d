(install-package 'haskell-mode)
(require 'haskell-mode-autoloads)

(defun setup-haskell ()
  (install-package 'hi2)

  (turn-on-haskell-indentation)
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-decl-scan)

  (setq haskell-compile-command "ghc -Wall -threaded -eventlog -rtsopts %s")

  (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-compile))

(add-hook 'haskell-mode-hook 'setup-haskell)

(provide 'init-haskell)
