
(provide 'init-haskell)

(eval-after-load 'haskell-mode
  (install-package 'hi2)
  (require 'haskell-mode-autoloads)

  (turn-on-haskell-indentation)
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-decl-scan)

  (setq haskell-compile-command "ghc -Wall -threaded -eventlog -rtsopts %s")

  (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-compile))
