
(provide 'init-haskell)

(use-package haskell
  :ensure t
  :bind ("C-c C-k" . haskell-compile)
  :mode "\\.hs\\'"
  :config
  (progn
    (use-package hi2
      :ensure t
      :config
      (hi2-mode))
    (require 'haskell-mode-autoloads)

    (turn-on-haskell-indentation)
    (turn-on-haskell-doc-mode)
    (turn-on-haskell-decl-scan)

    (setq haskell-compile-command "ghc -Wall -threaded -eventlog -rtsopts %s")))
