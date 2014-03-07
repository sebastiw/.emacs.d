
(add-hook 'LaTeX-mode-hook 'setup-latex)
(add-hook 'latex-mode-hook 'setup-latex)

(defun setup-latex ()
  (require 'latex)
  (require 'auctex-latexmk)
  (require 'ispell)
  (require 'auto-complete)

  (visual-line-mode t)
  (flyspell-mode t)
  (auto-fill-mode t)
  (auto-complete-mode)
  (ac-flyspell-workaround)
  (writegood-mode)

  (make-local-variable 'ispell-parser)
  (setq ispell-parser 'tex)

  (define-key TeX-mode-map (kbd "\C-c i") 'insert-latex)
  (define-key TeX-mode-map (kbd "\C-c \C-c") 'TeX-comment-or-uncomment-region)
  (define-key TeX-mode-map (kbd "\C-c \C-k") 'TeX-command-master)

  (lambda () (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|BUG\\)" 1 font-lock-warning-face t))))

  (auctex-latexmk-setup)

  (LaTeX-math-mode)

  (setq ac-auto-show-menu 0.01
        ac-auto-start 1
        ac-delay 0.01
        LaTeX-command "latex"

        TeX-PDF-mode t
        TeX-source-correlate-method 'synctex
        TeX-source-correlate-mode t
        TeX-source-correlate-start-server t
        TeX-clean-confirm nil
        TeX-view-predicate-list '((output-pdf (string-match "pdf" (TeX-output-extension))))
        TeX-view-program-list
        '(("Default"
           (lambda () (interactive) (progn (TeX-clean) (find-file-other-window "%o")))))
        ;;           (lambda () (interactive) (view-doc-in-emacs "%o" (ido-get-work-directory)))))
        ;; ("Okular" ("okular --unique %o#src:%n%b"))
        TeX-view-program-selection '((output-pdf "Default"))))

(provide 'init-latex)
