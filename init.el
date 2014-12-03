;;(setq debug-on-error t)

;; Not everything really needs version 24, but I haven't figured out
;; which parts that needs it.
(let ((minver 24))
  (unless (>= emacs-major-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher"
           minver)))

;; Load paths
(add-to-list 'load-path (expand-file-name "init" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "other" user-emacs-directory))

;; Benchmark how fast/slow different files load
;; C-h v sanityinc/require-times
(require 'custom-benchmarking "benchmarking.el" nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-install Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar elpa-required-packages '(use-package)
  "General packages which need to be installed")

;; Need to be first among the first
(require 'init-packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Requires
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global Modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Better M-x
(use-package smex
  :ensure t
  :config
  (progn
    ;; IDO for M-x
    (smex-initialize)
    ))

;; I-do mode, easier minibuffer/file lookups
(use-package ido
  :ensure t
  :config
  (progn
    (ido-mode)
    (setq ido-everywhere t)

    ;; Will show all alternative files vertically
    (use-package ido-vertical-mode
      :ensure t
      :requires ido
      :config
      (progn
        (ido-vertical-mode)
        (setq ido-use-vertical-buffers t)))

    ;; IDO-mode everywhere possible
    (use-package ido-ubiquitous
      :ensure t
      :requires ido
      :config
      (ido-ubiquitous-mode))

    ;; Advices for ido-mode
    (use-package ido-hacks
      :ensure t
      :disabled t
      :requires ido
      :config
      (ido-hacks-mode))
    ))

;; Help buffers are only displayed temporarily and when needed.
(use-package popwin
  :ensure t
  :config
  (popwin-mode))

;; 80 chars line
(use-package fill-column-indicator
  :ensure t
  :config
  (progn
    ;; A line on indicate when 80 chars (or whatever) has been reached
    ;; fci-mode is not a global mode, so to be able to turn it on default:
    (define-globalized-minor-mode global-fci-mode fci-mode turn-on-fci-mode)
    (global-fci-mode 1)
    ))

;; Show tabs, whitespaces etc
(use-package highlight-chars
  :pre-load (load-file "~/.emacs.d/elpa/highlight-chars-20140513.444/highlight-chars.el")
  :ensure t
  :config
  (progn
    (hc-toggle-highlight-tabs t)
    (hc-toggle-highlight-trailing-whitespace t)
    ;; Highlight hard tabs
    ;; Destroys colors in eshell. Why?
    (add-hook 'eshell-mode-hook (lambda ()
                                  (if (and (boundp 'hc-highlight-tabs-p) hc-highlight-tabs-p)
                                      (hc-toggle-highlight-tabs))
                                  (if (and (boundp 'hc-highlight-tabs-p) hc-highlight-trailing-whitespace-p)
                                      (hc-toggle-highlight-trailing-whitespace))))
    ))

;; Parantheses have different colors
(use-package rainbow-delimiters
  :ensure t
  :config
  (progn
    ;; Rainbow delimiters in all programming modes
    ;; Emacs 24+ needed for prog-mode
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
    ))

;; Nyan Cat buffer percentage
(use-package nyan-mode
  :pre-load (load-file "~/.emacs.d/elpa/nyan-mode-20140801.1329/nyan-mode.el")
  :ensure t
  :config
  (nyan-mode))

;; Global auto-complete
(use-package auto-complete
  :ensure t
  :config
  (progn
    (auto-complete-mode)
    (setq ac-delay 0
          ac-use-fuzzy t
          ac-auto-start 2)))

;; Clearcase version control
;; because it was written a looooong time back (round 2004), directory-sep-char
;; needs to be set.
;; Also, this mode takes forever to load (about 4 seconds) so load on demand instead.
(defun clearcase-mode-on ()
  (interactive)
  (setq directory-sep-char ?/)
  (require 'clearcase))

;; Custom---------------------------
(require 'init-autocomplete)
(require 'init-c)
(require 'init-erlang)
(require 'init-elisp)
(require 'init-eshell)
(require 'init-gnus)
(require 'init-haskell)
(require 'init-java)
(require 'init-javascript)
(require 'init-latex)
(require 'init-org)
(require 'init-python)

(require 'custom-backups)
(require 'custom-functions)
(require 'custom-keybindings)


(use-package markdown-mode
  :mode "\\.md\\'"
  :config (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))
(use-package php-mode
  :ensure t
  :mode "\\.php5?\\'")
(use-package web-mode
  :ensure t
  :mode "\\.html?\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Remember last window settings
(winner-mode t)

;; Visualization for matching parenthesis
(show-paren-mode t)

; Column number in mode line
(column-number-mode t)

; Yasnippet minor mode in all buffers
;; (yas-global-mode 1)

; no toolbars/scrollbars
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode t)

; Turn on highlighting current line
(global-hl-line-mode 1)

;; Buffers have unique names
(require 'uniquify)
;; Gives buffers/files with similiar names unique names instead
(toggle-uniquify-buffer-names)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Instead of answering "YES" or "NO"
(fset 'yes-or-no-p 'y-or-n-p)

;; For fullscreen on start
(fullscreen)

;; global variables
(setq standard-indent 2
      doc-view-continuous t
      inhibit-startup-screen t
      find-file-wildcards t
      )

;; buffer-local variables
(setq-default indent-tabs-mode nil
              fill-column 80
              )

;; The default font/fontsize can differ much between computers, better to set it.
(when (member "DejaVu Sans Mono" (font-family-list))
  (set-face-attribute 'default t :height 100 :font "DejaVu Sans Mono"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(add-hook 'before-save-hook 'delete-trailing-whitespace)

;(add-hook 'fundamental-mode 'global-unset-mouse)
