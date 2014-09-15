(let ((minver 24))
  (unless (>= emacs-major-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

(add-to-list 'load-path (expand-file-name "init" user-emacs-directory))

(require 'custom-benchmarking "benchmarking.el" nil)

(defvar elpa-required-packages '(
                                 auto-complete
                                 ido-hacks
                                 ido-vertical-mode
                                 ido-ubiquitous
                                 smex
                                 yasnippet
                                 popwin
                                 )
  "General packages which need to be installed")

;; Need to be first among the first
(require 'init-packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Alist
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-mode-alist
      (append '(("\\.[eh]rl$" . erlang-mode)
                ("\\.yaws$"   . erlang-mode)
                ("\\.php[5]?$". web-mode)
                ("\\.htm[l]?$". web-mode)
                ("\\.java\\'" . setup-java)
                )
              auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Requires
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Modes----------------------------
(require 'smex)
(require 'yasnippet)
(require 'ido-vertical-mode)
(require 'ido-ubiquitous)
(require 'uniquify)

;; Custom---------------------------
(require 'init-autocomplete)
(require 'init-c)
(require 'init-erlang)
(require 'init-elisp)
(require 'init-gnus)
(require 'init-haskell)
(require 'init-java)
(require 'init-javascript)
(require 'init-latex)
(require 'init-org)

(require 'custom-backups)
(require 'custom-functions)
(require 'custom-keybindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ido-mode t)
(ido-vertical-mode)
(ido-ubiquitous)
(winner-mode t)
(show-paren-mode t)
(column-number-mode t)

(yas-global-mode 1)
(toggle-uniquify-buffer-names)

; no toolbars/scrollbars
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode t)

(require 'popwin)
(popwin-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(smex-initialize)
(fset 'yes-or-no-p 'y-or-n-p)

(fullscreen)

(setq ido-use-virtual-buffers t
      ido-everywhere t
      standard-indent 2
      doc-view-continuous t
      fill-column 80
      inhibit-startup-screen t
      find-file-wildcards t)

(setq-default indent-tabs-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(make-face 'tabs-face)
(set-face-foreground 'tabs-face "LightGrey")

(add-hook 'font-lock-mode-hook
          '(lambda ()
             (font-lock-add-keywords
              nil
              '(("\t" 0 '(:background "MistyRose") prepend))
              )))


(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 2
                                  tab-width 2)))
