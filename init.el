(setq debug-on-error t)

; Not everything really needs version 24, but I haven't figured out which parts that needs it.
(let ((minver 24))
  (unless (>= emacs-major-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

; Load paths
(add-to-list 'load-path (expand-file-name "init" user-emacs-directory))

; Benchmark how fast/slow different files load
(require 'custom-benchmarking "benchmarking.el" nil)

(defvar elpa-required-packages '(
                                 auto-complete
                                 ido-hacks
                                 ido-vertical-mode
                                 ido-ubiquitous
                                 smex
                                 yasnippet
                                 popwin
                                 fill-column-indicator
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
;; Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar seba-fontify-tabs t
  "When set to t, hard tabs will be highlighted.")

(defvar seba-fullscreen-mode t
  "When set to t, emacs will start in fullscreen.")

(defvar seba-erlang-enable-quickcheck nil
  "When set to t, QuickCheck will be enabled for Erlang.

Remember to also set `eqc-root-dir' and `eqc-load-path'")

;; Can we do something here to auto find the Erlang root dir?
;; Maybe something with $PATH, or possibly os command `whereis'
(defvar seba-erlang-root-dir "/opt/erlang/17.1/lib/erlang/"
  "The Erlang root directory")

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
(require 'popwin)

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

; Much nicer find-file
(ido-mode t)
; Will show all alternative files vertically
(ido-vertical-mode)
; IDO-mode everywhere possible
(ido-ubiquitous-mode)

; Remember last window settings
(winner-mode t)
; Visualization for matching parenthesis
(show-paren-mode t)
; Column number in mode line
(column-number-mode t)

; Yasnippet minor mode in all buffers
(yas-global-mode 1)
; Gives buffers/files with similiar names unique names instead
(toggle-uniquify-buffer-names)

; no toolbars/scrollbars
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode t)

; Help buffers are only displayed temporarily and when needed.
(popwin-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; IDO for M-x
(smex-initialize)

; Instead of answering "YES" or "NO"
(fset 'yes-or-no-p 'y-or-n-p)

; For fullscreen on start
(when seba-fullscreen-mode
  (fullscreen))

(setq ido-use-virtual-buffers t
      ido-everywhere t
      standard-indent 2
      doc-view-continuous t
      fill-column 80
      inhibit-startup-screen t
      find-file-wildcards t)

; Don't insert hard tabs
(setq-default indent-tabs-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Because I hate hard tabs, font-lock them.
(when seba-fontify-tabs
  (make-face 'tabs-face)
  (set-face-foreground 'tabs-face "LightGrey")

  (add-hook 'font-lock-mode-hook
            '(lambda ()
               (font-lock-add-keywords
                nil
                '(("\t" 0 '(:background "MistyRose") prepend))
                ))))
