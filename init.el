;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  NOTICE NOTICE NOTICE NOTICE NOTICE NOTICE NOTICE NOTICE NOTICE NOTICE   ;;
;;                                                                          ;;
;;                 You are looking in the wrong file...                     ;;
;;          Go to the init.org file instead, where you will find            ;;
;;               an explanation to all parameters as well.                  ;;
;;                                                                          ;;
;;                             Good luck!                                   ;;
;;                                                                          ;;
;;  NOTICE NOTICE NOTICE NOTICE NOTICE NOTICE NOTICE NOTICE NOTICE NOTICE   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((minver 24)
      (recver 25))
  (if (< emacs-major-version minver)
      (error "Your Emacs is too old -- this config requires v%s or higher"
             minver)
    (when (< emacs-major-version recver)
      (format-message
       (concat "You are probably fine with Emacs v%s for this init-file, but "
               "I cannot guarantee it. Recommended version of Emacs is v%s")
       minver recver))))

(let ((recos "gnu/linux"))
  (when (not (string-equal system-type recos))
    (format-message
     (concat "You might be fine with %s as OS for this init-file, but I "
             "cannot guarantee it. Recommended OS is %s")
     system-type recos)))

(setq-default gc-cons-threshold 10000000) ;; 10mB

(add-to-list 'load-path (expand-file-name "other" user-emacs-directory))

(setq gnutls-log-level 0)

(setq tls-checktrust 'ask)

(let ((trustfile
       (replace-regexp-in-string
        "\\\\" "/"
        (replace-regexp-in-string
         "\n" ""
         (shell-command-to-string "python -m certifi")))))
  ;; (setq tls-program
  ;;       (list
  ;;        (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
  ;;                (if (eq window-system 'w32) ".exe" "") trustfile)))
  (setq gnutls-verify-error nil)
  (setq gnutls-trustfiles (list trustfile))
  (add-to-list 'gnutls-trustfiles "/etc/ssl/certs/ca-certificates.crt")
  (add-to-list 'gnutls-trustfiles "/etc/pki/tls/certs/ca-bundle.crt")
  (add-to-list 'gnutls-trustfiles "/etc/ssl/ca-bundle.pem")
  (add-to-list 'gnutls-trustfiles "/usr/ssl/certs/ca-bundle.crt")
  (add-to-list 'gnutls-trustfiles "/usr/local/share/certs/ca-root-nss.crt"))

(defconst elpa-dir "elpa"
  "Which directory elpa packages is installed in. Defined in package.el.")

(require 'package)
(setq package-archives
      '(
        ("gnu"       . "https://elpa.gnu.org/packages/")
        ("melpa"     . "https://melpa.org/packages/")
        ("marmalade" . "https://marmalade-repo.org/packages/")
        ("elpy"      . "https://jorgenschaefer.github.io/packages/")
        ))

(unless (file-exists-p (concat user-emacs-directory elpa-dir))
  (message "No packages exists yet, refreshing archives.")
  (package-refresh-contents))

(package-initialize)

(unless (package-installed-p 'use-package)
  (progn
    (unless (assoc 'use-package package-archive-contents)
      (package-refresh-contents))
    (package-install 'use-package)))

(setq use-package-compute-statistics t)

(set-face-attribute 'default nil :font "Noto Sans Mono" :height 80)

(defun toggle-dark ()
  (interactive)
  (if (bound-and-true-p toggle-dark-flag)
      (progn
        (disable-theme 'wheatgrass)
        (setq toggle-dark-flag nil))
    (load-theme 'wheatgrass t t)
    (enable-theme 'wheatgrass)
    (setq toggle-dark-flag t)))

(toggle-dark)

(setq-default bidi-paragraph-direction 'left-to-right)

(setq initial-scratch-message nil)

(setq inhibit-startup-screen t)

(global-set-key (kbd "C-c e") 'open-dot-emacs)
(defun open-dot-emacs ()
  (interactive)
  (let ((user-init-file-org (concat (file-name-directory user-init-file)
                                    (file-name-base user-init-file)
                                    ".org")))
    (if (file-exists-p user-init-file-org)
        (find-file user-init-file-org)
      (find-file user-init-file))))

(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "<C-S-iso-lefttab>") 'select-previous-window)
(defun select-previous-window ()
  (interactive)
  (select-window (previous-window)))

(global-set-key (kbd "<f11>") 'fullscreen)
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

(defun untabify-buffer ()
  "Untabify current buffer."
  (interactive)
  (save-excursion (untabify (point-min) (point-max))))

(setq-default indent-tabs-mode nil)

(setq whitespace-style '(face trailing tabs lines-tail))
(global-whitespace-mode 1)

(global-set-key (kbd "C-z") 'eof)

(defun global-unset-mouse ()
  "Unset all mouse events"
  (interactive)
  (dolist (k '([mouse-1] [down-mouse-1] [drag-mouse-1]
               [double-mouse-1] [triple-mouse-1]
               [mouse-2] [down-mouse-2] [drag-mouse-2]
               [double-mouse-2] [triple-mouse-2]
               [mouse-3] [down-mouse-3] [drag-mouse-3]
               [double-mouse-3] [triple-mouse-3]
               [mouse-4] [down-mouse-4] [drag-mouse-4]
               [double-mouse-4] [triple-mouse-4]
               [mouse-5] [down-mouse-5] [drag-mouse-5]
               [double-mouse-5] [triple-mouse-5]
               [C-mouse-5] [S-mouse-5] [C-mouse-4] [S-mouse-4]
               [C-down-mouse-1] [C-down-mouse-3]))
   (global-unset-key k)))

(defun create-non-existent-directory ()
  "If The parent directory does not exist, this function will ask to create it."
  (let ((parent-directory (file-name-directory buffer-file-name))
        (q "Directory `%s' does not exist! Create it?"))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p
                (format q parent-directory)))
      (make-directory parent-directory t))))

(winner-mode 1)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(require 'uniquify)

(setq uniquify-buffer-name-style 'post-forward
      uniquify-strip-common-suffix t)

(fset 'yes-or-no-p 'y-or-n-p)

(column-number-mode 1)

(setq
   backup-by-copying t             ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs.d/.saves")) ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)              ; use versioned backups

(setq system-time-locale "C")

(use-package magit
  :init
  (require 'magit)
  :config
  (setq magit-log-section-commit-count 0)
  :bind
  (("C-c q" . magit-status)
   :map magit-mode-map
   ("C-<tab>" . other-window))
  :ensure t)

(use-package org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (C . t)
     (python . t)
     (plantuml . t))))

(use-package ox-gfm
  :ensure t
  :after org)
(use-package ob-gnuplot
  :after org)
(use-package ob-lisp
  :after org)
(use-package ob-org
  :after org)
(use-package ob-calc
  :after org)
(use-package ob-js
  :after org)
(use-package ob-latex
  :after org)
(use-package ob-plantuml
  :after org)
(use-package ob-ditaa
  :after org)
(use-package ob-awk
  :after org)
(use-package ob-python
  :after org)
(use-package ob-dot
  :after org)
(use-package ob-R
  :after org)
(use-package ob-shell
  :after org)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-agenda-files (list "~/ORG/"))

(setq calendar-week-start-day 1)

(setq org-special-ctrl-a/e t)

(setq org-adapt-indentation t)

(setq org-use-sub-superscripts '{})

(setq org-inline-image-overlays t)

(define-key org-mode-map (kbd "C-<tab>") 'other-window)

(use-package eshell
  :config
  (progn
    (setenv "TERM" "xterm-256color")
    (setenv "PAGER" "cat")))

(use-package esh-opt
  :after eshell)

(defalias 'ec 'find-file)
(defalias 'emacs 'find-file)

(with-eval-after-load 'em-term
    (add-to-list 'eshell-visual-commands "ssh")
    (add-to-list 'eshell-visual-commands "htop")
    (add-to-list 'eshell-visual-commands "tail"))

(setq eshell-cmpl-cycle-completions nil)

(use-package erlang
  :init (require 'erlang-start)
  :config
  (setq erlang-root-dir "~/.kerl/builds/25.0.4/release_25.0.4/")
  (add-to-list 'exec-path (concat erlang-root-dir "/bin"))
  :ensure t)

(use-package edts
  ;; :load-path "~/git/edts/"
  :init
  (setq edts-inhibit-package-check t)
  (add-hook 'erlang-mode-hook '(lambda () (require 'edts-start)))
  (add-to-list 'load-path "~/git/edts/elisp/f/")
  :config
  (setq edts-man-root erlang-root-dir)
  (setq edts-log-level 'debug)
  :after erlang
  :ensure t)

(add-to-list 'treesit-language-source-alist (cons 'erlang '("https://github.com/WhatsApp/tree-sitter-erlang")))
(if (not (treesit-language-available-p 'erlang))
    (treesit-install-language-grammar 'erlang))
; (add-to-list 'major-mode-remap-alist '(erlang-mode . erlang-ts-mode))

(use-package alchemist
  :init
  (require 'alchemist)
  :ensure t)

(use-package asn1-mode
  :ensure t)

(add-to-list 'treesit-language-source-alist (cons 'bash '("https://github.com/tree-sitter/tree-sitter-bash")))
(if (not (treesit-language-available-p 'bash))
    (treesit-install-language-grammar 'bash))
(add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))

(use-package elisp-slime-nav
  :init
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'turn-on-elisp-slime-nav-mode))
  :ensure t)

(add-to-list 'treesit-language-source-alist (cons 'elisp '("https://github.com/Wilfred/tree-sitter-elisp")))
(if (not (treesit-language-available-p 'elisp))
    (treesit-install-language-grammar 'elisp))

(use-package protobuf-ts-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist (cons "\\.proto$" 'protobuf-ts-mode)))

(use-package cobol-mode
  :mode (("\\.cob\\'" . cobol-mode)
         ("\\.cbl\\'" . cobol-mode)
         ("\\.cpy\\'" . cobol-mode)))

(use-package js-comint
  :disabled
  :init
  (require 'js-comint)
  :config
  (setq inferior-js-program-command "node")
  (setq js-indent-level 2))

(use-package typescript-mode
  :init
  (add-to-list 'auto-mode-alist (cons "\\.tsx$" 'typescript-mode))
  :config
  (setq typescript-indent-level 2))

(use-package scala-mode
  :disabled
  )

(use-package clojure-mode
  :init
  (add-to-list 'auto-mode-alist (cons "\\.clj$" 'clojure-mode))
  :disabled
  )
(use-package clojure-mode-extra-font-locking
  :after 'clojure-mode
  )
(use-package cider
  :after 'clojure-mode
  :config
  ;; Do not try to start it if it is already started.
  (condition-case nil
      (cider-ping)
    (error
     (require 'cider)
     (cider-jack-in))))

(use-package jedi
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(add-to-list 'treesit-language-source-alist (cons 'dockerfile '("https://github.com/camdencheek/tree-sitter-dockerfile")))
(if (not (treesit-language-available-p 'dockerfile))
    (treesit-install-language-grammar 'dockerfile))
(add-to-list 'major-mode-remap-alist '(dockerfile-mode . dockerfile-ts-mode))

(use-package dart-mode
  :ensure t
  :hook (dart-mode . flutter-test-mode))

(use-package flutter
  :after dart-mode
  :ensure t
  :bind (:map dart-mode-map
              ("C-M-x" . #'flutter-run-or-hot-reload)))

(use-package lsp-mode
  :ensure t)

(use-package lsp-dart
  :ensure t
  :hook (dart-mode . lsp))

;; Optional packages
(use-package lsp-ui
  :ensure t)
(use-package company
  :ensure t)

(use-package web-mode
  :ensure t
  :mode (("\\.html?" . web-mode)
         ("\\.php" . web-mode)
         ("\\.jsx" . web-mode)
         ("\\.tsx" . web-mode)
         ("\\.vue" . web-mode)
         ("\\.jsp" . web-mode)))

(add-to-list 'treesit-language-source-alist (cons 'json '("https://github.com/tree-sitter/tree-sitter-json")))
(if (not (treesit-language-available-p 'json))
    (treesit-install-language-grammar 'json))
(add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode))

(when (featurep 'treesit)
  (add-to-list 'treesit-extra-load-path "~/src/sources/tree-sitter-module/dist"))

(use-package helm
  :disabled

  :init
  (require 'helm-config)
  (helm-mode 1)
  :config
  (helm-autoresize-mode t)
  (setq helm-split-window-in-side-p t)
  (setq helm-ff-auto-update-initial-value nil)

  ;; fuzzy matching
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-recentf-fuzzy-match t)
  (setq helm-locate-fuzzy-match t)
  (setq helm-M-x-fuzzy-match t)
  (setq helm-semantic-fuzzy-match t)
  (setq helm-imenu-fuzzy-match t)
  (setq helm-apropos-fuzzy-match t)
  (setq helm-lisp-fuzzy-completion t)

  :bind
  (("C-x C-b" . helm-buffers-list)
   ("M-x" .     helm-M-x)
   ("C-c h" .   helm-command-prefix)
   ("C-x C-f" . helm-find-files)
   ("M-y" .     helm-show-kill-ring)
   ("M-s o" .   helm-occur)
   ("C-h SPC" . helm-all-mark-rings)
   :map helm-map
   ("<tab>" .   helm-execute-persistent-action))
  )

(add-to-list 'treesit-language-source-alist (cons 'markdown '("https://github.com/ikatyang/tree-sitter-markdown")))
(if (not (treesit-language-available-p 'markdown))
    (treesit-install-language-grammar 'markdown))

(add-to-list 'treesit-language-source-alist (cons 'yaml '("https://github.com/ikatyang/tree-sitter-yaml")))
(if (not (treesit-language-available-p 'yaml))
    (treesit-install-language-grammar 'yaml))

(use-package pcap-mode
  :ensure t
  :defer t)

(use-package hexl
  :mode ("\\.pcap\\'" . hexl-mode)
  :ensure t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t)

(add-hook 'prog-mode-hook 'copilot-mode)
(add-hook 'yaml-mode-hook 'copilot-mode)

(with-eval-after-load 'company
  ;; disable inline previews
  (delq 'company-preview-if-just-one-frontend company-frontends))

(define-key copilot-completion-map (kbd "C-.") 'copilot-accept-completion)
