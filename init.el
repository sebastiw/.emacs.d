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

(defun sanityinc/time-subtract-millis (b a)
  (* 1000.0 (float-time (time-subtract b a))))

(defvar sanityinc/require-times nil
  "A list of (FEATURE . LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

(defadvice require
  (around build-require-times (feature &optional filename noerror) activate)
  "Note in `sanityinc/require-times' the time taken to require each feature."
  (let* ((already-loaded (memq feature features))
         (require-start-time (and (not already-loaded) (current-time))))
    (prog1
        ad-do-it
      (when (and (not already-loaded) (memq feature features))
        (add-to-list 'sanityinc/require-times
                     (cons feature
                           (sanityinc/time-subtract-millis (current-time)
                                                           require-start-time))
                     t)))))

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

(unless (file-exists-p  (concat user-emacs-directory elpa-dir))
  (message "No packages exists yet, refreshing archives.")
  (package-refresh-contents))

(package-initialize)

(defun ensure-pkg (&rest pkgs)
  "If package PKG is not installed, install it."
  (dolist (pkg pkgs)
    (unless (package-installed-p pkg)
      (progn
        (unless (assoc pkg package-archive-contents)
          (package-refresh-contents))
        (package-install pkg)))))

(setq-default bidi-paragraph-direction 'left-to-right)

(setq initial-scratch-message nil)

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

(ensure-pkg 'magit)
(require 'magit)

(define-key magit-mode-map (kbd "C-<tab>") 'other-window)

(global-set-key (kbd "C-c q") 'magit-status)

(ensure-pkg 'org)
(require 'org)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-agenda-files (list "~/ORG/"))

(setq calendar-week-start-day 1)

(setq org-special-ctrl-a/e t)

(setq org-adapt-indentation t)

(setq org-use-sub-superscripts '{})

(define-key org-mode-map (kbd "C-<tab>") 'other-window)

;(require 'eshell)

(defalias 'ec 'find-file)
(defalias 'emacs 'find-file)

(with-eval-after-load 'em-term
    (add-to-list 'eshell-visual-commands "ssh")
    (add-to-list 'eshell-visual-commands "htop")
    (add-to-list 'eshell-visual-commands "tail"))

(setq eshell-cmpl-cycle-completions nil)

(setq shell-prompt-pattern "^.*eselnts1349[^>]* *")

(defun clearcase-mode-on ()
  (interactive)
  (setq directory-sep-char ?/
        ;clearcase-checkin-arguments (quote ("-nc"))
        ;clearcase-checkout-arguments (quote ("-nc"))
        )
  (require 'clearcase))

(let ((bt-erl-path (getenv "BT_ERL_PATH")))
  (when (and bt-erl-path (file-exists-p bt-erl-path))
    (add-to-list 'load-path bt-erl-path)
    (require 'emacs_bt)))

(ensure-pkg 'erlang)
(require 'erlang-start)

(setq erlang-root-dir "/opt/erlang/19.1")

(add-to-list 'exec-path (concat erlang-root-dir "/bin"))

(ensure-pkg 'edts)
(require 'edts-start)

; (add-hook 'erlang-mode-hook '(lambda () (require 'edts-start)))

(setq edts-man-root "/opt/erlang/19.0/lib/erlang")



(ensure-pkg 'js-comint)
(require 'js-comint)

(setq inferior-js-program-command "node")

(with-eval-after-load 'js-mode
  (define-key js-mode-map (kbd "C-c C-k") 'seba/start-nodejs-in-background)
  (defun seba/start-nodejs-in-background ()
    (interactive)
    (message "Not Implemented Yet")))

(ensure-pkg 'scala-mode)

(ensure-pkg 'helm)
(require 'helm-config)

(helm-mode 1)

(helm-autoresize-mode t)

(setq helm-split-window-in-side-p t)

(setq helm-ff-auto-update-initial-value nil)

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

(global-set-key (kbd "C-x C-b")    'helm-buffers-list)
(global-set-key (kbd "M-x")        'helm-M-x)
(global-set-key (kbd "C-c h")      'helm-command-prefix)
(global-set-key (kbd "C-x C-f")    'helm-find-files)
(global-set-key (kbd "M-y")        'helm-show-kill-ring)
(global-set-key (kbd "M-s o")      'helm-occur)
(global-set-key (kbd "C-h SPC")    'helm-all-mark-rings)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
