(let ((minver 24))
  (unless (>= emacs-major-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

(add-to-list 'load-path (expand-file-name "init" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar elpa-required-packages '(
				 json-mode
				 js2-mode
				 ac-js2
				 coffee-mode
				 js-comint
				 rainbow-delimiters

                                 )
  "Packages which need to be installed")

(defvar elpa-dir "elpa"
  "Which directory elpa packages should be installed in")

(require 'package)
(setq package-archives '(("gnu"       . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa"     . "http://melpa.milkbox.net/packages/")))

(unless (file-exists-p elpa-dir)
  (package-refresh-contents))

(package-initialize)

(dolist (pkg elpa-required-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Alist
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-mode-alist
      (append '(("\\.[eh]rl$" . erlang-mode)
                ("\\.yaws$"   . erlang-mode)
                ("\\.php[5]?$". web-mode)
                ("\\.htm[l]?$". web-mode))
              auto-mode-alist))


(require 'init-erlang)
(require 'init-javascript)
(require 'init-org)
(require 'init-latex)

(require 'init-c)
(require 'init-autocomplete)

(require 'custom-functions)
(require 'custom-keybindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yasnippet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'yasnippet)
(yas-global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs IRC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; '(rcirc-server-alist (quote (("dtek.se" :nick "visnae" :channels ("#dtek") nil nil))))

(eval-after-load "erc"
  '(progn
     (setq erc-nick "visnae"
           erc-autojoin-channels-alist '(("dtek.se" "#dtek"))
           erc-interpret-mirc-color t
           erc-button-buttonize-nicks nil)
     (erc-scrolltobottom-enable)
     (erc-spelling-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'java-mode-hook (lambda ()
			    (setq c-basic-offset 2
				  tab-width 2
				  indent-tabs-mode t)))

;; (require 'no-easy-keys)
;; (no-easy-keys 1)

(ido-mode t)
(setq ido-use-virtual-buffers t)

(winner-mode t)
(show-paren-mode t)
(column-number-mode t)

;; (helm-mode)

(require 'elisp-slime-nav)
(add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t)))

(require 'smex)

; no toolbars/scrollbars
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backup files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs-saves")) ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gnus | Mail
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Change header
(autoload 'gnus-alias-determine-identity "gnus-alias" "" t)
(add-hook 'message-setup-hook 'gnus-alias-determine-identity)

;; (setq gnus-alias-identity-alist '(())
;;       gnus-alias-identity-rules
;;       gnus-alias-default-identity
;;       gnus-alias-allow-forward-as-reply)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fset 'yes-or-no-p 'y-or-n-p)

(fullscreen)

(setq standard-indent 2
      fill-column 80)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doc-view-continuous t)
 '(fill-column 80)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
