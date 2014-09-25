;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar elpa-required-packages '()
  "Packages which need to be installed")

(defvar elpa-dir "elpa"
  "Which directory elpa packages should be installed in.")

(require 'package)
(setq package-archives '(("gnu"       . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa"     . "http://melpa.milkbox.net/packages/")
                         ("org"       . "http://orgmode.org/elpa/")))

(unless (file-exists-p  (concat user-emacs-directory elpa-dir))
  (message "The directory %s does not exist, creating it." elpa-dir)
  (package-refresh-contents))

(package-initialize)

(message "Checking if required packages is installed...")
(dolist (pkg elpa-required-packages)
  (unless (package-installed-p pkg)
    (message "Packet %s was not installed, installing..." pkg)
    (package-install pkg)))


(defun install-package (pkg)
  "If package PKG is not installed, install it."
  (unless (package-installed-p pkg)
    (progn
      (unless (assoc pkg package-archive-contents)
        (package-refresh-contents))
      (package-install pkg))))

;; Thank you Prelude
(defmacro prelude-auto-install (extension package mode)
  "When file with EXTENSION is opened triggers auto-install of PACKAGE.
PACKAGE is installed only if not already present. The file is opened in MODE."
  `(add-to-list 'auto-mode-alist
                `(,extension . (lambda ()
                                 (unless (package-installed-p ',package)
                                   (package-install ',package))
                                 (,mode)))))

;; No autoload so add manually
(when (package-installed-p 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))
(when (package-installed-p 'pkgbuild-mode)
  (add-to-list 'auto-mode-alist '("PKGBUILD\\'" . pkgbuild-mode)))

;; build auto-install mappings
(mapc
 (lambda (entry)
   (let ((extension (car entry))
         (package (cadr entry))
         (mode (cadr (cdr entry))))
     (unless (package-installed-p package)
       (prelude-auto-install extension package mode))))
 prelude-auto-install-alist)

(provide 'init-packages)
