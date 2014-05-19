(add-hook 'erlang-mode-hook 'setup-erlang)

(defun setup-erlang ()
  (install-package 'erlang)

  (when (package-installed-p 'erlang)
    (setq erlang-root-dir "/usr/lib/erlang")
    (setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
    (add-to-list 'load-path "/usr/lib/erlang/lib/tools-2.6.6.5/emacs/")

    (require 'erlang-start))

  ;; EDTS-mode
  ;; Very powerful development toolkit for Erlang, a must have.
  ;; But does not work for Windows at the moment.
  ;; Check: https://github.com/tjarvstrand/edts

  (cond  ((string-equal system-type "windows-nt") ;; if windows
          (message "EDTS currently not supported in Windows."))
         ((not nil) ;; otherwise
          (defvar edts-directory (concat emacs-dir "/" "edts")
            "Where EDTS is installed. If the filename-string can't be found, run ``make edts'' from emacs-dir.")

          (unless (file-exists-p edts-directory)
            (compile (concat "make -C " emacs-dir " edts")))

          (when (file-exists-p edts-directory)
            (autoload 'erlang-mode "erlang.el" "" t)
            (add-to-list 'load-path "~/.emacs.d/edts")
            (require 'edts-start))))

  ;; Quviq QuickCheck
  ;; Automated testing using properties.
  ;; Check http://www.quviq.com
  ;; Commercial, this is why we don't auto-install it.
  ;; Just load it if its there.

  (defvar eqc-root-dir "/usr/lib/erlang/lib/eqc-1.30.0"
    "Where EQC is installed.")
  (defvar eqc-load-path "/usr/lib/erlang/lib/eqc-1.30.0/emacs/"
    "EQC's load path.")

  (when (file-exists-p eqc-root-dir)
    (add-to-list 'load-path eqc-load-path)
    (autoload 'eqc-erlang-mode-hook "eqc-ext" "EQC Mode" t)
    (add-hook 'erlang-mode-hook 'eqc-erlang-mode-hook)
    (setq eqc-max-menu-length 30))


  ;; Settings
)
(setq erlang-indent-level 2
      edts-man-root "~/.emacs.d/edts/doc/R16B03")

(provide 'init-erlang)
