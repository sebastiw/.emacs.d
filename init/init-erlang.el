(add-hook 'erlang-mode-hook 'setup-erlang)

(defun setup-erlang ()
  (install-package 'erlang)

  (when (package-installed-p 'erlang)
    (setq erlang-root-dir "/opt/erlang/17.1/")
    (setq exec-path (cons "/opt/erlang/17.1/bin" exec-path))
    (add-to-list 'load-path "/opt/erlang/17.1/lib/erlang/lib/tools-2.6.15/emacs/")

    (require 'erlang-start))

  ;; EDTS-mode
  ;; Very powerful development toolkit for Erlang, a must have.
  ;; But does not work for Windows at the moment.
  ;; Check: https://github.com/tjarvstrand/edts

  (cond  ((string-equal system-type "windows-nt") ;; if windows
          (message "EDTS currently not supported in Windows."))

         ((not nil) ;; otherwise
          (install-package 'auto-highlight-symbol)
          (install-package 'edts)

          (require 'edts-start))))

  ;; Quviq QuickCheck
  ;; Automated testing using properties.
  ;; Check http://www.quviq.com
  ;; Commercial, this is why we don't auto-install it.
  ;; Just load it if its there.

  ;; (defvar eqc-root-dir "/usr/lib/erlang/lib/eqc-1.30.0"
  ;;   "Where EQC is installed.")
  ;; (defvar eqc-load-path "/usr/lib/erlang/lib/eqc-1.30.0/emacs/"
  ;;   "EQC's load path.")

  ;; (when (file-exists-p eqc-root-dir)
  ;;   (add-to-list 'load-path eqc-load-path)
  ;;   (autoload 'eqc-erlang-mode-hook "eqc-ext" "EQC Mode" t)
  ;;   (add-hook 'erlang-mode-hook 'eqc-erlang-mode-hook)
  ;;   (setq eqc-max-menu-length 30))


  ;; Settings

(setq erlang-indent-level 2
      edts-man-root "/opt/erlang/17.1/lib/erlang/man")

(provide 'init-erlang)
