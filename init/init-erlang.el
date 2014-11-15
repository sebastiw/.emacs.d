(provide 'init-erlang)

(use-package erlang-start
  :mode (("\\.[eh]rl\\'" . erlang-mode)
	 ("\\.yaws?\\'" . erlang-mode)
	 ("\\.escript?\\'" . erlang-mode))
  :config
  (progn
    ;; Find the erlang-root-dir automatically, either it is already set, or
    ;; elisp knows where it is, or `which' knows where.
    (let ((erootdir erlang-root-dir)
          (exe-find (directory-file-name (file-name-directory (executable-find "erl"))))
          (shell-cmd-find (directory-file-name (file-name-directory (shell-command-to-string "which erl")))))


      (if (and (equal erootdir nil)
               (equal exe-find "")
               (equal shell-cmd-find ""))
          (error "Could not find erlang, set the variable `erlang-root-dir'"))

      (if (equal erootdir nil)
          (if (equal exe-find "")
              (setq erlang-root-dir shell-cmd-find)
            (setq erlang-root-dir exe-find)))

      ;; Also set the manual directory and indent level
      (setq edts-man-root (expand-file-name ".." erlang-root-dir)
            erlang-indent-level 4)

      ;; Add Erlangs Emacs directory to the load-path
      (add-to-list 'load-path (file-expand-wildcards
                               (concat erlang-root-dir
                                       "lib/tools-*/emacs")))

      ;; EDTS-mode
      ;; Very powerful development toolkit for Erlang, a must have.
      ;; But does not work for Windows at the moment.
      ;; Check: https://github.com/tjarvstrand/edts
      ;;
      ;; Also to set up first run /home/esebwed/scripts/edts_create_config.sh
      ;; when in a 'clearcase view'
      (use-package edts
        ;; EDTS currently not supported in Windows.
        :if (not (string-equal system-type "windows-nt"))
        :config (require 'edts-start))

      ;; Daniel Mauritzson's (emmnddl) MME-TOOLS
      (add-to-list 'load-path (concat user-emacs-directory "other/mme-tools"))
      (require 'mme-tools)

      ;; Quviq QuickCheck
      ;; Automated testing using properties.
      ;; Check http://www.quviq.com
      ;; Commercial, this is why we don't auto-install it.
      ;; Just load it if its there.
      (defvar eqc-root-dir (expand-file-name "lib/eqc-1.30.0"
                                             erlang-root-dir)
        "Where EQC is installed.")
      (defvar eqc-load-path (expand-file-name "lib/eqc-1.30.0/emacs/"
                                              erlang-root-dir)
        "EQC's load path.")

      (use-package eqc-erlang-mode
        :if (file-exists-p eqc-root-dir)
        :init (progn
                (add-to-list 'load-path eqc-load-path)
                (autoload 'eqc-erlang-mode-hook "eqc-ext" "EQC Mode" t)
                (add-hook 'erlang-mode-hook 'eqc-erlang-mode-hook)
                (setq eqc-max-menu-length 30)
                )))))
