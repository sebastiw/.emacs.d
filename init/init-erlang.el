
(provide 'init-erlang)

(require 'erlang-start)

(eval-after-load 'erlang
  '(progn
     (let ((erootdir erlang-root-dir)
           (exe-find (executable-find "erl"))
           (shell-cmd-find (shell-command-to-string "which erl")))

     (if (or (equal erootdir "")
             (equal exe-find "")
             (equal shell-cmd-find ""))
         (error "Could not find erlang, set the variable `erlang-root-dir'")

       (if (equal erootdir "")
           (if (equal exe-find "")
               (setq erlang-root-dir shell-cmd-find)
             (setq erlang-root-dir exe-find)))))

     (setq exec-path (cons (expand-file-name "bin/" erlang-root-dir)
                           exec-path)
           edts-man-root (expand-file-name "man" erlang-root-dir)
           erlang-indent-level 4)

     ;; Add Erlangs Emacs directory to the load-path
     (add-to-list 'load-path (file-expand-wildcards
                              (concat erlang-root-dir
                                      "lib/tools-*/emacs")))

     ;; EDTS-mode
     ;; Very powerful development toolkit for Erlang, a must have.
     ;; But does not work for Windows at the moment.
     ;; Check: https://github.com/tjarvstrand/edts

     (cond  ((string-equal system-type "windows-nt") ;; if windows
             (message "EDTS currently not supported in Windows."))

            ((not nil) ;; otherwise
             (install-package 'auto-highlight-symbol)
             (install-package 'edts)

             (require 'edts-start)))

     ;; Daniel Mauritzson's (emmnddl) MME-TOOLS
     (add-to-list 'load-path (concat user-emacs-directory "other/mme-tools"))
     (require 'mme-tools)

     (edts-project-override "*"
                            '(:name "eps"
                              :node-sname "eps"
                              :start-command "erl -sname eps -pa do3/erlang -pa /vobs/gsn/product/code/business_specific/cos/do3/erlang"))

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

     (when (file-exists-p eqc-root-dir)
       (add-to-list 'load-path eqc-load-path)
       (autoload 'eqc-erlang-mode-hook "eqc-ext" "EQC Mode" t)
       (add-hook 'erlang-mode-hook 'eqc-erlang-mode-hook)
       (setq eqc-max-menu-length 30))
     ))
