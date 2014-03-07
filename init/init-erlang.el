(add-hook 'erlang-mode-hook 'setup-erlang)

(defun setup-erlang ()
  (require 'erlang)

  (when (package-installed-p 'erlang)
    (setq erlang-root-dir "/usr/lib/erlang")
    (setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
    (add-to-list 'load-path "/usr/lib/erlang/lib/tools-2.6.6.5/emacs/")

    (require 'erlang-start))


  (when (package-installed-p 'edts)
    ;; git clone https://github.com/tjarvstrand/edts
    ;; EDTS mode
    (autoload 'erlang-mode "erlang.el" "" t)
    (add-to-list 'load-path "~/.emacs.d/edts")
    (require 'edts-start))

  (when (package-installed-p 'eqc)
    ;; EQC Emacs Mode -- Configuration Start
    (add-to-list 'load-path "/usr/lib/erlang/lib/eqc-1.30.0/emacs/")
    (autoload 'eqc-erlang-mode-hook "eqc-ext" "EQC Mode" t)
    (add-hook 'erlang-mode-hook 'eqc-erlang-mode-hook)
    (setq eqc-max-menu-length 30)
    (setq eqc-root-dir "/usr/lib/erlang/lib/eqc-1.30.0")
    ;; EQC Emacs Mode -- Configuration End
    )

  ;; Settings
  (setq erlang-indent-level 2
        edts-man-root "~/.emacs.d/edts/doc/R16B03"))

(provide 'init-erlang)
