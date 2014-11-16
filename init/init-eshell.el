
(when (cl-equalp "DUMB" (getenv "TERM"))
  (setenv "PAGER" "cat"))

(use-package eshell
  :config
  (progn
    (if hc-highlight-tabs-p
        (hc-toggle-highlight-tabs))
    (if hc-highlight-trailing-whitespace-p
        (hc-toggle-highlight-trailing-whitespace))
    (unload-feature 'highlight-chars)

    (defalias 'emacs 'find-file)
    (defalias 'ec 'find-file)
    (setenv "TERM" "xterm-256color")
    (setenv "PAGER" "cat")

    (use-package esh-opt
      :config
      (progn
        (use-package em-cmpl)
        (use-package em-prompt)
        (use-package em-term)

        ;; Taken from Phil's config
        (setq eshell-cmpl-cycle-completions nil
              eshell-history-size 20480
              eshell-buffer-shorthand t)
        (add-hook 'eshell-mode-hook (defun pnh-eshell-term-dumb ()
                                      (eshell/export "TERM" "dumb")))

        (when (not (functionp 'eshell/rgrep))
          (defun eshell/rgrep (&rest args)
            "Use Emacs grep facility instead of calling external grep."
            (eshell-grep "rgrep" args t)))

        (defun eshell/cds ()
          "Change directory to the project's root."
          (eshell/cd (locate-dominating-file default-directory ".git")))

        (defun eshell/l () "Same as `ls -lh'" (eshell/ls "-lh"))
        (defun eshell/ll () "Same as `ls -lh'" (eshell/ls "-lh"))
        (defun eshell/la () "Same as `ls -alh'" (eshell/ls "-alh"))

        (defun eshell/clear ()
          "Clear the eshell buffer"
          (interactive)
          (let ((eshell-buffer-maximum-lines 0))
            (eshell-truncate-buffer)))

        (defun eshell/export-env (&optional env-file)
          (interactive)
          (let ((original-buffer (current-buffer)))
            (with-temp-buffer
              (insert-file (or env-file ".env"))
              (goto-char (point-min))
              (while (< (point) (point-max))
                (let ((line (substring (thing-at-point 'line) 0 -1)))
                  (with-current-buffer original-buffer
                    (eshell/export line)))
                (next-line)))))

        (eval-after-load 'em-term
          '(add-to-list 'eshell-visual-commands "ssh"))

        (defalias 'eshell/ee 'eshell/export-env)))

    (add-hook 'eshell-mode-hook
              (lambda ()
                (define-key eshell-mode-map (kbd "M-l")
                  'helm-eshell-history)))

    ;; See eshell-prompt-function below
    (setq eshell-prompt-regexp "^[^#$\n]* [#$] ")
    (add-to-list 'eshell-visual-commands "el")
    (add-to-list 'eshell-visual-commands "elinks")
    (add-to-list 'eshell-visual-commands "htop")

    ;; So the history vars are defined
    (require 'em-hist)
    (if (boundp 'eshell-save-history-on-exit)
        ;; Don't ask, just save
        (setq eshell-save-history-on-exit t))
    ;;(message "eshell-ask-to-save-history is %s" eshell-ask-to-save-history)
    (if (boundp 'eshell-ask-to-save-history)
        ;; For older(?) version
        (setq eshell-ask-to-save-history 'always))
    ;;(message "eshell-ask-to-save-history is %s" eshell-ask-to-save-history)

    (defun eshell/ef (fname-regexp &rest dir) (ef fname-regexp default-directory))


;;; ---- path manipulation

    ;; See: https://github.com/kaihaosw/eshell-prompt-extras
    (use-package eshell-prompt-extras
      :ensure t
      :pre-load (load-file "elpa/eshell-prompt-extras-20141110.151/eshell-prompt-extras.el")
      :init
      (progn
        (setq eshell-highlight-prompt nil
              ;; epe-git-dirty-char "Ϟ"
              epe-git-dirty-char "*"
              eshell-prompt-function 'epe-theme-dakrone)))

    (setq eshell-highlight-prompt nil)

    (defun eshell/magit ()
      "Function to open magit-status for the current directory"
      (interactive)
      (magit-status default-directory)
      nil)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (local-set-key
             (kbd "C-c h")
             (lambda ()
               (interactive)
               (insert
                (ido-completing-read
                 "Eshell history: "
                 (delete-dups
                  (ring-elements eshell-history-ring))))))
            (local-set-key (kbd "C-c C-h") 'eshell-list-history)))

(provide 'init-eshell)
