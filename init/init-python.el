


(use-package python
  :ensure t
  :mode ("\\<SConstruct\\>$" . python-mode)
  :config
  (progn
    ;; disable auto-complete-mode (we use company-mode)
    (auto-complete-mode -1)
    (define-key python-mode-map (kbd "C-c C-z") 'run-python)
    (define-key python-mode-map (kbd "<backtab>") 'python-back-indent)
    (use-package elpy
      :ensure t
      :config
      (progn
        (elpy-enable)

        ;; pip install jedi
        (use-package jedi
          :ensure t
          :disabled t
          :config
          (progn
            (jedi:setup)
            (jedi:ac-setup)
            (setq jedi:setup-keys t
                  jedi:complete-on-dot t
                  jedi:tooltip-method nil)
            (set-face-attribute 'jedi:highlight-function-argument nil
                                :foreground "green")

            (define-key python-mode-map (kbd "C-c C-d") 'jedi:show-doc)
            (define-key python-mode-map (kbd "C-c C-l") 'jedi:get-in-function-call)
            (add-hook 'python-mode-hook (lambda () (jedi-mode t)))))
        (setq python-indent-offset 2)))
    (setq python-indent-offset 2)))



;;; From https://github.com/fxfactorial/emacsd/blob/master/init.el
;;; Python Stuff
;;; Get these variables set before the inferior mode comes up, otherwise too late.
;;; Might as well just use this VM

;; (setq python-shell-interpreter "/usr/local/bin/ipython3"
;;       python-shell-interpreter-args "--matplotlib=osx --colors=Linux"
;;       python-shell-prompt-regexp "In \\[[0-9]+\\]: "
;;       python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
;;       python-shell-completion-setup-code
;;       "from IPython.core.completerlib import module_completion"
;;       python-shell-completion-module-string-code
;;       "';'.join(module_completion('''%s'''))\n"
;;       python-shell-completion-string-code
;;       "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
;; (add-hook 'inferior-python-mode-hook (lambda ()
;;                                        (set-process-query-on-exit-flag
;;                                         ;; Just like killing the shell without asking me.
;;                                         (get-process "Python") nil)))
;; (add-hook 'python-mode-hook (lambda ()
;;                               (electric-pair-mode nil)
;;                               (hs-minor-mode)
;;                               (define-key hs-minor-mode-map (kbd "C-c C-t") 'hs-toggle-hiding)
;;                               (define-key python-mode-map (kbd "M-q") 'python-fill-paren)
;;                               (jedi:setup)
;;                               (setq jedi:setup-keys t
;;                                     jedi:server-args '("--sys-path"
;;                                                        "/usr/local/Cellar/python3/3.4.2_1/Frameworks/Python.framework/Versions/3.4/lib/python3.4/site-packages")
;;                                     jedi:complete-on-dot t)
;;                               ;; keeping a consistent interface for autocomplete type things.
;;                               (define-key python-mode-map (kbd "M-/") 'jedi:complete)
;;                               (let ((interpreter python-shell-interpreter)
;;                                     (args python-shell-interpreter-args))
;;                                 (when python-shell--parent-buffer
;;                                   (python-util-clone-local-variables python-shell--parent-buffer))
;;                                 ;; Users can override default values for these vars when calling
;;                                 ;; `run-python'. This ensures new values let-bound in
;;                                 ;; `python-shell-make-comint' are locally set.
;;                                 (set (make-local-variable 'python-shell-interpreter) interpreter)
;;                                 (set (make-local-variable 'python-shell-interpreter-args) args))
;;                               (flycheck-mode)
;;                               (setq-local show-trailing-whitespace t)))


(provide 'init-python)
