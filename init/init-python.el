


(use-package python
  :ensure t
  :mode ("\\<SConstruct\\>$" . python-mode)
  :config
  (progn
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


;; (use-package elpy
;;   :pre-load (load-file "~/.emacs.d/elpa/python-20120402/python.el")
;;   :ensure t
;;   :mode ("\\.py\\'" . python-mode)
;;   :requires company find-file-in-project highlight-indentation
;;   idomenu pyvenv yasnippet
;;   :config
;;   (progn
;;     (elpy-mode)
;;     (define-key python-mode-map (kbd "C-c C-z") 'run-python)
;;     (define-key python-mode-map (kbd "<backtab>") 'python-back-indent)
;;     (use-package jedi
;;       :ensure t
;;       :config
;;       (progn
;;         (jedi:setup)
;;         (jedi:ac-setup)
;;         (setq jedi:setup-keys t)
;;         (setq jedi:complete-on-dot t)
;;         (define-key python-mode-map (kbd "C-c C-d") 'jedi:show-doc)
;;         (setq jedi:tooltip-method nil)
;;         (set-face-attribute 'jedi:highlight-function-argument nil
;;                             :foreground "green")
;;         (define-key python-mode-map (kbd "C-c C-l") 'jedi:get-in-function-call)
;; 	(add-hook 'python-mode-hook (lambda () (jedi-mode t)))))

;;     (setq python-indent-offset 2)
;;     ))


(provide 'init-python)
