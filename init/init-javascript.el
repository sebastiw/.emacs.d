
(add-hook 'js-mode-hook 'setup-javascript)

;; js2 could either be installed as a major mode
; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; or as a minor mode under js
(add-hook 'js-mode-hook 'js2-minor-mode)

;; Also hook it on for shell scripts running via node.js
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
; (add-to-list 'interpreter-mode-alist '("node" . js-mode))

(provide 'init-javascript)

(eval-after-load 'js-mode
  '(progn
     (install-package 'coffee-mode)
     (install-package 'js-comint)
     (install-package 'js2-mode)
     (install-package 'json-mode)
     (install-package 'ac-js2)
     (install-package 'rainbow-delimiters)

     (require 'skewer-mode)

     ;; js2-mode-20140114
     ;; This mode does not yet work with "multi-mode" modes such as `mmm-mode'
     ;; and `mumamo', although it could be made to do so with some effort.
     ;; This means that `js2-mode' is currently only useful for editing JavaScript
     ;; files, and not for editing JavaScript within <script> tags or templates.

     ;; To unset the mouse
     ;; (global-unset-mouse)

     ;; Colorful parenthesis
     (rainbow-delimiters-mode)

     ;; Imenu support?
     (js2-imenu-extras-setup)

     (define-key js2-mode-map (kbd "TAB") 'indent-for-tab-command)

     (setq mode-name "JS2"
	   indent-tabs-mode nil
	   js-indent-level 2
	   js2-basic-offset 2
	   js2-use-font-lock-faces t
	   js2-mode-must-byte-compile nil
	   js2-indent-on-enter-key t
	   js2-auto-indent-p t
	   js2-bounce-indent-p nil)))
