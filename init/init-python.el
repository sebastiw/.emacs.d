

;; (load-file (car (file-expand-wildcards
;;                  "~/.emacs.d/elpa/python*/python.el")))
;; (load-file (car (file-expand-wildcards
;;                  "~/.emacs.d/elpa/elpy-*/elpy.el")))

(setq python-indent-offset 4)
(elpy-enable)

(provide 'init-python)
