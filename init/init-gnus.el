;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gnus | Mail
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Change header
(autoload 'gnus-alias-determine-identity "gnus-alias" "" t)
(add-hook 'message-setup-hook 'gnus-alias-determine-identity)

;; (setq gnus-alias-identity-alist '(())
;;       gnus-alias-identity-rules
;;       gnus-alias-default-identity
;;       gnus-alias-allow-forward-as-reply)

(provide 'init-gnus)
