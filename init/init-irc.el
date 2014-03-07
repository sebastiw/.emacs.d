;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs IRC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-after-load "erc"
  '(progn
     (setq erc-nick "visnae"
           erc-autojoin-channels-alist '(("dtek.se" "#dtek"))
           erc-interpret-mirc-color t
           erc-button-buttonize-nicks nil)
     (erc-scrolltobottom-enable)
     (erc-spelling-mode t)))
