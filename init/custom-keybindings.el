(global-set-key (kbd "\C-c\C-k") 'compile)

(global-set-key (kbd "\C-c e") 'open-dot-emacs)

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "<C-S-iso-lefttab>") 'select-previous-window)

(global-set-key (kbd "C-h o") 'find-library)

(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(global-set-key (kbd "M-,") 'pop-tag-mark)

(global-set-key (kbd "<f11>") 'fullscreen)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(provide 'custom-keybindings)
