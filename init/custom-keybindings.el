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

(global-set-key (kbd "M-l") 'downcase-word-at-point)
(global-set-key (kbd "M-c") 'capitalize-word-at-point)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Unbind mouse events
(dolist (k '([mouse-1] [down-mouse-1] [drag-mouse-1] [double-mouse-1] [triple-mouse-1]
             [mouse-2] [down-mouse-2] [drag-mouse-2] [double-mouse-2] [triple-mouse-2]
             [mouse-3] [down-mouse-3] [drag-mouse-3] [double-mouse-3] [triple-mouse-3]
             [mouse-4] [down-mouse-4] [drag-mouse-4] [double-mouse-4] [triple-mouse-4]
             [mouse-5] [down-mouse-5] [drag-mouse-5] [double-mouse-5] [triple-mouse-5]
             [C-mouse-5] [S-mouse-5] [C-mouse-4] [S-mouse-4]
             [C-down-mouse-1] [C-down-mouse-3]))
  (global-unset-key k))
(provide 'custom-keybindings)
