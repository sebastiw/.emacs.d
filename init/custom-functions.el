(defun open-dot-emacs ()
  (interactive)
  (let ((initfiles '(".emacs" "_emacs" ".emacs.d/init.el"))
        (filefound)
        (file))

    (while (and (not filefound) initfiles)
      (setq file (concat "~/" (car initfiles))
            initfiles (cdr initfiles))

      (when (file-exists-p file)
        (setq filefound t)
        (find-file file)))))


(defun select-previous-window ()
  (interactive)
  (select-window (previous-window)))

(defun insert-latex ()
  (interactive)
  (insert-file "~/.emacs.d/.latexmall"))

(defun insert-random-number ()
  (interactive)
  (insert (number-to-string (random 100))))

(defun fullscreen ()
 (interactive)
 (set-frame-parameter nil 'fullscreen
                      (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

(defun untabify-buffer ()
  "Untabify current buffer."
  (interactive)
  (save-excursion (untabify (point-min) (point-max))))

(provide 'custom-functions)
