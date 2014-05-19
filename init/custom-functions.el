(defun open-dot-emacs ()
  (interactive)
  (find-file user-init-file))

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

(defun downcase-word-at-point (&optional argv)
  "It should not matter where at the word you stand to do a downcase"
  (interactive)
  (case argv
    ('nil (downcase-word -1))
    (t (downcase-word -argv))))

(defun capitalize-word-at-point (&optional argv)
  "It should not matter where at the word you stand to do a capitalize"
  (interactive)
  (case argv
    ('nil (capitalize-word -1))
    (t (capitalize-word -argv))))


(provide 'custom-functions)
