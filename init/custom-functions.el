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

(defun upcase-word-at-point (&optional argv)
  "It should not matter where at the word you stand to do a capitalize"
  (interactive)
  (case argv
    ('nil (upcase-word -1))
    (t (upcase-word -argv))))

;; Unbind mouse events
(defun global-unset-mouse ()
  "Unset all mouse events"
  (interactive)
  (dolist (k '([mouse-1] [down-mouse-1] [drag-mouse-1]
               [double-mouse-1] [triple-mouse-1]
               [mouse-2] [down-mouse-2] [drag-mouse-2]
               [double-mouse-2] [triple-mouse-2]
               [mouse-3] [down-mouse-3] [drag-mouse-3]
               [double-mouse-3] [triple-mouse-3]
               [mouse-4] [down-mouse-4] [drag-mouse-4]
               [double-mouse-4] [triple-mouse-4]
               [mouse-5] [down-mouse-5] [drag-mouse-5]
               [double-mouse-5] [triple-mouse-5]
               [C-mouse-5] [S-mouse-5] [C-mouse-4] [S-mouse-4]
               [C-down-mouse-1] [C-down-mouse-3]))
   (global-unset-key k)))


(provide 'custom-functions)
