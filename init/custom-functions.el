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

(defun view-doc-in-emacs (File &optional Directory)
  (interactive)
;  (other-window)
;  (letf (current-page (doc-view-current-page))
  (print (format "Lol: file is %s in %s" File Directory))
    (find-alternate-file-other-window (format "%s%s" Directory File)))
;    (other-window)
;    (doc-view-goto-page current-page)))


(provide 'custom-functions)
