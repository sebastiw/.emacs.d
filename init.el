
(add-to-list 'load-path (expand-file-name "other" user-emacs-directory))

(defun sanityinc/time-subtract-millis (b a)
  (* 1000.0 (float-time (time-subtract b a))))

(defvar sanityinc/require-times nil
  "A list of (FEATURE . LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

(defadvice require
  (around build-require-times (feature &optional filename noerror) activate)
  "Note in `sanityinc/require-times' the time taken to require each feature."
  (let* ((already-loaded (memq feature features))
         (require-start-time (and (not already-loaded) (current-time))))
    (prog1
        ad-do-it
      (when (and (not already-loaded) (memq feature features))
        (add-to-list 'sanityinc/require-times
                     (cons feature
                           (sanityinc/time-subtract-millis (current-time)
                                                           require-start-time))
                     t)))))

(defvar elpa-dir "elpa"
  "Which directory elpa packages should be installed in.")

(require 'package)
(setq package-archives
      '(("gnu"       . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa"     . "http://melpa.milkbox.net/packages/")
        ("org"       . "http://orgmode.org/elpa/")
        ("elpy"      . "http://jorgenschaefer.github.io/packages/")))

(unless (file-exists-p  (concat user-emacs-directory elpa-dir))
  (message "The directory %s does not exist, creating it." elpa-dir)
  (package-refresh-contents))

(package-initialize)

(defun ensure-pkg (&rest pkgs)
  "If package PKG is not installed, install it."
  (dolist (pkg pkgs)
    (unless (package-installed-p pkg)
      (progn
        (unless (assoc pkg package-archive-contents)
          (package-refresh-contents))
        (package-install pkg)))))

(ensure-pkg 'smex)
(global-set-key [(meta x)] (lambda ()
                             (interactive)
                             (or (boundp 'smex-cache)
                                 (smex-initialize))
                             (global-set-key [(meta x)] 'smex)
                             (smex)))

(global-set-key [(shift meta x)] (lambda ()
                                   (interactive)
                                   (or (boundp 'smex-cache)
                                       (smex-initialize))
                                   (global-set-key [(shift meta x)] 'smex-major-mode-commands)
                                   (smex-major-mode-commands)))

(ensure-pkg 'ido 'ido-vertical-mode 'ido-ubiquitous 'ido-hacks)

(require 'ido)
(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching t)

(setq org-completion-use-ido t)
(setq magit-completing-read-function 'magit-ido-completing-read)

(require 'ido-vertical-mode)
(setq ido-use-faces t)
(set-face-attribute 'ido-vertical-first-match-face nil
                    :background nil
                    :foreground "orange")
(set-face-attribute 'ido-vertical-only-match-face nil
                    :background nil
                    :foreground nil)
(set-face-attribute 'ido-vertical-match-face nil
                    :foreground nil)
(ido-vertical-mode 1)

(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

(ensure-pkg 'ido-hacks)
(require 'ido-hacks)
(ido-hacks-mode)

(ensure-pkg 'popwin)
(require 'popwin)
(popwin-mode 1)

(ensure-pkg 'fill-column-indicator)
(require 'fill-column-indicator)

(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)

(ensure-pkg 'highlight-chars)
(require 'highlight-chars)
;(hc-toggle-highlight-tabs t)
;(hc-toggle-highlight-trailing-whitespace t)
(add-hook 'font-lock-mode-hook 'hc-highlight-tabs)
(add-hook 'font-lock-mode-hook 'hc-highlight-trailing-whitespace)

(ensure-pkg 'rainbow-delimiters)
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(ensure-pkg 'auto-complete)
(require 'auto-complete)
(setq ac-delay 0
      ac-use-fuzzy t
      ac-auto-start 2)

(defun clearcase-mode-on ()
  (interactive)
  (setq directory-sep-char ?/)
  (require 'clearcase))

(ensure-pkg 'nyan-mode)
(require 'nyan-mode)
(nyan-mode 1)

(winner-mode 1)

(show-paren-mode t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(global-hl-line-mode 1)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-strip-common-suffix t)

(column-number-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)

(setq standard-indent 2
      doc-view-continuous t
      inhibit-startup-screen t
      find-file-wildcards t)

(setq-default indent-tabs-mode nil
              fill-column 80)

(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10"))

(defvar oni:normal-color "DarkOliveGreen"
  "Cursor color to pass along to `set-cursor-color' for normal
  buffers.")

(defvar oni:normal-cursor-type 'bar
  "A `cursor-type' for normal buffers.")

(defvar oni:overwrite-color "red"
  "Cursor color to pass along to `set-cursor-color' for buffers
  in overwrite mode.")

(defvar oni:overwrite-cursor-type 'box
  "A `cursor-type' for buffers in overwrite mode.")

(defvar oni:read-only-color "DarkGrey"
  "Cursor color to pass along to `set-cursor-color' for read-only
  buffers.")

(defvar oni:read-only-cursor-type 'hbar
  "A `cursor-type' for read-only buffers.")

(defun oni:set-cursor-according-to-mode ()
  "Change cursor color and type according to some minor modes."
  (cond
   (buffer-read-only
    (set-cursor-color oni:read-only-color)
    (setq cursor-type oni:read-only-cursor-type))
   (overwrite-mode
    (set-cursor-color oni:overwrite-color)
    (setq cursor-type oni:overwrite-cursor-type))
   (t
    (set-cursor-color oni:normal-color)
    (setq cursor-type oni:normal-cursor-type))))

(add-hook 'post-command-hook 'oni:set-cursor-according-to-mode)

(add-to-list 'find-file-not-found-functions #'create-non-existent-directory)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;(ensure-pkg 'auto-complete 'auto-complete-config)
;(require 'auto-complete)
;(setq ac-auto-show-menu 0.01
;      ac-auto-start 1
;      ac-delay 0.01)

(ensure-pkg 'company)
(add-hook 'after-init-hook 'global-company-mode)

(add-hook 'c-mode-hook
          (lambda () (local-set-key (kbd "M-,") #'pop-tag-mark)))
(add-hook 'c-mode-hook
          (lambda () (local-set-key (kbd "M-*") #'tags-loop-continue)))

(add-to-list 'auto-mode-alist '("\\.[eh]rl\\'" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.yaws?\\'" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.escript?\\'" . erlang-mode))

(ensure-pkg 'erlang)
(require 'erlang-start)
(let ((erootdir (if (boundp 'erlang-root-dir) erlang-root-dir nil))
        (exe-find (if (executable-find "erl")
                      (directory-file-name (file-name-directory (executable-find "erl")))
                    nil))
        (shell-cmd-find (if (file-name-directory (shell-command-to-string "which erl"))
                            (directory-file-name (file-name-directory (shell-command-to-string "which erl")))
                          nil)))

    (if (and (equal erootdir nil)
             (equal exe-find "")
             (equal shell-cmd-find ""))
        (error "Could not find erlang, set the variable `erlang-root-dir'"))

    (if (equal erootdir nil)
        (if (equal exe-find "")
            (setq erlang-root-dir shell-cmd-find)
          (setq erlang-root-dir exe-find))))

(ensure-pkg 'edts)
(setq edts-man-root (expand-file-name ".." erlang-root-dir))
(add-hook 'erlang-mode-hook '(lambda () (require 'edts-start)))

(defvar eqc-root-dir (expand-file-name "lib/eqc-1.30.0"
                                       erlang-root-dir)
 "Where EQC is installed.")
(defvar eqc-load-path (expand-file-name "lib/eqc-1.30.0/emacs/"
                                        erlang-root-dir)
 "EQC's load path.")

(when (file-exists-p eqc-root-dir)
    (add-to-list 'load-path eqc-load-path)
    (autoload 'eqc-erlang-mode-hook "eqc-ext" "EQC Mode" t)
    (add-hook 'erlang-mode-hook 'eqc-erlang-mode-hook)
    (setq eqc-max-menu-length 30))

(ensure-pkg 'elisp-slime-nav)
(require 'elisp-slime-nav)
(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
(add-hook 'lisp-interaction-mode-hook 'elisp-slime-nav-mode)

(ensure-pkg 'paredit)
(require 'paredit)
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)

(defalias 'emacs 'find-file)
(defalias 'ec 'find-file)
(defalias 'd 'dired)

(setenv "TERM" "xterm-256color")
(setenv "PAGER" "cat")

(eval-after-load 'esh-opt
   '(progn
     (require 'em-cmpl)
     (require 'em-prompt)
     (require 'em-term)
     ; (require 'em-unix)

(add-to-list 'eshell-visual-commands "el")
(add-to-list 'eshell-visual-commands "elinks")
(add-to-list 'eshell-visual-commands "htop")
(add-to-list 'eshell-visual-commands "tail")

(require 'em-hist)
(setq eshell-history-size 20000
      eshell-save-history-on-exit t
      eshell-hist-ignoredups t)


;; History if Helm is installed
(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key eshell-mode-map (kbd "M-l")
              'helm-eshell-history)))
;; History if ido is installed
(add-hook 'eshell-mode-hook
          (lambda ()
            (local-set-key
             (kbd "C-c h")
             (lambda ()
               (interactive)
               (insert
                (ido-completing-read
                 "Eshell history: "
                 (delete-dups
                  (ring-elements eshell-history-ring))))))
            (local-set-key (kbd "C-c C-h") 'eshell-list-history)))

(require 'em-smart)

(setq eshell-where-to-jump 'begin
      eshell-review-quick-commands nil
      eshell-smart-space-goes-to-end t)

(add-hook 'eshell-mode-hook
          '(lambda () (define-key eshell-mode-map "\C-a" 'eshell-bol)))

(add-to-list 'eshell-command-completions-alist
             '("gunzip" "gz\\'"))
(add-to-list 'eshell-command-completions-alist
             '("tar" "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'"))
;(add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color)

(ensure-pkg 'eshell-prompt-extras 'virtualenvwrapper)

    ;; Show python virtual environment information
    (require 'virtualenvwrapper)
    (venv-initialize-eshell)

    (require 'eshell-prompt-extras)

    (require 'cl)
    (defun oni:shorten-dir (dir)
      "Shorten a directory, (almost) like fish does it."
      (let ((scount (1- (count ?/ dir))))
        (dotimes (i scount)
          (string-match "\\(/\\.?.\\)[^/]+" dir)
          (setq dir (replace-match "\\1" nil nil dir))))
      dir)
    (defun oni:eshell-prompt-function ()
      (let ((status (if (zerop eshell-last-command-status) ?+ ?-))
            (hostname (shell-command-to-string "hostname"))
            (dir (abbreviate-file-name (eshell/pwd)))
            (branch
             (shell-command-to-string
              "sh -c \"git branch --contains HEAD 2>/dev/null\""))
            (userstatus (if (zerop (user-uid)) ?# ?$)))
        (format "%c%s:%s%s %c "
                status
                (substring hostname 0 -1)
                (oni:shorten-dir dir)
                (if (not (string= branch ""))
                  (concat "@" (substring branch 2 -1))
                 "")
                userstatus)))

    (setq eshell-highlight-prompt t
;          epe-git-dirty-char "*"
          eshell-prompt-function 'oni:eshell-prompt-function ;epe-theme-dakrone
    )))

(autoload 'gnus-alias-determine-identity "gnus-alias" "" t)
(add-hook 'message-setup-hook 'gnus-alias-determine-identity)

(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))

(ensure-pkg 'haskell-mode 'hi2)
(eval-after-load 'haskell-mode
    '(progn
      (local-set-key (kbd "C-c C-k") 'haskell-compile)

      ;; Haskell-indentation
      (require 'hi2)
      (hi2-mode)
      (require 'haskell-mode-autoloads)

      (turn-on-haskell-indentation)
      (turn-on-haskell-doc-mode)
      (turn-on-haskell-decl-scan)

      (setq haskell-compile-command "ghc -Wall -threaded -eventlog -rtsopts %s")))

(add-to-list 'auto-mode-alist '("\\.java\\'" . java-mode))

(ensure-pkg 'android-mode)
(eval-after-load 'java-mode
    '(progn
      (require 'android)
      (android-mode)
      (custom-set-variables '(android-mode-sdk-dir
                              "~/Android/android-sdk-linux"))))

(add-hook 'js-mode-hook 'js2-minor-mode)

(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
; (add-to-list 'interpreter-mode-alist '("node" . js-mode))

(ensure-pkg 'coffee-mode 'js-comint 'js2-mode 'json-mode 'ac-js2)
(eval-after-load 'js-mode
  '(progn
    (require 'coffee-mode)
    (require 'js-comint)
    (require 'js2-mode)
    (require 'json-mode)
    (require 'ac-js2)

    (require 'skewer-mode)

    ;; js2-mode-20140114
    ;; This mode does not yet work with "multi-mode" modes such as `mmm-mode'
    ;; and `mumamo', although it could be made to do so with some effort.
    ;; This means that `js2-mode' is currently only useful for editing
    ;; JavaScript files, and not for editing JavaScript within <script> tags
    ;; or templates.

    ;; To unset the mouse
    ;; (global-unset-mouse)

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

(add-to-list 'auto-mode-alist '("\\.(la)?tex\\'" . LaTeX))

(ensure-pkg 'auctex 'auctex-latexmk 'ispell 'ac-ispell 'writegood-mode
            'smartparens 'ac-math)
(require 'tex-mode)
(eval-after-load 'LaTeX
    '(progn
      (bind-key "C-c i" 'insert-latex LaTeX-mode-map)
      (bind-key "C-c C-c" 'TeX-comment-or-uncomment-region LaTeX-mode-map)
      (bind-key "C-c C-k" 'TeX-command-master LaTeX-mode-map)

      (require 'auctex)
      (require 'auctex-latexmk)
      (require 'ispell)
      (require 'ac-ispell)
      (require 'writegood-mode)
      (require 'smartparens-latex)
      (require 'ac-math)

      (auctex-latexmk-setup)
      (make-local-variable 'ispell-parser)
      (setq ispell-parser 'tex)
      (writegood-mode)
      (smartparens-mode 1)
      (LaTeX-math-mode)


    (visual-line-mode t)
    (flyspell-mode t)
    (auto-fill-mode t)
    (abbrev-mode +1)

    (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|BUG\\)" 1 font-lock-warning-face t)))

    (setq-default TeX-master nil)

    (setq LaTeX-command "latex"
          TeX-parse-self t
          TeX-auto-save t
          TeX-PDF-mode t
          TeX-source-correlate-method 'synctex
          TeX-source-correlate-mode t
          TeX-source-correlate-start-server t
          TeX-clean-confirm nil
          TeX-view-predicate-list '((output-pdf (string-match "pdf" (TeX-output-extension))))
          TeX-view-program-list
          '(("Default"
             (lambda () (interactive) (progn (TeX-clean) (find-file-other-window "%o")))))
          TeX-view-program-selection '((output-pdf "Default")))))

(define-skeleton my-tex-default
  "Latex default skeleton"
  (concat
   "\\documentclass[11pt,a4paper]{report}\n"
   "\\usepackage[OT1]{fontenc}\n"
   "\\usepackage[utf8x]{inputenc}\n"
   "\\usepackage[english]{babel}\n\n"
   "\\begin{document}\n\n\n"
   "\\end{document}"))

(define-auto-insert "\\.tex\\'" 'my-tex-default)

(add-hook 'doc-view-mode-hook (lambda ()
                                (setq doc-view-resolution 300)
                                (auto-revert-mode)))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)

;; Where to keep org agenda files
(setq org-agenda-files (list "~/ORG/"))

(eval-after-load "org"
  '(progn
     (setq
      ;; Indent after a new node?
      org-adapt-indentation t
      ;; Stupid yanks, Monday is the first day of the week
      calendar-week-start-day 1
      ;; Do not dim blocked items
      org-agenda-dim-blocked-tasks nil
      ;; How many days to include in overview
      org-agenda-span 'week
      ;; Show all occurrences of a repeating timestamp
      org-agenda-repeating-timestamp-show-all nil
      ;; Don't show deadlines if the item is done
      org-agenda-skip-deadline-if-done t
      ;; Don't show scheduled items in agenda if done
      org-agenda-skip-scheduled-if-done t
      ;; Start agenda on the current day
      org-agenda-start-on-weekday nil
      ;; Unchecked boxes will block switching the parent to DONE
      org-enforce-todo-checkbox-dependencies t
      ;; Provide refile targets as paths
      org-refile-use-outline-path t
      ;; Store new notes at the beginning
      org-reverse-note-order t
      ;; Be able to mark a region using Shift
      org-support-shift-select t
      ;; TeX-like sub and superscripts with X^{some} and Y_{thing}
      org-use-sub-superscripts '{}
      ;; Hide the markup elements
      org-hide-emphasis-markers t
;;;        org-agenda-tags-todo-honor-ignore-options t
;;;        org-clock-modeline-total 'today
;;;        org-mobile-force-id-on-agenda-items nil
;;;        org-habit-show-habits-only-for-today nil
      )

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (java . t)
   (dot . t)
   (ditaa . t)
   (R . t)
   (python . t)
   (ruby . t)
   (gnuplot . t)
   (clojure . t)
   (sh . t)
   (ledger . t)
   (org . t)
   (plantuml . t)
   (latex . t)))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/ORG/gtd.org" "Inbox")
             "* TODO %^{Brief Description}\n%U\n%?")))

;     (define-key org-mode-map (kbd "C-c C-c") 'org-todo)
;     (define-key org-mode-map (kbd "C-c C-k") 'org-ctrl-c-ctrl-c)

     ;; I really like to change windows with C-<tab>
     (define-key org-mode-map (kbd "C-<tab>") 'other-window)

     ;; Insert template (special function defined below)
     (define-key org-mode-map (kbd "C-#") 'org-begin-template)

(lambda () (font-lock-add-keywords nil '(("\\<\\(FIXME\\|UNREACHABLE\\|REACHABLE\\|BUG\\)" 1 font-lock-warning-face t))))

(defun org-begin-template ()
  "Make a template at point."
  (interactive)
  (if (org-at-table-p)
      (call-interactively 'org-table-rotate-recalc-marks)
    (let* ((choices '(("s" . "SRC")
                      ("e" . "EXAMPLE")
                      ("q" . "QUOTE")
                      ("v" . "VERSE")
                      ("c" . "CENTER")
                      ("l" . "LaTeX")
                      ("h" . "HTML")
                      ("a" . "ASCII")))
           (key
            (key-description
             (vector
              (read-key
               (concat (propertize "Template type: " 'face 'minibuffer-prompt)
                       (mapconcat (lambda (choice)
                                    (concat (propertize (car choice) 'face 'font-lock-type-face)
                                            ": "
                                            (cdr choice)))
                                  choices
                                  ", ")))))))
      (let ((result (assoc key choices)))
        (when result
          (let ((choice (cdr result)))
            (cond
             ((region-active-p)
              (let ((start (region-beginning))
                    (end (region-end)))
                (goto-char end)
                (insert "\n#+END_" choice)
                (goto-char start)
                (insert "#+BEGIN_" choice "\n")))
             (t
              (insert "#+BEGIN_" choice "\n")
              (save-excursion (insert "\n#+END_" choice))))))))))))

(ensure-pkg 'python 'elpy)
; (add-to-list 'load-path (expand-file-name "python-2*/" "~/.emacs.d/elpa/"))
(require 'python "python.el")
(setq python-indent-offset 4)
(elpy-enable)

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.php5?\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(autoload 'xmodmap-mode "xmodmap-mode" nil t)
(add-to-list 'auto-mode-alist '("^\\.Xmodmap$" . xmodmap-mode))

(defun open-dot-emacs ()
  (interactive)
  (let ((user-init-file-org (concat (file-name-directory user-init-file)
                                    (file-name-base user-init-file)
                                    ".org")))
    (if (file-exists-p user-init-file-org)
      (find-file user-init-file-org)
     (find-file user-init-file))))

(defun select-previous-window ()
  (interactive)
  (select-window (previous-window)))

(defun insert-latex ()
  (interactive)
  (insert-file "~/.emacs.d/.latexmall"))

(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

(defun untabify-buffer ()
  "Untabify current buffer."
  (interactive)
  (save-excursion (untabify (point-min) (point-max))))

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

(defun create-non-existent-directory ()
  "If The parent directory does not exist, this function will ask to create it."
  (let ((parent-directory (file-name-directory buffer-file-name))
        (q "Directory `%s' does not exist! Create it?"))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p
                (format q parent-directory)))
      (make-directory parent-directory t))))

(defun insert-random-number ()
  (interactive)
  (insert (number-to-string (random 100))))

(defun hex-to-dec ()
  "Prints the decimal value of a hexadecimal string under cursor.
Samples of valid input:

  ffff
  0xffff
  #xffff
  FFFF
  0xFFFF
  #xFFFF

Test cases
  64*0xc8+#x12c 190*0x1f4+#x258
  100 200 300   400 500 600"
  (interactive)

  (let (inputStr tempStr p1 p2)
    (save-excursion
      (search-backward-regexp "[^0-9A-Fa-fx#]" nil t)
      (forward-char)
      (setq p1 (point))
      (search-forward-regexp "[^0-9A-Fa-fx#]" nil t)
      (backward-char)
      (setq p2 (point)))

    (setq inputStr (buffer-substring-no-properties p1 p2))

    (let ((case-fold-search nil))
      (setq tempStr (replace-regexp-in-string "^0x" "" inputStr)) ; C, Perl, …
      (setq tempStr (replace-regexp-in-string "^#x" "" tempStr)) ; elisp …
      (setq tempStr (replace-regexp-in-string "^#" "" tempStr))  ; CSS …
      )

    (message "Hex %s is %d" tempStr (string-to-number tempStr 16))))

(defun dec-to-hex ()
  "Convert decimal numbers to hexadecimal."
  (interactive)

  (let (inputStr p1 p2)
    (save-excursion
      (search-backward-regexp "[^0-9]" nil t)
      (forward-char)
      (setq p1 (point))
      (search-forward-regexp "[^0-9]" nil t)
      (backward-char)
      (setq p2 (point)))

  (setq inputStr (buffer-substring-no-properties p1 p2))

  (message "Dec %s is 0x%X" inputStr (string-to-number inputStr 10))))

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
;(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(global-set-key (kbd "M-,") 'pop-tag-mark)

(global-set-key (kbd "<f11>") 'fullscreen)

(global-set-key (kbd "C-z") 'eof)
