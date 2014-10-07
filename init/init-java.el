(provide 'init-java)

(eval-after-load 'java-mode
  '(progn
     (require 'android-mode)
     (android-mode)
     (custom-set-variables '(android-mode-sdk-dir "~/Android/android-sdk-linux"))))


; (require 'cedet)
; (global-ede-mode 1)
; (semantic-mode 1)

;;      (defcustom cedet-android-sdk-root "~/Downloads/android-sdk-linux/"
;;        "The root to the android SDK."
;;        :group 'android
;;        :type 'file)

;;      (require 'cedet)

;;      (setq semantic-default-submodes '(global-semantic-idle-completions-mode
;;                                        global-semantic-idle-scheduler-mode
;;                                        global-semantic-idle-summary-mode
;;                                        global-semantic-idle-local-symbol-highlight-mode
;;                                        global-semanticdb-minor-mode
;;                                        global-semantic-mru-bookmark-mode)

;;            semantic-idle-scheduler-idle-time 10
;; ;; ;           semanticdb-javap-classpath (quote ("/System/Library/Frameworks/JavaVM.framework/Versions/CurrentJDK/Classes/classes.jar"))
;;            c-basic-offset 2
;;            tab-width 2
;;            )

;;       (semantic-mode 1)
;;       (require 'semantic/ia)

;;       (require 'semantic/db-javap)

;;       (ede-java-root-project "Appen"
;;                              :file "~/Android/workspace/Appen/build.xml"
;;                              :srcroot '("src")
;;                              :classpath '("~/Android/android-sdk-linux/platforms/android-20/android.jar"))

;; ;;      (global-semanticdb-minor-mode 1)

;; ;;      (global-ede-mode t)

;;       (add-to-list 'ac-sources 'ac-source-gtags)
;;       (add-to-list 'ac-sources 'ac-source-semantic)

;;       (local-set-key [(control return)] 'semantic-ia-complete-symbol)
;;       (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
;;       (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
;;       (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
;;       (local-set-key "." 'semantic-complete-self-insert)
;;       (local-set-key ">" 'semantic-complete-self-insert)
;; ;;      ;; functions which are disabled
;; ;;      ;; (local-set-key "\C-cp" 'semantic-ia-show-summary)
;; ;;      ;; (local-set-key "\C-cl" 'semantic-ia-show-doc)
;; ;;      ;; (local-set-key "." 'semantic-complete-self-insert)
;; ;;      ;; (local-set-key ">" 'semantic-complete-self-insert)
;; ;;      (local-set-key "\M-n" 'semantic-ia-complete-symbol-menu)  ;; auto ompletet by menu
;; ;;      (local-set-key "\C-c/" 'semantic-ia-complete-symbol)
;; ;;      (local-set-key "\C-cb" 'semantic-mrub-switch-tags)
;; ;;      (local-set-key "\C-cj" 'semantic-ia-fast-jump)
;; ;;      (local-set-key "\C-cR" 'semantic-symref)
;; ;;      (local-set-key "\C-cr" 'semantic-symref-symbol)



;; ;; ede customization
;; (global-ede-mode t)
;; (ede-enable-generic-projects)
