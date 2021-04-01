(use-package helm
  :config
  (require 'helm-config)
  (setq helm-input-idle-delay                     0.01
        helm-reuse-last-window-split-state        t
        helm-always-two-windows                   t
        helm-split-window-inside-p                nil
        helm-commands-using-frame                 '(completion-at-point
                                                    helm-apropos
                                                    helm-eshell-prompts helm-imenu
                                                    helm-imenu-in-all-buffers)
        helm-actions-inherit-frame-settings       t
        helm-use-frame-when-more-than-two-windows t
        helm-use-frame-when-dedicated-window      t
;        helm-frame-background-color               "DarkSlateGray"
        helm-show-action-window-other-window      'left
        helm-allow-mouse                          t
        helm-move-to-line-cycle-in-source         t
        helm-autoresize-max-height                80 ; it is %.
        helm-autoresize-min-height                20 ; it is %.
        helm-debug-root-directory                 "/home/pasza/tmp/helm-debug"
        helm-follow-mode-persistent               t
        helm-candidate-number-limit               500)
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-info-bash)
  (helm-define-key-with-subkeys global-map (kbd "C-c n") ?n 'helm-cycle-resume))

(defun helm/debug-toggle ()
  (interactive)
  (setq helm-debug (not helm-debug))
  (message "Helm Debug is now %s"
           (if helm-debug "Enabled" "Disabled")))


(defun helm/turn-on-header-line ()
  (interactive)
  (setq helm-echo-input-in-header-line t)
  (setq helm-split-window-inside-p t)
  (helm-autoresize-mode -1)
  (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)
  )

(defun helm/turn-off-header-line ()
  (interactive)
  (setq helm-echo-input-in-header-line nil)
  ;;(helm-autoresize-mode 1)
  (setq helm-split-window-inside-p nil)
  (remove-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)
  )


;; (use-package helm-find
;;   :config
;;   (setq helm-find-noerrors t))


;; (use-package helm-grep
;;   :config
;;   (setq helm-pdfgrep-default-read-command
;;         "evince --page-label=%p '%f'"
;;         helm-grep-default-command
;;         "ack-grep -Hn --color --smart-case --no-group %e %p %f"
;;         helm-grep-default-recurse-command
;;         "ack-grep -H --color --smart-case --no-group %e %p %f"
;;         helm-grep-ag-command
;;         "rg --color=always --colors 'match:bg:yellow' --colors 'match:fg:black' --smart-case --no-heading --line-number %s %s %s"
;;         helm-grep-ag-pipe-cmd-switches
;;         '("--colors 'match:bg:yellow' --colors 'match:fg:black'")
;;         helm-grep-git-grep-command
;;         "git --no-pager grep -n%cH --color=always --exclude-standard --no-index --full-name -e %p -- %f")
;;   (add-hook 'helm-grep-mode-hook 'hl-line-mode)
;;   (define-key helm-grep-map   (kbd "C-M-a") 'helm/occur-which-func))


;;; Global-map
;;
;;
(global-set-key (kbd "M-x")                          'undefined)
(global-set-key (kbd "M-x")                          'helm-M-x)
(global-set-key (kbd "M-y")                          'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f")                      'helm-find-files)
(global-set-key (kbd "C-c <SPC>")                    'helm-all-mark-rings)
(global-set-key [remap bookmark-jump]                'helm-filtered-bookmarks)
;(global-set-key (kbd "C-:")                          'helm-eval-expression-with-eldoc)
;(global-set-key (kbd "C-,")                          'helm-calcul-expression)
;(global-set-key (kbd "C-h d")                        'helm-info-at-point)
;(global-set-key (kbd "C-h i")                        'helm-info)
;(global-set-key (kbd "C-x C-d")                      'helm-browse-project)
(global-set-key (kbd "<f1>")                         'helm-resume)
(global-set-key (kbd "C-h C-f")                      'helm-apropos)
(global-set-key (kbd "C-h a")                        'helm-apropos)
(global-set-key (kbd "C-h C-d")                      'helm-debug-open-last-log)
;(global-set-key (kbd "<f5> s")                       'helm-find)
;(global-set-key (kbd "S-<f2>")                       'helm-execute-kmacro)
(global-set-key (kbd "C-c i")                        'helm-imenu-in-all-buffers)
(global-set-key (kbd "C-c C-i")                      'helm-imenu)
;(global-set-key (kbd "<f11>")                        nil)
;(global-set-key (kbd "<f11> o")                      'helm-org-agenda-files-headings)
(global-set-key (kbd "M-s")                          nil)
(global-set-key (kbd "M-s")                          'helm-occur-visible-buffers)
;(global-set-key (kbd "<f6> h")                       'helm-emms)
(define-key global-map [remap jump-to-register]      'helm-register)
(define-key global-map [remap list-buffers]          'helm-mini)
(define-key global-map [remap dabbrev-expand]        'helm-dabbrev)
(define-key global-map [remap find-tag]              'helm-etags-select)
(define-key global-map [remap xref-find-definitions] 'helm-etags-select)
;(define-key global-map (kbd "M-g a")                 'helm-do-grep-ag)
;(define-key global-map (kbd "M-g g")                 'helm-grep-do-git-grep)
;(define-key global-map (kbd "M-g i")                 'helm-gid)
;(define-key global-map (kbd "C-x r p")               'helm-projects-history)
;(define-key global-map (kbd "C-x r c")               'helm-addressbook-bookmarks)
(define-key global-map (kbd "C-c t r")               'helm-dictionary)


(add-to-list 'completion-ignored-extensions ".gvfs/")
(add-to-list 'completion-ignored-extensions ".dbus/")
(add-to-list 'completion-ignored-extensions "dconf/")

