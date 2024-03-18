;;; Package system
(setq package-archives 
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

;;; Quelpa
;; (use-package quelpa)
;; (use-package quelpa-use-package)
;; (quelpa-use-package-activate-advice)
(setq load-prefer-newer t)

;;; Vim Bindings
(use-package evil
  :demand t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  ;; allows for using cgn
  ;; (setq evil-search-module 'evil-search)
  (setq evil-want-keybinding nil)
  ;; no vim insert bindings
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-mode 1))

(use-package undo-fu)

;;; Vim Bindings Everywhere else
(use-package evil-collection
  :after evil
  :config
  (setq evil-want-integration t)
  (evil-collection-init))

;;; relative line numbers
(setq-default display-line-numbers 'visual
              display-line-numbers-widen t
              ;; this is the default
              display-line-numbers-current-absolute t)

(defun noct-relative ()
  "Show relative line numbers."
  (setq-local display-line-numbers 'visual))

(defun noct-absolute ()
  "Show absolute line numbers."
  (setq-local display-line-numbers t))

(add-hook 'evil-insert-state-entry-hook #'noct-absolute)
(add-hook 'evil-insert-state-exit-hook #'noct-relative)

;;; Settings
(fset 'yes-or-no-p 'y-or-n-p)
(setq kill-ring-max 1000)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq calendar-longitude 21)
(setq calendar-latitude 52.15)
(setq ring-bell-function 'ignore)	

;;; Enable commands
(put 'narrow-to-region 'disabled nil)
(put 'list-timers 'disabled nil)


;;; Appearance
;; when display-graphic-p

(defun my-theme-settings ()
  (load-theme 'catppuccin :no-confirm)
  (if (display-graphic-p)
      (progn
	(set-frame-parameter nil 'alpha-background 90)
	(cond
	 ((member "Cascadia Code" (font-family-list))
	  (set-frame-font "Cascadia Code PL-13"))
	 ((member "Monaco" (font-family-list))
	  (set-frame-font "Monaco-12"))
	 ((member "Inconsolata" (font-family-list))
	  (set-frame-font "Inconsolata-12"))
	 ((member "Consolas" (font-family-list))
	  (set-frame-font "Consolas-11"))
	 ((member "DejaVu Sans Mono" (font-family-list))
	  (set-frame-font "DejaVu Sans Mono-13"))
	 (t nil)))
    (progn (set-face-background 'default "undefined")
	   (set-face-background 'line-number "undefined"))))



(use-package catppuccin-theme
  :init (my-theme-settings))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("80214de566132bf2c844b9dee3ec0599f65c5a1f2d6ff21a2c8309e6e70f9242" "0f76f9e0af168197f4798aba5c5ef18e07c926f4e7676b95f2a13771355ce850" default))
 '(package-selected-packages
   '(compat prescient catpuccin-theme catppuccin-theme rust-mode dap-mode lsp-ui lsp-mode slime-company slime go-mode affe orderless vertico company-prescient evil-collection cmake-mode quelpa-use-package modus-themes evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(evil-set-leader 'normal (kbd "SPC"))
(evil-define-key 'normal 'global (kbd "<leader>SPC") 'switch-to-buffer)

(use-package vertico :config (vertico-mode +1))
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (Completion-category-overrides '((file (styles basic partial-completion)))))
(use-package prescient :config (prescient-persist-mode +1))
(use-package company-prescient :init (company-prescient-mode +1))

(use-package affe)

(evil-define-key 'normal 'global (kbd "<leader>p") 'affe-find)
(evil-define-key 'normal 'global (kbd "<leader>s g") 'affe-grep)

;; TODO: load only for go
(use-package go-mode)

;; TODO: load only on lisp
(use-package slime)
;; TODO: SETUP
(use-package slime-company)


;; (use-package lsp-mode
;; ;  :if my-laptop-p
;;   :config
;;   (setq lsp-headerline-breadcrumb-enable t
;;         gc-cons-threshold (* 100 1024 1024)
;;         read-process-output-max (* 1024 1024)
;;         company-idle-delay 0.5
;;         company-minimum-prefix-length 1
;;         create-lockfiles nil ;; lock files will kill `npm start'
;;         )
;; ;  (lsp-register-custom-settings
;; ;   '(("pyls.plugins.pyls_mypy.enabled" t t)
;; ;     ("pyls.plugins.pyls_mypy.live_mode" nil t)
;; ;     ("pyls.plugins.pyls_black.enabled" t t)
;; ;     ("pyls.plugins.pyls_isort.enabled" t t)))
;; ;  :hook ((prog-mode-hook . lsp)
;; ;         (python-mode . lsp)
;; ;         (lsp-mode-hook . lsp-enable-which-key-integration))
;;   :commands lsp
;;   :after ht
;;   ;; :vc (:url "https://github.com/emacs-lsp/lsp-mode"
;;   ;;      :branch "13f400b4f108b6286089764b3efad082c67f72ed")
;;   )

;; (use-package lsp-ui
;;   :commands lsp-ui-mode
;;   :after lsp-mode)

;; (use-package dap-mode
;;   :after lsp-mode)


;; (use-package rust-mode
;;   :mode ("\\.rust$" . rust-mode)
;;   :commands (rust-mode)
;;   :config
;;   ;; install rustfmt using `cargo install rustfmt'
;;   (when (executable-find "rustfmt")
;;     (add-hook 'rust-mode-hook
;;               (lambda ()
;;                 (add-hook 'before-save-hook
;;                           (lambda ()
;;                             (rust-format-buffer)) nil t)))))  
