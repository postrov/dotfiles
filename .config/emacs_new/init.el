;;; Package system
(setq package-archives 
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(setq warning-minimum-level :error)
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
(xterm-mouse-mode 1)

;;; Vim Bindings
(use-package evil
  :demand t
  :bind (("<escape>" . keyboard-escape-quit))
  :init
  ;; allows for using cgn
  ;; (setq evil-search-module 'evil-search)
  ;; "\" switches to emacs for one command, so just "\ C-u" if needed
  (setq evil-want-C-u-scroll t)
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
	;; don't enable transparency on windows/wsl
	(unless (member system-name '("LAPTOP-TH7UB9JA" "Pasza-PC"))
	  (set-frame-parameter nil 'alpha-background 90))
	(when-let ((available-fonts (font-family-list))
		   (found (seq-find (lambda (x) (member (car x) available-fonts))
				    '(("Cascadia Code" . 13)
				      ("Monaco" . 12)
				      ("Inconsolata" . 12)
				      ("Consolas" . 11)
				      ("DejaVu Sans Mono" . 13))))
		   (font (car found))
		   (size (cdr found)))
	  (set-frame-font (format "%s-%d" font size))))
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
   '(janet-ts-mode paredit slynk sly flycheck compat prescient catpuccin-theme catppuccin-theme rust-mode dap-mode lsp-ui lsp-mode go-mode affe orderless vertico company-prescient evil-collection cmake-mode quelpa-use-package modus-themes evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(evil-set-leader '(normal visual) (kbd "SPC"))
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
(evil-define-key 'normal 'global (kbd "<leader>/") 'comment-line)


(use-package go-mode
  :mode "\\.go\\'"
  :config
  (defun my/go-mode-setup ()
    "Basic Go mode setup."
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t)))

(use-package lsp-ui)
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-mode lsp-deferred)
  :hook ((rust-mode go-mode) . lsp-deferred)
  :init (add-hook
	 'lsp-mode-hook
	 (lambda nil
	   (define-key evil-normal-state-local-map
		       (kbd "gd") 'lsp-find-definition)
	   (define-key evil-normal-state-local-map
		       (kbd "<leader>lfr") 'lsp-find-references)
	   (define-key evil-normal-state-local-map
		       (kbd "<leader>lrn") 'lsp-rename)
	   (define-key evil-normal-state-local-map
		       (kbd "<leader>lj") 'flycheck-next-error)
	   (define-key evil-normal-state-local-map
		       (kbd "<leader>lk") 'flycheck-previous-error)
	   (define-key evil-normal-state-local-map
		       (kbd "<leader>k") 'lsp-execute-code-action)
	   ))
  :config
  (setq lsp-prefer-flymake nil
        lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil
        lsp-rust-server 'rust-analyzer)
  ;; for filling args placeholders upon function completion candidate selection
  ;; lsp-enable-snippet and company-lsp-enable-snippet should be nil with
  ;; yas-minor-mode is enabled: https://emacs.stackexchange.com/q/53104
  (lsp-modeline-code-actions-mode)
  ;; (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (add-to-list 'lsp-file-watch-ignored "\\.vscode\\'"))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "C-Y") #'company-complete-selection))

(use-package sly
  :mode ("\\.lisp\\'" . lisp-mode)
  :hook (sly-mode lambda ()
	    (company-mode)
            (define-key evil-normal-state-local-map
                        (kbd "<leader>er") 'sly-eval-defun)
            (define-key evil-normal-state-local-map
                        (kbd "<leader>ee") 'sly-eval-last-expression)
            (define-key evil-normal-state-local-map
                        (kbd "<leader>eb") 'sly-eval-buffer)
            (define-key evil-visual-state-local-map
                        (kbd "<leader>E") 'sly-eval-region)
            (define-key evil-normal-state-local-map
                        (kbd "<leader>lfr") 'sly-edit-uses)
            (define-key evil-normal-state-local-map
                        (kbd "<leader>gd") 'sly-edit-definition)
            (define-key evil-normal-state-local-map
                        (kbd "<leader>hd") 'sly-describe-symbol)
	    ))

; (use-package evil-paredit
; 	:after paredit)

(use-package paredit
    :init
    (add-hook 'clojure-mode-hook #'enable-paredit-mode)
    (add-hook 'cider-repl-mode-hook #'enable-paredit-mode)
    (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
    (add-hook 'ielm-mode-hook #'enable-paredit-mode)
    (add-hook 'lisp-mode-hook #'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
    (add-hook 'scheme-mode-hook #'enable-paredit-mode)
    :config
    (show-paren-mode t)
    :hook (paredit-mode lambda ()
            (define-key evil-normal-state-local-map
                        (kbd "<leader>@") 'paredit-splice-sexp)
            (define-key evil-normal-state-local-map
                        (kbd "<leader>w") 'paredit-wrap-sexp)
            (define-key evil-normal-state-local-map
                        (kbd ">)") 'paredit-forward-slurp-sexp)
            (define-key evil-normal-state-local-map
                        (kbd "<)") 'paredit-forward-barf-sexp)
            (define-key evil-normal-state-local-map
                        (kbd ">(") 'paredit-backward-barf-sexp)
            (define-key evil-normal-state-local-map
                        (kbd "<(") 'paredit-backward-slurp-sexp)
            (define-key evil-normal-state-local-map
                        (kbd "<leader>o") 'paredit-raise-sexp)
	    )
    :diminish nil
    )

;;     :bind (("M-[" . paredit-wrap-square)
;; 	   ("M-{" . paredit-wrap-curly))

;; ;; (use-package lsp-mode
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

(use-package consult
  ;:load-path "~/vendor/consult"
  ;:quelpa (consult :fetcher github :repo "minad/consult")
  :after projectile
  :bind (("C-x r x" . consult-register)
         ("C-x r b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complet-command
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ("M-g o" . consult-outline)
         ("M-g h" . consult-org-heading)
         ("M-g a" . consult-org-agenda)
         ("M-g m" . consult-mark)
         ("C-x b" . consult-buffer)
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-project-imenu)
         ("M-g e" . consult-error)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s i" . consult-info)
         ("M-s L" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch)
         ("M-g l" . consult-line)
         ("M-s m" . consult-multi-occur)
         ("C-x c o" . consult-multi-occur)
         ("C-x c SPC" . consult-mark)
         :map isearch-mode-map
         ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
         ("M-s l" . consult-line))
  :init
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  :custom
  consult-preview-key '(:debounce 0.2 any)
  consult-narrow-key "<"
  :config
  (setq consult-project-root-function #'projectile-project-root))
