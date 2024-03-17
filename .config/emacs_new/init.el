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
(use-package quelpa)
(use-package quelpa-use-package)
(quelpa-use-package-activate-advice)
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
  ;;(setq evil-undo-system 'undo-fu)
  :config
  (evil-mode 1))


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
(defun my-setup-color-theme ()
  (interactive)
  (when (display-graphic-p)
    (load-theme (car modus-themes-to-toggle))))

(use-package modus-themes
  :quelpa (modus-themes :fetcher github :repo "protesilaos/modus-themes")
  :init (setq modus-themes-to-toggle '(modus-vivendi modus-operandi))
  :config (my-setup-color-theme))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("0f76f9e0af168197f4798aba5c5ef18e07c926f4e7676b95f2a13771355ce850" default))
 '(package-selected-packages
   '(evil-collection cmake-mode quelpa-use-package modus-themes evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
