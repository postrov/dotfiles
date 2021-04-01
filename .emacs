;; -*-coding: utf-8 -*-

;; own emacs lisp dir
(defvar *local-elisp-dir* "~/.elisp")
(defun local-elisp-subpath (subdir)
  (if (> (length subdir) 0)
      (let (sep)
        (unless (eq ?/ (aref subdir 0))
          (setf sep "/"))
        (concat *local-elisp-dir* sep subdir))))

;; try setting variable via appropriate custom-set function
;; fallback to set-default
(defun set-activate (variable value)
  "Set VARIABLE to VALUE.  If there is a set-function, call it."
  (custom-load-symbol variable)
  (funcall (or (get variable 'custom-set) 'set-default) variable value))

;; load own functions
(load-file (local-elisp-subpath "/config/functions.el"))

(defun add-to-load-path (path)
  (push path load-path))

(defun add-local-load-path (relative-path)
  (add-to-load-path (local-elisp-subpath relative-path)))

;; package/MELPA
(require 'package)
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(setq package-user-dir (expand-file-name "elpa/" user-emacs-directory))
(package-initialize)

;; system-wide site-lisp path
;; (add-to-load-path "/usr/share/emacs/site-lisp" )
(add-to-load-path *local-elisp-dir*)

 
;; use buffer menu instead of buffer list
;; (global-set-key [(control x) (control b)] 'buffer-menu) ; removed for helm-mini
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; turn off menu bar - available under C-Mouse3
(menu-bar-mode -1)
;; turn off scrollbars
(set-activate 'scroll-bar-mode nil)
;; turn off toolbar
(set-activate 'tool-bar-mode nil)
;; use spaces instead of tabs
(set-activate 'indent-tabs-mode nil)
;; turn off this annoying bell
(set-activate 'visible-bell 'top-bottom)

;; all backups go to ~/.emacs.d/backups
(set-activate 'backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; ask when leaving emacs
(set-activate 'confirm-kill-emacs 'y-or-n-p)
;; enable downcase region
(put 'downcase-region 'disabled nil)
;; enable upcase region
(put 'upcase-region 'disabled nil)
;; enable narrow to  region
(put 'narrow-to-region 'disabled nil)
;; enable set goal column
(put 'set-goal-column 'disabled nil)
;; enable 'a' in dired
(put 'dired-find-alternate-file 'disabled nil)

;; do not enter debugger when evaluating lisp expressions
(setq eval-expression-debug-on-error nil)

;; Install use-package that we require for managing all other dependencies
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; enable command history between sessions
;(use-package save-history
;  :ensure)

(set-activate 'calendar-longitude 21)
(set-activate 'calendar-latitude 52.15)
;; set default dictionary to polish
(set-activate 'ispell-dictionary "polish")
;; forbid server raising new frame
(set-activate 'server-raise-frame nil)

;;; company mode
(set-activate 'company-idle-delay 0.25)
(set-activate 'company-minimum-prefix-length 2)

;;; CIDER
(add-hook 'cider-repl-mode-hook #'subword-mode)
(add-hook 'cider-repl-mode-hook #'company-mode)

(add-hook 'clojure-mode-hook #'subword-mode)
(add-hook 'clojure-mode-hook #'company-mode)


;; TERN
(add-to-list 'load-path "~/js/tern/emacs")
(autoload 'tern-mode "tern.el" nil t)


;; JS2 mode
(add-to-list 'auto-mode-alist '("\\.js" . js2-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (helm popup async company exec-path-from-shell toml-mode yasnippet flycheck lsp-ui lsp-mode rustic selectrum which-key ac-cider js2-mode company-tern cider))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(load-theme 'leuven t)

;;; font setup
(cond
 ((member "Cascadia Code" (font-family-list))
  (set-face-attribute 'default nil :font "Cascadia Code PL"))
 ((member "Monaco" (font-family-list))
  (set-face-attribute 'default nil :font "Monaco-12"))
 ((member "Inconsolata" (font-family-list))
  (set-face-attribute 'default nil :font "Inconsolata-12"))
 ((member "Consolas" (font-family-list))
  (set-face-attribute 'default nil :font "Consolas-11"))
 ((member "DejaVu Sans Mono" (font-family-list))
  (set-face-attribute 'default nil :font "DejaVu Sans Mono-10")))


(use-package which-key
  :ensure
  :init
  (which-key-mode))

;;; rust support
(load-file (local-elisp-subpath "/rust.el"))
(load-file (local-elisp-subpath "/helm-init.el"))
