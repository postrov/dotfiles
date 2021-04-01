;; -*-coding: utf-8 -*-
;; for faster load time
(let ((file-name-handler-alist nil))

(unless (>= emacs-major-version 27)
  (load (expand-file-name "early-init.el" user-emacs-directory)))

;; own emacs lisp dir
(defvar *local-elisp-dir* "~/.elisp")
(defun local-elisp-subpath (subdir)
  (if (> (length subdir) 0)
      (let (sep)
        (unless (eq ?/ (aref subdir 0))
          (setf sep "/"))
        (concat *local-elisp-dir* sep subdir))))

;; load own functions
(load-file (local-elisp-subpath "/config/functions.el"))

(defun add-to-load-path (path)
  (push path load-path))

(defun add-local-load-path (relative-path)
  (add-to-load-path (local-elisp-subpath relative-path)))

;; package/straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; system-wide site-lisp path
;; (add-to-load-path "/usr/share/emacs/site-lisp" )
(add-to-load-path *local-elisp-dir*)

 
;; use buffer menu instead of buffer list
;; (global-set-key [(control x) (control b)] 'buffer-menu) ; removed for helm-mini
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

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

;; enable command history between sessions
;(use-package save-history
;  :ensure)

(set-activate 'calendar-longitude 21)
(set-activate 'calendar-latitude 52.15)
;; set default dictionary to polish
(set-activate 'ispell-dictionary "polish")
;; forbid server raising new frame
(set-activate 'server-raise-frame nil)


(use-package company
  :defer t
  :config
  (set-activate 'company-idle-delay 0.25)
  (set-activate 'company-minimum-prefix-length 2))

(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode))
  :init
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'company-mode))

(use-package cider
  :defer t)
  ;; :init
  ;; (add-hook 'cider-repl-mode-hook #'subword-mode)
  ;; (add-hook 'cider-repl-mode-hook #'company-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package which-key
  :init
  (which-key-mode))

;;; rust support
(add-to-list 'auto-mode-alist'("\\.rs" . rustic-mode))
(autoload 'rustic-mode (local-elisp-subpath "/rust.el") nil t)

;;(load-file (local-elisp-subpath "/rust.el"))


(load-file (local-elisp-subpath "/helm-init.el"))

)
