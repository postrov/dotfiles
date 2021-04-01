(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))

;; try setting variable via appropriate custom-set function
;; fallback to set-default
(defun set-activate (variable value)
  "Set VARIABLE to VALUE.  If there is a set-function, call it."
  (custom-load-symbol variable)
  (funcall (or (get variable 'custom-set) 'set-default) variable value))

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

(setq inhibit-startup-message t)

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

