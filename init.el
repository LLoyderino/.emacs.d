;; For the sake of keeping this file clean
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Increase garbage collection threshold (200MB at startup - 50MB at runtime)
(setq normal-gc-cons-threshold (* 50 1024 1024))
(setq init-gc-cons-threshold (* 200 1020 1024))
(setq gc-cons-threshold init-gc-cons-threshold)
(add-hook 'emacs-startup-hook
	  (lambda () (setq gc-cons-threshold normal-gc-cons-threshold)))

;; Activate MELPA
(require 'package)
(add-to-list 'package-archives '( "melpa" . "https://melpa.org/packages/") t)

;; Configure use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;; Disable menus
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Optimization
(require 'init-async)

;; Load graphics
(require 'init-theme)
(require 'init-telephone-line)
(require 'init-dimmer)
(require 'init-hl-todo)

;; Load keybinds
(require 'init-evil)
(require 'init-which-key)

;; Others
(require 'init-projectile)
(require 'init-magit)

;; custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Quick reload utility
(defun reload-config ()
  (interactive)
  (load-file (concat user-emacs-directory "init.el")))

(provide 'init)
