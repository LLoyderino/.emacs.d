;; I will not bother handling older version of Emacs, will just throw a warning message
(when (version< emacs-version "28.1")
  (warn "An older emacs version has been detected."))

;; On Non-Windows we can speed up the boot by avoiding double checking the alist
;; Source: https://www.gnu.org/software/emacs/manual/html_node/emacs/Choosing-Modes.html
(unless (eq system-type 'windows-nt)
  (setq auto-mode-case-fold nil))

;; Increase garbage collection threshold (200MB at startup - 50MB at runtime)
(setq normal-gc-cons-threshold (* 50 1024 1024))
(setq init-gc-cons-threshold (* 200 1020 1024))
(setq gc-cons-threshold init-gc-cons-threshold)
(add-hook 'emacs-startup-hook
	  (lambda () (setq gc-cons-threshold normal-gc-cons-threshold)))

;; For the sake of keeping this file clean
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Activate MELPA
(require 'package)
(add-to-list 'package-archives '( "melpa" . "https://melpa.org/packages/") t)
(package-refresh-contents) ; Always refresh contents...

;; Configure use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t))

(eval-when-compile
  (require 'use-package))

;; Optimization
(require 'init-async)

;; Load keybinds
(require 'init-evil)
(require 'init-which-key)
(require 'init-keybinds)

;; Others
(require 'init-projectile)
(require 'init-magit)

;; Load graphics
(require 'init-theme)
(require 'init-dashboard)
(require 'init-doom-modeline)
(require 'init-treemacs)

;; Programming & Misc
(require 'init-org)
(require 'init-restart-emacs)
(require 'init-c)

;; custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
