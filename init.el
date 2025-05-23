;; Please no custom file
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)

;; Start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Add line numbers
(setq display-line-numbers t)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Remove clutter
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

;; Melpa
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Ensure packages by default
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Magit
(use-package magit)

