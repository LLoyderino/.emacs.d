;; Please no custom file
(setq custom-file (locate-user-emacs-file "emacs-custom.el")
(load custom-file 'noerror 'nomessage)

;; Start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Add line numbers
(setq display-line-numbers t)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Melpa
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Ensure packages by default
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Better defaults
(use-package better-defaults)
(setq read-process-output-max (* 1024 1024 4)) ; Increase garbage collection threshold

(global-auto-revert-mode 1)                    ; Update files changed externally
(setq global-auto-revert-non-file-buffers t)   ; Update dired when directory changes

;; Catppuccin theme
(load "~/.emacs.d/theme.el")

;; pdf-tools
(use-package pdf-tools)

;; Magit
(use-package magit)

;; Raindow delimiters
(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Markdown
(use-package markdown-mode)

;; Flymake
(setq flymake-show-diagnostics-at-end-of-line t)
(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)

;; Auto-completion
(use-package company)
(add-hook 'after-init-hook 'global-company-mode)

