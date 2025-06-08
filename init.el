;; Please no custom file
(setq custom-file (locate-user-emacs-file "emacs-custom.el"))
(load custom-file 'noerror 'nomessage)

;; Start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Font
(add-to-list 'default-frame-alist
             '(font . "JetBrainsMono Nerd Font 12"))

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

(setq calendar-week-start-day 1)               ; Start calendar on Monday

;; Catppuccin theme
(load (locate-user-emacs-file "theme.el"))

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
(require 'flymake)
(setq flymake-show-diagnostics-at-end-of-line t)
(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)

;; Auto-completion
(use-package company)
(add-hook 'after-init-hook 'global-company-mode)

;; Nix
(use-package nix-ts-mode
  :mode "\\.nix\\'")

;; Web development
(use-package web-mode)

;; Angular
(define-derived-mode angular-mode web-mode "angular")

;; TODO: automatic ngserver path evaluation
;; (with-eval-after-load 'eglot
;;   (let ((ngserver-path (executable-find "ngserver")))
;;     (when ngserver-path
;;       (add-to-list 'eglot-server-programs
;;                    '(angular-mode . ("ngserver"
;;                                      "--stdio"
;;                                      "--tsProbeLocations"
;;                                      "./node_modules/typescript/lib"
;;                                      "--ngProbeLocations"
;;                                      ngserver-path))))))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(angular-mode . ("ngserver"
                                 "--stdio"
                                 "--tsProbeLocations"
                                 "./node_modules/typescript/lib"
                                 "--ngProbeLocations"
                                 "/etc/profiles/per-user/lloyd/bin/ngserver"))))


(add-hook 'angular-mode-hook 'eglot-ensure)

;; ;; Astro (depends on web-mode)
;; (define-derived-mode astro-mode web-mode "astro")
;; (setq auto-mode-alist
;;       (append '((".*\\.astro\\'" . astro-mode))
;;               auto-mode-alist))

;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                '(astro-mode . ("astro-ls" "--stdio"
;;                                :initializationOptions
;;                                (:typescript (:tsdk "./node_modules/typescript/lib"))))))

;; (add-hook 'astro-mode-hook 'eglot-ensure)

