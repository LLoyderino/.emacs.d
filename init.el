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
(use-package vertico
  :init (vertico-mode))

(use-package better-defaults                   ; It is important to run after vertico
                                               ; or it will run completion with ido
  :after vertico)
(setq read-process-output-max (* 1024 1024 4)) ; Increase garbage collection threshold

(use-package no-littering)                     ; Move litter to separate dirs

(global-auto-revert-mode 1)                    ; Update files changed externally
(setq global-auto-revert-non-file-buffers t)   ; Update dired when directory changes

(setq calendar-week-start-day 1)               ; Start calendar on Monday

(which-key-mode)                               ; Enable which-key (didn't know this is built-in now!)

(setq scroll-conservatively 5                  ; Smooth scrolling~
      scroll-margin 5)                         ; https://themkat.net/2025/03/25/simple_smoother_emacs_scrolling.html

;; Org & Agenda
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(setq org-directory "~/Documents/Org"
      org-agenda-directory (file-name-concat org-directory "Agenda")
      org-agenda-files (list org-agenda-directory))

(setq org-html-head ""
      org-html-head-extra ""
      org-html-head-include-default-style nil
      org-html-head-include-scripts nil
      org-html-preamble nil
      org-html-postamble nil
      org-html-use-infojs nil)

(setq org-capture-templates
      `(("t" "Todo" entry (file ,(file-name-concat org-agenda-directory "Todo.org"))
         "* TODO %?\n %i\n")
        ("i" "Idea" entry (file ,(file-name-concat org-agenda-directory "Idea.org"))
         "* TODO %^{Title} :idea:\n%?\n %i\n")))

;; Undo
(use-package vundo
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)
  (vundo-compact-display t))

;; Catppuccin theme
(load (locate-user-emacs-file "theme.el"))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

;; Completion
(use-package corfu
  :commands global-corfu-mode
  :init (add-hook 'after-init-hook #'global-corfu-mode)
  :config (setq corfu-auto t))

;; Retro-active completion
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Annotations in minibuffer
(use-package marginalia
  :bind
  (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :commands marginalia-mode
  :init (add-hook 'after-init-hook #'marginalia-mode))

;; pdf-tools
(use-package pdf-tools)

;; Magit
(use-package magit)

;; Parenthesis
(add-hook 'prog-mode-hook #'electric-pair-mode)

(use-package rainbow-delimiters
  :hook (prog-mode-hook . rainbow-delimiters-mode))

;; Markdown
(use-package markdown-mode)

;; Flymake
(require 'flymake)
(setq flymake-show-diagnostics-at-end-of-line t)
(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)

;; Snippets
(use-package yasnippet
  :hook ((text-mode
          prog-mode
          conf-mode
          snippet-mode) . yas-minor-mode)
  :init (setq yas-snippet-dir (locate-user-emacs-file "snippets")))

(use-package yasnippet-snippets) ; Third party snippets, could replace
                                 ; them with my own one day...

;; Projects
(use-package rg)

(use-package projectile
  :after rg
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

;; Treesitter grammars
(setq treesit-language-source-alist
      '((php "https://github.com/tree-sitter/tree-sitter-php" "v0.23.12" "php/src")
        (phpdoc "https://github.com/claytonrcarter/tree-sitter-phpdoc" "v0.1.6")))

;; LSP configuration with Eglot for minimalism™
(use-package eglot-booster
  :vc (:url "https://github.com/jdtsmith/eglot-booster")
  :after eglot
  :config (eglot-booster-mode))

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

