;; Start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Font
(add-to-list 'default-frame-alist
             `(font . ,(if (eq system-type 'windows-nt)
                           "JetBrainsMono NF 12"
                         "JetBrainsMono Nerd Font 12")))

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

(setq-default fill-column 94)                  ; Fill columns to half of my laptop's screen

(global-set-key (kbd "M-\\") #'just-one-space) ; Leave one space instead of killing all

;; Spellcheck
(setq ispell-program-name "aspell")
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Multiple-cursors
(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this))

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
         "* TODO %?\n %i\n")))

;; Undo
(use-package vundo
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)
  (vundo-compact-display t))

;; Catppuccin theme
(if (eq system-type 'windows-nt)
    (use-package catppuccin-theme
      :config ;; On Windows I am happy with perma dark mode
      (setq catppuccin-flavor 'mocha)
      (load-theme 'catppuccin :no-confirm))
  (load (locate-user-emacs-file "theme.el")))

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
(use-package pdf-tools
  :config
  ;; Let's face it, I'll be more likely reading PDFs than editing 😉
  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode)))

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

;; Treesitter grammars
(setq treesit-language-source-alist
      '((php "https://github.com/tree-sitter/tree-sitter-php" "v0.23.12" "php/src")
        (phpdoc "https://github.com/claytonrcarter/tree-sitter-phpdoc" "v0.1.6")))

;; PHP
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-ts-mode))
(add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))

;; Nix
(use-package nix-ts-mode
  :mode "\\.nix\\'")

;; Java
(add-to-list 'auto-mode-alist '("\\.java\\'" . java-ts-mode))

;; Kotlin
(use-package kotlin-ts-mode
  :mode "\\.kt\\'")

;; Web development
(use-package web-mode)
