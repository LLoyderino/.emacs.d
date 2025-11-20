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

;; Visualize whitespace for programming modes
;; Yoinked from Rexim (Tsoding)
;; https://github.com/rexim/dotfiles/blob/a96479ac248e8d1b3ad307fdd667eb4593eec56d/.emacs.custom.el#L31
(setq whitespace-style
      '(face
        tabs
        spaces
        trailing
        space-before-tab
        newline
        indentation
        empty
        space-after-tab
        space-mark
        tab-mark))
(add-hook 'prog-mode-hook 'whitespace-mode)

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

(setq-default fill-column 94)                  ; Fill columns to half of my laptop's screen

(global-set-key (kbd "M-\\") #'just-one-space) ; Leave one space instead of killing all

;; Spellcheck
(unless (eq system-type 'windows-nt)
  (setq ispell-program-name "aspell")
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

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

;; Roam
(use-package org-roam
  :custom
  (org-roam-directory (file-truename (if (eq system-type 'windows-nt)
                                         "D:/Documents/Org/Roam"
                                       "~/Documents/Org/Roam/")))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture))
  :config
  (org-roam-db-autosync-mode))

;; Mail
(load (locate-user-emacs-file "secrets.el"))                  ; Full-name and Mail address
(setq gnus-select-method '(nnimap "imap.gmail.com")
      message-send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server "smtp.gmail.com"
      gnus-message-archive-method '(nnimap "imap.gmail.com")
      gnus-message-archive-group "[Gmail]/Sent Mail")         ; Gmail configuration

;; Undo
(use-package vundo
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)
  (vundo-compact-display t))

;; Catppuccin theme
(load (locate-user-emacs-file "theme.el"))

;; Completion
(use-package corfu
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

;; Enable indentation+completion using the TAB key.
;; `completion-at-point' is often bound to M-TAB.
(setq tab-always-indent 'complete)

(use-package cape
  :bind ("C-c p" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-dict))

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
      '((css "https://github.com/tree-sitter/tree-sitter-css" "v0.23.2")
        (html "https://github.com/tree-sitter/tree-sitter-html" "v0.23.2")
        (java "https://github.com/tree-sitter/tree-sitter-java" "v0.23.5")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "v0.25.0")
        (jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc" "v0.25.0")
        (nix "https://github.com/nix-community/tree-sitter-nix" "v0.3.0")
        (php "https://github.com/tree-sitter/tree-sitter-php" "v0.23.12" "php/src")
        (phpdoc "https://github.com/claytonrcarter/tree-sitter-phpdoc" "v0.1.6")
        (python "https://github.com/tree-sitter/tree-sitter-python" "v0.25.0")))

;; PHP
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-ts-mode))
(add-to-list 'auto-mode-alist '("^artisan$" . php-ts-mode))
(add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))

;; Nix
(use-package nix-ts-mode
  :mode "\\.nix\\'")

;; Java
(add-to-list 'auto-mode-alist '("\\.java\\'" . java-ts-mode))

;; Web development
(use-package web-mode)
