;; Disable menus
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Font setup
(setq fixed-pitch-font-face "JetBrains Mono"
      variable-pitch-font-face "Source Sans Pro")

(defun font-exists-p (font)
  "Check if the font exists"
  (if (null (x-list-fonts font)) nil t))

;; TODO auto-install font if missing
(when (font-exists-p fixed-pitch-font-face)
  (set-face-attribute 'default nil
		      :font fixed-pitch-font-face
		      :height 120)
  (set-face-attribute 'fixed-pitch nil
		      :font fixed-pitch-font-face
		      :height 120))

(when (font-exists-p variable-pitch-font-face)
  (set-face-attribute 'variable-pitch nil
		      :font variable-pitch-font-face
		      :height 140))

;; Theme setup
(use-package dracula-theme
  :config (load-theme 'dracula t))

;; All the icons
(use-package all-the-icons
  :if (display-graphic-p))

;; Relative line numbers
(use-package linum-relative)
(add-hook 'prog-mode-hook
	  (if (and (fboundp 'display-lines-number-mode) (display-graphic-p))
	      #'display-line-numbers-mode
	    #'linum-relative-mode))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Highlight current line
(use-package hl-line
  :ensure nil
  :hook ((after-init . global-hl-line-mode)
	 ((dashboard-mode eshell-mode shell-mode term-mode vterm-mode) .
	  (lambda () (setq-local global-hl-line-mode nil)))))

;; Highlight keywords
(use-package hl-todo
  :init
  (setq hl-todo-keyword-faces
	'(("TODO" . "#3AB0FF")))
  :config
  (global-hl-todo-mode)
  (define-key hl-todo-mode-map (kbd "C-c p") 'hl-todo-previous)
  (define-key hl-todo-mode-map (kbd "C-c n") 'hl-todo-next)
  (define-key hl-todo-mode-map (kbd "C-c o") 'hl-todo-occur)
  (define-key hl-todo-mode-map (kbd "C-c i") 'hl-todo-insert))

;; Dimming when out of focus
(use-package dimmer
  :init
  (setq dimmer-fraction 0.40)
  (dimmer-configure-which-key)
  :config
  (dimmer-mode t))

(provide 'init-theme)
