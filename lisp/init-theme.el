;; Disable menus
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Font setup
(setq frame-font-face "JetBrains Mono"
      frame-font-size "11")

(defun font-exists-p (font)
  "Check if the font exists"
  (if (null (x-list-fonts font)) nil t))

;; TODO implement non monospace fonts
(when (font-exists-p frame-font-face)
  (set-frame-font (concat frame-font-face " " frame-font-size) nil t))

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

(provide 'init-theme)
