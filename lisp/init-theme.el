;; Font Settings
(setq frame-font-face "JetBrains Mono"
      frame-font-size "11")

;; Utility to check if a font exists
(defun font-exists-p (font)
  "Check if the font exists"
  (if (null (x-list-fonts font)) nil t))

;; Font
;; TODO implement non monospace fonts
(when (font-exists-p frame-font-face)
  (set-frame-font (concat frame-font-face " " frame-font-size) nil t))

;; Theme
(use-package dracula-theme
  :config (load-theme 'dracula t))

;; Relative line numbers
(use-package linum-relative)
(add-hook 'prog-mode-hook
	  (if (and (fboundp 'display-lines-number-mode) (display-graphic-p))
	      #'display-line-numbers-mode
	    #'linum-relative-mode))

(provide 'init-theme)
