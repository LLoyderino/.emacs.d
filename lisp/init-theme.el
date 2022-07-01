;; Utility to check if a font exists
(defun font-exists-p (font)
  "Check if the font exists"
  (if (null (x-list-fonts font)) nil t))

;; Font
;; TODO implement non monospace fonts

(when (font-exists-p "JetBrains Mono")
  (set-frame-font "JetBrains Mono 11" nil t))

;; Theme
(use-package nord-theme
  :config (load-theme 'nord t))

;; Relative line numbers
(use-package linum-relative)
(add-hook 'prog-mode-hook
	  (if (and (fboundp 'display-lines-number-mode) (display-graphic-p))
	      #'display-line-numbers-mode
	    #'linum-relative-mode))

(provide 'init-theme)
