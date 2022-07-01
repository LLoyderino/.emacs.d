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

;; Line numbers
(use-package linum-relative
  :init (linum-relative-mode))

(provide 'init-theme)
