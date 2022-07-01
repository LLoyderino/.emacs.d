;; Font
;; TODO implement non monospace fonts
(set-frame-font "JetBrains Mono 11" nil t)

;; Theme
(use-package nord-theme
  :config (load-theme 'nord t))

;; Line numbers
(use-package linum-relative
  :init (linum-relative-mode))

(provide 'init-theme)
