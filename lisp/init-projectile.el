(use-package projectile
  :ensure t
  :config (projectile-mode))

;; Keybinds
(define-key projectile-mode-map (kbd "SPC p f") 'projectile-find-file)

(provide 'init-projectile)
