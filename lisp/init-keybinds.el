;; Bind windows key as super
(when (eq system-type 'windows-nt)
  (setq w32-lwindow-modifier 'super))

;; Paste Alt + Win + V
(global-set-key (kbd "M-s-v") 'yank)

;; Write tilde as M-\
(global-set-key (kbd "M-\\") (lambda () (interactive) (insert "~")))

(provide 'init-keybinds)
