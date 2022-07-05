;; Bind windows key as super
(when (eq system-type 'windows-nt)
  (setq w32-lwindow-modifier 'super))

;; Paste Alt + Win + V
(global-set-key (kbd "s-v") 'yank)

;; Write tilde as M-\
(global-set-key (kbd "M-\\") (lambda () (interactive) (insert "~")))

;; <SPC> Keybinds
(evil-leader/set-key "f s" 'save-buffer)
(evil-leader/set-key "p f" 'project-find-file)

;; Git - Magit
(evil-leader/set-key "g g" 'magit-status)

;; Buffers
(evil-leader/set-key "b b" 'switch-to-buffer)

(provide 'init-keybinds)
