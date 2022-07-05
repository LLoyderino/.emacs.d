;; Bind windows key as super
(when (eq system-type 'windows-nt)
  (setq w32-lwindow-modifier 'super))

;; Paste Alt + Win + V
(global-set-key (kbd "M-s-v") 'yank)

;; Write tilde as M-\
(global-set-key (kbd "M-\\") (lambda () (interactive) (insert "~")))

;; <SPC> Keybinds
(evil-leader/set-key "f s" 'save-buffer)
(evil-leader/set-key "p f" 'project-find-file)
(evil-leader/set-key "g" 'magit-status)

(provide 'init-keybinds)
