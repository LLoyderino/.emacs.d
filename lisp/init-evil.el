(use-package evil
  :ensure t
  ; Dependencies listed on Github
  :ensure undo-tree
  :ensure undo-fu
  ; For Evil-Collection
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  ; Enable
  :config
  (evil-mode 1))

;; Use Evil everywhere
(use-package evil-collection
  :after evil
  :ensure t
  :custom
  (evil-collection-setup-minibuffer t) ; Enable in mini-buffer
  :config
  (evil-collection-init))

(provide 'init-evil)
