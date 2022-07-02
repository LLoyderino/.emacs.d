(use-package dashboard
  :config (dashboard-setup-startup-hook))

;; Look and feel
(setq dashboard-banner-logo-title nil)
(setq dashboard-center-content t)

;; Dashboard items
(setq dashboard-items '((recents . 5)
			(projects . 5)
			(bookmarks . 5)))

;; No thanks
(setq dashboard-set-init-info nil)
(setq dashboard-set-footer nil)

(provide 'init-dashboard)
