;; Removing emphasis markers
(setq org-hide-emphasis-markers t)

;; Org graphic improvements
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Variable font by default
(add-hook 'org-mode-hook 'variable-pitch-mode)

;; TODO add customized org-sections fonts etc
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil  :inherit 'fixed-pitch)

(provide 'init-org)
