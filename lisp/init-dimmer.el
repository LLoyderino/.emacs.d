(use-package dimmer
  :init
  (setq dimmer-fraction 0.40)
  (dimmer-configure-which-key)
  :config
  (dimmer-mode t))

(provide 'init-dimmer)
