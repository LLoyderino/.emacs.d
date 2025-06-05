# Emacs.d

This is my emacs configuration. It aims to be *simple* and *lean*

## Notes

This setup is tested on Emacs 30.1, this means some packages are already built-in, such as:

- `use-package`
- `eglot`
- `pdf-tools` with `emacsPackages.pdf-tools`

Additionally the theme custom module assumes you are running Gnome

## Goodies to check out

### Tab management

```elisp
;; Tab history
(tab-bar-history-mode t)
(global-set-key (kbd "M-[") 'tab-bar-history-back)
(global-set-key (kbd "M-]") 'tab-bar-history-forward)
```
