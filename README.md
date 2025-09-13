# Emacs.d

This is my emacs configuration. It aims to be *simple* and *lean*

## Requirements

### Emacs version

I run this config on Emacs 30, which means some packages are already built-in. On older
version you might require to install them separately.

Some examples include:

- flymake
- project.el
- treesitter
- use-package

### External dependencies

The following programs need to be installed externally in order for everything to work:

- [Git](https://git-scm.com)
- [Gnome](https://www.gnome.org/) - (alternatively change `theme.el` to match your DE/WM)
- [aspell](http://aspell.net/) - (alternatively just use ispell)
- [epdfinfo server](https://github.com/vedang/pdf-tools?tab=readme-ov-file#installing-the-epdfinfo-server) - Needed to run `pdf-tools`
