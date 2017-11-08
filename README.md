# add-node-modules-path

[![MELPA](http://melpa.org/packages/add-node-modules-path-badge.svg)](http://melpa.org/#/add-node-modules-path)

This file provides `add-node-modules-path`, which searches
the current files parent directories for the `node_modules/.bin/` directory
and adds it to the buffer local `exec-path`.
This allows Emacs to find project based installs of e.g. eslint.

## Usage
`M-x add-node-modules-path`

To automatically run it when opening a new buffer:
(Choose depending on your favorite mode.)

```
(eval-after-load 'js-mode
  '(add-hook 'js-mode-hook #'add-node-modules-path))
```
