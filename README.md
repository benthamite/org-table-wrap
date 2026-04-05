# `org-table-wrap`: Visual word-wrapping for Org tables

Org mode tables are plain-text constructs where every cell must fit on a single line. When a table has many columns or long cell content, it overflows the window and forces horizontal scrolling. `org-table-wrap` fixes this by rendering overflowing tables with word-wrapped cells using overlays — the buffer text is never modified.

The wrapped rendering uses Unicode box-drawing characters (with an ASCII fallback) and follows the same reveal-on-enter pattern as `org-appear`: when point enters a wrapped table, the overlay is removed for normal editing; when point leaves, the overlay is re-applied. Tables are automatically re-wrapped on window resize.

Column widths are allocated proportionally based on content, with a two-phase fitting approach: first a character-based allocation, then a pixel-accurate measurement pass that handles variable-width fonts, display scaling, and `org-indent-mode` prefix overhead.

## Installation

Requires Emacs 29.1+ and Org 9.6+.

### package-vc (built-in since Emacs 30)

```emacs-lisp
(use-package org-table-wrap
  :vc (:url "https://github.com/benthamite/org-table-wrap"))
```

### Elpaca

```emacs-lisp
(use-package org-table-wrap
  :ensure (:host github :repo "benthamite/org-table-wrap"))
```

### straight.el

```emacs-lisp
(use-package org-table-wrap
  :straight (:host github :repo "benthamite/org-table-wrap"))
```

## Quick start

Enable `org-table-wrap` globally for all Org buffers:

```emacs-lisp
(global-org-table-wrap-mode 1)
```

Or enable it per-buffer:

```emacs-lisp
(add-hook 'org-mode-hook #'org-table-wrap-mode)
```

Open any Org file with a wide table and it will be automatically wrapped to fit the window. Move point into the table to edit it normally.

## Documentation

For a comprehensive description of all user options, commands, and functions, see the [manual](README.org).

## License

`org-table-wrap` is licensed under the GPL-3.0. See [LICENSE](LICENSE) for details.
