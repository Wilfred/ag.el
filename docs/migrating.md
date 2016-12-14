# Migrating from 0.x to 1.x

## `k` does not kill the results buffer

Ag.el no longer kills the result buffer when pressing `k`.

**Why**: `k` is surprising to people who are used to vim keybindings,
and results buffers can be slow to recreate.

**Undoing**: You can restore the old behaviour with the following
code:

``` emacs-lisp
(define-key ag-mode-map (kbd "k") '(lambda () (interactive) 
                                     (let (kill-buffer-query-functions) (kill-buffer))))
```

**Recommendations**: Use `kill-this-buffer` (bound to `C-x k` by
default) instead. If you're killing ag results buffers frequently, you
may want to use `ag-kill-buffers` and `ag-kill-other-buffers`.

## Obsolete aliases removed

The following command aliases are no longer available:

* `ag-project-at-point`
* `ag-regexp-project-at-point`

**Why**: TODO (cluttered M-x)

## Private functions renamed

TODO
