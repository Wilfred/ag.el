# Migrating from 0.x to 1.x

## Dropped wgrep support

TODO

## `k` does not kill the results buffer

Ag.el no longer kills the result buffer when pressing `k`.

**Why**: `k` is surprising to people who are used to vim keybindings,
and results buffers can be slow to recreate.

**Undoing**: You can restore the old behaviour with the following
code:

```lisp
(define-key ag-mode-map (kbd "k")
  '(lambda () (interactive)
     (let (kill-buffer-query-functions) (kill-buffer))))
```

**Recommendations**: Use `kill-this-buffer` (bound to `C-x k` by
default) instead. If you're killing ag results buffers frequently, you
may want to use `ag-kill-buffers` and `ag-kill-other-buffers`.

## Obsolete aliases removed

The following command aliases are no longer available:

* `ag-project-at-point` (use `ag-project`)
* `ag-regexp-project-at-point` (use `ag-project-regexp`)

**Why**: Extra commands were confusing for new users who typed `M-x
ag-<TAB>`. These were purely aliases and offered no additional
functionality.

**Recommendations**: If your configuration doesn't reference the old
aliases, you don't need to do anything. If you are using the aliases,
just do a find and replace. For example:

``` lisp
;; Before
(global-set-key (kbd "<f11>") #'ag-project-at-point)
(global-set-key (kbd "<f12>") #'ag-regexp-project-at-point)

;; After
(global-set-key (kbd "<f11>") #'ag-project)
(global-set-key (kbd "<f12>") #'ag-project-regexp)
```

## Private symbols renamed or removed

ag.el v0.x previously used an `ag/foo` naming convention for private functions and
variables. In v1.x, we use `ag--foo`.

Many internal functions have been removed, replaced, or have different
calling conventions.

**Why**: Ag.el had reached the limits of what was possible with
compilation-mode, so it now defines its own results buffer logic.

**Recommendations**: If you depend on a private function or variable,
please file a bug so we can add a public API that meets your needs.
