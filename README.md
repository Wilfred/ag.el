# ag.el

A simple ag.el frontend, loosely based on ack-and-half.el.

## FAQ

### ag.el can't find the ag executable

Install the [exec-path-from-shell](https://github.com/purcell/exec-path-from-shell) package
from [Marmalade](http://marmalade-repo.org/) or [MELPA](http://melpa.milkbox.net/), or try
putting the following code in your Emacs configuration:

    (defun set-exec-path-from-shell-PATH ()
      "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

    This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
      (interactive)
      (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
        (setenv "PATH" path-from-shell)
        (setq exec-path (split-string path-from-shell path-separator))))

    (set-exec-path-from-shell-PATH)

## Changelog

### 0.15

Fixed `ag-project` and `ag-project-regexp` not working in buffers that
aren't associated with a specific file, such as dired and magit buffers.

### 0.14

The compilation mode regexp is now more accurate, so you should no
longer get 'compilation-next-error: No error here' when trying to open
a file in the results list.

### 0.13

Current stable ag (0.13.1) doesn't support `--color-match`, ag.el now
only highlights when `ag-highlight-search` is non-nil (the default is nil).

If you're upgrading ag.el and your ag version is 0.14 or higher, you
need to explicitly enable highlighting:

    (setq ag-highlight-search t)
