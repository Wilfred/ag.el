# ag.el

A simple ag frontend, loosely based on ack-and-half.el.

## Screenshot

[![screen_thumb](https://f.cloud.github.com/assets/70800/239876/738b5bd8-88d8-11e2-96a1-606e8d17d5ba.png)](https://f.cloud.github.com/assets/70800/239871/d8421a54-88d7-11e2-9cc0-df569b228888.png)

## Configuration

### Highlighting results

ag.el supports highlighting results for ag 0.14 or later. Previous
versions of ag don't support the `--color-match` argument.

If your version of ag is recent enough, you can add highlighting by
adding the following to your Emacs configuration:

    (setq ag-highlight-search t)

### Path to the ag executable

ag.el assumes that the ag executable is in one of the directories on
`exec-path`. Generally, this is sufficient.

However, you may find that you can run ag in a terminal but ag.el
isn't finding the ag executable. This is common on Mac OS X. You'll
need to update `exec-path` to match your terminal. The best way to do
this is to install
[exec-path-from-shell](https://github.com/purcell/exec-path-from-shell)
(available on both [Marmalade](http://marmalade-repo.org/) and
[MELPA](http://melpa.milkbox.net/)).

Alternatively, you can do this yourself by putting the following code
in your Emacs configuration:

    (defun set-exec-path-from-shell-PATH ()
      "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

    This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
      (interactive)
      (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
        (setenv "PATH" path-from-shell)
        (setq exec-path (split-string path-from-shell path-separator))))

    (set-exec-path-from-shell-PATH)

## Changelog

### 0.18

Search results are now highlighted as information, rather than
errors. The ag output is now more consistent with grep.el.

### 0.17

The interactive functions provided by ag.el are now autoloaded.

### 0.16

Removed the unused variable `ag-last-buffer`

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

## Alternatives

* There's an ag plugin for helm: https://github.com/syohex/emacs-helm-ag
