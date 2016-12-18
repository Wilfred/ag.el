# Contributing

## Filing Bugs

Please do. You can
[file bugs on GitHub](https://github.com/wilfred/ag.el/issues).

When reporting issues, please include:

* Your version of Emacs
* Your version of ag
* Your platform (e.g. Windows 10)
* The output of the `*ag debug* buffer`

## Contributing Code

Pull requests are welcome, but if you're unsure, open an issue to
discuss changes.

Bug fixes are always welcome. ag.el prioritises not breaking users'
workflows, so behaviour changes will generally need to be configurable
and opt-in.

## Code Conventions

ag.el uses `ag-foo` for public variables and functions, and `ag--foo`
for private variables and functions.

## Building The Docs

This documentation is written using
[gitbook](https://www.gitbook.com/), which has
[its own installation instructions](https://toolchain.gitbook.com/setup.html). You
can install gitbook and build the docs lcoally as follows:

``` bash
$ npm config set prefix=$HOME/.npm-packages
$ npm install -g gitbook-cli
$ ~/.npm-packages/bin/gitbook install
$ ~/.npm-packages/bin/gitbook serve
```
