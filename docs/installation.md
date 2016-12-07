# Installation

**Note**: In all these docs, **ag.el** always refers to the Emacs package,
and **ag** refers to the search program.

## Requirements

ag.el requires Emacs 24.4+, and works on Linux, OS X and Windows.

## Install ag

You will need the `ag` program installed.

If you're on **Linux**, `ag` is probably available on your distro
already.

```bash
# Debian, Ubuntu
$ apt-get install silversearcher-ag
# Fedora 21 and lower
$ yum install the_silver_searcher
# Fedora 22+
$ dnf install the_silver_searcher
# RHEL 7+
$ yum install epel-release.noarch the_silver_searcher
# Gentoo
$ emerge the_silver_searcher
# Arch
$ pacman -S the_silver_searcher
# Slackware
$ sbopkg -i the_silver_searcher
```

If you're on **OS X**, you can use [Homebrew](http://brew.sh/) or
[MacPorts](https://www.macports.org/).

``` bash
# Homebrew
$ brew install the_silver_searcher
# MacPorts
$ port install the_silver_searcher
```

If you're on **Windows**, there's
[an ag package](https://chocolatey.org/packages/ag) available through
the [chocolatey package manager](https://chocolatey.org/).

```bash
$ choco install ag
```

Krzysztof Kowalczyk also
[provides Windows binaries](http://blog.kowalczyk.info/software/the-silver-searcher-for-windows.html).

If you're on **Cygwin**, there's a package `the_silver_searcher` in
the 'Utils' category of the main Cygwin distribution.

Ag itself also provides
[installation instructions](https://github.com/ggreer/the_silver_searcher#installing).

## Install ag.el

You can install ag.el from [MELPA](http://melpa.org/).

If you haven't used MELPA before, you will need to configure Emacs to
download packages from it. Add the following lines to your `init.el`.

``` lisp
(require 'package)
;; Note that there are known issues with package.el and HTTPS on Windows.
;; If installation fails, try using HTTP.
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize) ;; You might already have this line
```

You can now install ag.el with `M-x list-packages` followd by `M-x
package-install ag`.

You're now ready! Head on to
[getting started](getting_started.md) to start searching!
