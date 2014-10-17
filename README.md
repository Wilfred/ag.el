# ag.el

Ag.el allows you to search using `ag` from inside Emacs. You can
filter by file type, edit results inline, or find files.

Ag.el tries very hard to be Do-What-I-Mean, and will make intelligent
suggestions about what to search and which directories to search in.

[![screen_thumb](https://f.cloud.github.com/assets/70800/239876/738b5bd8-88d8-11e2-96a1-606e8d17d5ba.png)](https://f.cloud.github.com/assets/70800/239871/d8421a54-88d7-11e2-9cc0-df569b228888.png)

Documentation: http://agel.readthedocs.org/en/latest/index.html

Bugs: https://github.com/Wilfred/ag.el/issues

## Alternatives

* There's an ag plugin for helm: https://github.com/syohex/emacs-helm-ag

## Todo

* Remove `*-at-point` commands in favour of always defaulting to the
  symbol at point.
* Add aliases for the old command names to ensure backward
  compatibility.
* Modify `ag-regexp-project-at-point` to quote the default search
  string, otherwise `"foo.bar"` will match other things.
