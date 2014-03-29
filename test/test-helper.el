; Equivalent to f-parent, but we don't want to depend on f.
(defun ag-test/parent (path)
  (directory-file-name (file-name-directory path)))

(defvar ag-test/test-path
  (ag-test/parent (buffer-file-name)))

(defvar ag-test/root-path
  (ag-test/parent ag-test/test-path))

(require 'ert)
(require 'ag (directory-file-name (expand-file-name "ag" ag-test/root-path)))
