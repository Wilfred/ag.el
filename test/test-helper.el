(require 'f)

(defvar ag-test/test-path
  (f-parent (f-this-file)))

(defvar ag-test/root-path
  (f-parent ag-test/test-path))

(require 'ert)
(require 'ag (f-expand "ag" ag-test/root-path))
