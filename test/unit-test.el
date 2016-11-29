(require 'ert)
(require 'ag)

(ert-deftest ag--escape-pcre ()
  (should (equal (ag--escape-pcre "ab.*(") "ab\\.\\*\\(")))
