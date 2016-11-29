(require 'ert)
(require 'ag)

(ert-deftest ag--escape-pcre ()
  (should (equal (ag--escape-pcre "ab.*(") "ab\\.\\*\\(")))

(ert-deftest ag--longest-string ()
  (should
   (equal (ag--longest-string nil "foo" nil "f" "foobarbaz" "z")
          "foobarbaz")))
