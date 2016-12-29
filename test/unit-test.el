(require 'ert)
(require 'ag)

(ert-deftest ag--escape-pcre ()
  (should (equal (ag/escape-pcre "ab.*(") "ab\\.\\*\\(")))

(ert-deftest ag--longest-string ()
  (should
   (equal (ag/longest-string nil "foo" nil "f" "foobarbaz" "z")
          "foobarbaz")))

(ert-deftest ag-project ()
  "Smoke test for what is probably the most useful command in ag.el"
  (switch-to-buffer (ag-project "defun"))
  (should
   (equal (buffer-name)
          (format "*ag search text:defun dir:%s*"
                  (f-abbrev default-directory)))))
