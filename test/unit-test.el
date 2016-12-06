(require 'ert)
(require 'ag)

(ert-deftest ag--escape-pcre ()
  (should (equal (ag/escape-pcre "ab.*(") "ab\\.\\*\\(")))

(ert-deftest ag-project ()
  "Smoke test for what is probably the most useful command in ag.el"
  (switch-to-buffer (ag-project "defun"))
  (should
   (equal (buffer-name)
          (format "*ag search text:defun dir:%s*"
                  (f-abbrev default-directory)))))

(ert-deftest ag--buffer-name ()
  "Ensure buffer names take the expected form."
  ;; Simple case
  (should
   (equal (ag--buffer-name "foo" "/") "*ag: / foo*"))
  ;; Abbreviate paths where possible
  (should
   (equal (ag--buffer-name "foo" (f-expand "~/bar")) "*ag: ~/bar foo*")))
