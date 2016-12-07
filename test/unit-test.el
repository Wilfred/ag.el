(require 'ert)
(require 'ag)
(require 'f)

(ert-deftest ag--escape-pcre ()
  (should (equal (ag--escape-pcre "ab.*(") "ab\\.\\*\\(")))

(ert-deftest ag-project ()
  "Smoke test for what is probably the most useful command in ag.el"
  (ag-project "defun")
  (should
   (equal (buffer-name)
          (format "*ag: defun %s*" (f-abbrev default-directory)))))

(ert-deftest ag--buffer-name ()
  "Ensure buffer names take the expected form."
  ;; Simple case
  (should
   (equal (ag--buffer-name "foo" "/") "*ag: foo /*"))
  ;; Abbreviate paths where possible
  (should
   (equal (ag--buffer-name "foo" (f-expand "~/bar")) "*ag: foo ~/bar*")))

(ert-deftest ag--pluralize ()
  (should
   (equal (ag--pluralize 1 "thing") "1 thing"))
  (should
   (equal (ag--pluralize 1000 "thing") "1,000 things")))

(ert-deftest ag--insert-results-heading ()
  (with-current-buffer (ag--create-results-buffer "bananas" "ag -- foo" "/")
    (ag--insert-results-heading (current-buffer))
    (should
     (s-starts-with? "Search term: bananas\nCommand:     ag -- foo"
                     (buffer-string)))))

