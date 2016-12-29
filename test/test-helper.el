;;; test-helper.el --- Helper for tests              -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Wilfred Hughes

;; Author: Wilfred Hughes <me@wilfred.me.uk>

;;; Code:

(require 'ert)
(require 'f)

(let ((ag-el-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path ag-el-dir))

(require 'undercover)
(undercover "ag.el"
	    (:exclude "*-test.el")
	    (:report-file "/tmp/undercover-report.json"))

(provide 'test-helper)
;;; test-helper.el ends here
