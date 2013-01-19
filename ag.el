;;; ag.el --- A front-end for ag, the C ack replacement.

;; Copyright (C) 2012 Wilfred Hughes <me@wilfred.meuk>
;;
;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Created: 11 January 2012
;; Version: 0.8

;;; Commentary

;; This file is heavily based on the excellent ack-and-a-half.el.

;;; Usage

;; Add you to your .emacs.d:

;; (add-to-list 'load-path "/path/to/ag.el") ;; optional
;; (require 'ag)

;; I like to bind ag-project-at-point to F5:

;; (global-set-key (kbd "<f5>") 'ag-project-at-point)

;;; Todo:

;; 1. Add ag-regexp
;; 2. Add highlighting to *Ag* buffer

;;; License:

;; This file is not part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(defcustom ag-arguments
  (list "--nocolor" "--literal" "--smart-case" "--nogroup" "--column")
  "Default arguments passed to ag."
  :type '(repeat (string)))

(require 'compile)

(define-compilation-mode ag-mode "Ag"
  "Ag results compilation mode")

(defun ag/s-join (separator strings)
  "Join all the strings in STRINGS with SEPARATOR in between."
  (mapconcat 'identity strings separator))

(defun ag/s-replace (old new s)
  "Replace all occurrences of OLD in NEW in S."
  (replace-regexp-in-string (regexp-quote old) new s t t))

(defun ag/shell-quote (string)
  "Wrap in single quotes, and quote existing single quotes to make shell safe."
  (concat "'" (ag/s-replace "'" "'\\''" string) "'"))

(defun ag/search (string directory)
  "Run ag searching for the literal STRING given in DIRECTORY."
  (let ((default-directory (file-name-as-directory directory)))
    (unless (file-exists-p default-directory)
      (error "No such directory %s" default-directory))
    (compilation-start
     (ag/s-join " "
                (append '("ag") ag-arguments (list (ag/shell-quote string))))
     'ag-mode)))

(autoload 'vc-git-root "vc-git")
(autoload 'vc-svn-root "vc-svn")

(defun ag/project-root (file-path)
  "Guess the project root of the given file path."
  (or (vc-git-root file-path)
      (vc-svn-root file-path)
      file-path))

(defun ag (string directory)
  "Search using ag in a given directory for a given string."
  (interactive "sSearch string: \nDDirectory: ")
  (ag/search string directory))

(defun ag-project (string)
  "Guess the root of the current project and search it with ag
for the given string."
  (interactive "sSearch string: ")
  (ag/search string (ag/project-root (buffer-file-name))))

(autoload 'symbol-at-point "thingatpt")

(defun ag-project-at-point (string)
  "Same as ``ag-project'', but with the search string defaulting
to the symbol under point."
   (interactive (list (read-from-minibuffer "Search string: "
                                           (if (symbol-at-point)
                                               (symbol-name (symbol-at-point))))))
   (ag/search string (ag/project-root default-directory)))

(provide 'ag)
