;;; ag.el --- A front-end for ag ('the silver searcher'), the C ack replacement.

;; Copyright (C) 2013 Wilfred Hughes <me@wilfred.me.uk>
;;
;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Created: 11 January 2013
;; Version: 0.24

;;; Commentary:

;; This file is heavily based on the excellent ack-and-a-half.el.

;; Usage:

;; Add you to your .emacs.d:

;; (add-to-list 'load-path "/path/to/ag.el") ;; optional
;; (require 'ag)

;; Alternatively, just install the package from MELPA.

;; If you're using ag 0.14+, which supports --color-match, then you
;; can add highlighting with:

;; (setq ag-highlight-search t)

;; I like to bind the *-at-point commands to F5 and F6:

;; (global-set-key (kbd "<f5>") 'ag-project)
;; (global-set-key (kbd "<f6>") 'ag-regexp-project-at-point)

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

;;; Code:

(defcustom ag-arguments
  (list "--smart-case" "--nogroup" "--column" "--")
  "Default arguments passed to ag."
  :type '(repeat (string))
  :group 'ag)

(defcustom ag-highlight-search nil
  "Non-nil means we highlight the current search term in results.

This requires the ag command to support --color-match, which is only in v0.14+"
  :type 'boolean
  :group 'ag)

(require 'compile)

;; Although ag results aren't exactly errors, we treat them as errors
;; so `next-error' and `previous-error' work. However, we ensure our
;; face inherits from `compilation-info-face' so the results are
;; styled appropriately.
(defvar ag-hit-face compilation-info-face
  "Face name to use for ag matches.")

(defvar ag-match-face 'match
  "Face name to use for ag matches.")

(define-compilation-mode ag-mode "Ag"
  "Ag results compilation mode"
  (let ((smbl  'compilation-ag-nogroup)
        ;; Note that we want to use as tight a regexp as we can to try and
        ;; handle weird file names (with colons in them) as well as possible.
        ;; E.g. we use [1-9][0-9]* rather than [0-9]+ so as to accept ":034:"
        ;; in file names.
        (pttrn '("^\\([^:\n]+?\\):\\([1-9][0-9]*\\):\\([1-9][0-9]*\\):" 1 2 3)))
    (set (make-local-variable 'compilation-error-regexp-alist) (list smbl))
    (set (make-local-variable 'compilation-error-regexp-alist-alist) (list (cons smbl pttrn)))
    (set (make-local-variable 'compilation-error-face)
         ag-hit-face))
  (add-hook 'compilation-filter-hook 'ag-filter nil t))

(define-key ag-mode-map (kbd "p") 'compilation-previous-error)
(define-key ag-mode-map (kbd "n") 'compilation-next-error)

(defun ag/buffer-name (search-string directory regexp)
  (if regexp
      (format "*ag regexp:%s dir:%s*" search-string directory)
    (format "*ag text:%s dir:%s*" search-string directory)))

(defun ag/s-join (separator strings)
  "Join all the strings in STRINGS with SEPARATOR in between."
  (mapconcat 'identity strings separator))

(defun ag/s-replace (old new s)
  "Replace all occurrences of OLD in NEW in S."
  (replace-regexp-in-string (regexp-quote old) new s t t))

(defun ag/shell-quote (string)
  "Wrap STRING in single quotes, and quote existing single quotes to make shell safe."
  (concat "'" (ag/s-replace "'" "'\\''" string) "'"))

(defun ag/search (string directory &optional regexp)
  "Run ag searching for the STRING given in DIRECTORY.
If REGEXP is non-nil, treat STRING as a regular expression."
  (let ((default-directory (file-name-as-directory directory))
        (arguments (if regexp
                       ag-arguments
                     (cons "--literal" ag-arguments))))
    (if ag-highlight-search
        (setq arguments (append '("--color" "--color-match" "'30;43'") arguments))
      (setq arguments (append '("--nocolor") arguments)))
    (unless (file-exists-p default-directory)
      (error "No such directory %s" default-directory))
    (compilation-start
     (ag/s-join " "
                (append '("ag") arguments (list (ag/shell-quote string))))
     'ag-mode
     (lambda (mode-name) (ag/buffer-name string directory regexp)))))

(defun ag/dwim-at-point ()
  "If there's an active selection, return that.
Otherwise, get the symbol at point."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (if (symbol-at-point)
        (symbol-name (symbol-at-point)))))

(autoload 'vc-git-root "vc-git")
(autoload 'vc-svn-root "vc-svn")
(autoload 'vc-hg-root "vc-hg")

(defun ag/project-root (file-path)
  "Guess the project root of the given FILE-PATH."
  (or (vc-git-root file-path)
      (vc-svn-root file-path)
      (vc-hg-root file-path)
      file-path))

;;;###autoload
(defun ag (string directory)
  "Search using ag in a given DIRECTORY for a given search STRING,
with STRING defaulting to the symbol under point."
   (interactive (list (read-from-minibuffer "Search string: " (ag/dwim-at-point))
                      (read-directory-name "Directory: ")))
   (ag/search string directory))

;;;###autoload
(defun ag-regexp (string directory)
  "Search using ag in a given directory for a given regexp."
  (interactive "sSearch regexp: \nDDirectory: ")
  (ag/search string directory t))

;;;###autoload
(defun ag-project (string)
  "Guess the root of the current project and search it with ag
for the given string."
  (interactive (list (read-from-minibuffer "Search string: " (ag/dwim-at-point))))
  (ag/search string (ag/project-root default-directory)))

;;;###autoload
(defun ag-project-regexp (regexp)
  "Guess the root of the current project and search it with ag
for the given regexp."
  (interactive "sSearch regexp: ")
  (ag/search regexp (ag/project-root default-directory) t))

(autoload 'symbol-at-point "thingatpt")

;;;###autoload
(defalias 'ag-project-at-point 'ag-project)

;;;###autoload
(defun ag-regexp-project-at-point (regexp)
  "Same as ``ag-regexp-project'', but with the search regexp defaulting
to the symbol under point."
   (interactive (list (read-from-minibuffer "Search regexp: " (ag/dwim-at-point))))

   (ag/search regexp (ag/project-root default-directory) t))

;; Taken from grep-filter, just changed the color regex.
(defun ag-filter ()
  "Handle match highlighting escape sequences inserted by the ag process.
This function is called from `compilation-filter-hook'."
  (when ag-highlight-search
    (save-excursion
      (forward-line 0)
      (let ((end (point)) beg)
        (goto-char compilation-filter-start)
        (forward-line 0)
        (setq beg (point))
        ;; Only operate on whole lines so we don't get caught with part of an
        ;; escape sequence in one chunk and the rest in another.
        (when (< (point) end)
          (setq end (copy-marker end))
          ;; Highlight ag matches and delete marking sequences.
          (while (re-search-forward "\033\\[30;43m\\(.*?\\)\033\\[[0-9]*m" end 1)
            (replace-match (propertize (match-string 1)
                                       'face nil 'font-lock-face ag-match-face)
                           t t))
          ;; Delete all remaining escape sequences
          (goto-char beg)
          (while (re-search-forward "\033\\[[0-9;]*[mK]" end 1)
            (replace-match "" t t)))))))

(provide 'ag)
;;; ag.el ends here
