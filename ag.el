;;; ag.el --- A front-end for ag ('the silver searcher'), the C ack replacement.

;; Copyright (C) 2013 Wilfred Hughes <me@wilfred.me.uk>
;;
;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Created: 11 January 2013
;; Version: 0.32

;;; Commentary:

;; Please see README.md for documentation, or read in online at
;; https://github.com/Wilfred/ag.el/#agel

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
(eval-when-compile (require 'cl)) ;; dolist, flet

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

(defcustom ag-reuse-buffers nil
  "Non-nil means we reuse the existing search results buffer, rather than
creating one buffer per unique search."
  :type 'boolean
  :group 'ag)

(defcustom ag-reuse-window nil
  "Non-nil means we open search results in the same window,
hiding the results buffer."
  :type 'boolean
  :group 'ag)

(defcustom ag-project-root-function nil
  "A function to determine the project root for `ag-project'.

If set to a function, call this function with the name of the
file or directory for which to determine the project root
directory.

If set to nil, fall back to finding VCS root directories."
  :type '(choice (const :tag "Default (VCS root)" nil)
                 (function :tag "Function"))
  :group 'ag)

(require 'compile)

;; Although ag results aren't exactly errors, we treat them as errors
;; so `next-error' and `previous-error' work. However, we ensure our
;; face inherits from `compilation-info-face' so the results are
;; styled appropriately.
(defface ag-hit-face '((t :inherit compilation-info))
  "Face name to use for ag matches."
  :group 'ag)

(defface ag-match-face '((t :inherit match))
  "Face name to use for ag matches."
  :group 'ag)

(defun ag/next-error-function (n &optional reset)
  "Open the search result at point in the current window or a
different window, according to `ag-open-in-other-window'."
  (if ag-reuse-window
      ;; prevent changing the window
      (flet ((pop-to-buffer (buffer &rest args)
                            (switch-to-buffer buffer)))
        (compilation-next-error-function n reset))
    ;; just navigate to the results as normal
    (compilation-next-error-function n reset)))

(define-compilation-mode ag-mode "Ag"
  "Ag results compilation mode"
  (let ((smbl  'compilation-ag-nogroup)
        ;; Note that we want to use as tight a regexp as we can to try and
        ;; handle weird file names (with colons in them) as well as possible.
        ;; E.g. we use [1-9][0-9]* rather than [0-9]+ so as to accept ":034:"
        ;; in file names.
        (pttrn '("^\\([^:\n]+?\\):\\([1-9][0-9]*\\):\\([1-9][0-9]*\\):" 1 2 3)))
    (set (make-local-variable 'compilation-error-regexp-alist) (list smbl))
    (set (make-local-variable 'compilation-error-regexp-alist-alist) (list (cons smbl pttrn))))
  (set (make-local-variable 'compilation-error-face) 'ag-hit-face)
  (set (make-local-variable 'next-error-function) 'ag/next-error-function)
  (add-hook 'compilation-filter-hook 'ag-filter nil t))

(define-key ag-mode-map (kbd "p") 'compilation-previous-error)
(define-key ag-mode-map (kbd "n") 'compilation-next-error)

(defun ag/buffer-name (search-string directory regexp)
  (cond
   (ag-reuse-buffers "*ag*")
   (regexp (format "*ag regexp:%s dir:%s*" search-string directory))
   (:else (format "*ag text:%s dir:%s*" search-string directory))))

(defun ag/search (string directory &optional regexp)
  "Run ag searching for the STRING given in DIRECTORY.
If REGEXP is non-nil, treat STRING as a regular expression."
  (let ((default-directory (file-name-as-directory directory))
        (arguments (if regexp
                       ag-arguments
                     (cons "--literal" ag-arguments))))
    (if ag-highlight-search
        (setq arguments (append '("--color" "--color-match" "30;43") arguments))
      (setq arguments (append '("--nocolor") arguments)))
    (unless (file-exists-p default-directory)
      (error "No such directory %s" default-directory))
    (compilation-start
     (mapconcat 'shell-quote-argument
                (append '("ag") arguments (list string))
                " ")
     'ag-mode
     `(lambda (mode-name) ,(ag/buffer-name string directory regexp)))))

(defun ag/dwim-at-point ()
  "If there's an active selection, return that.
Otherwise, get the symbol at point."
  (cond ((use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end)))
        ((symbol-at-point)
         (substring-no-properties
          (symbol-name (symbol-at-point))))))

(defun ag/longest-string (&rest strings)
  "Given a list of strings and nils, return the longest string."
  (let ((longest-string nil))
    (dolist (string strings)
      (cond ((null longest-string)
             (setq longest-string string))
            ((stringp string)
             (when (< (length longest-string)
                      (length string))
               (setq longest-string string)))))
    longest-string))

(autoload 'vc-git-root "vc-git")
(autoload 'vc-svn-root "vc-svn")
(autoload 'vc-hg-root "vc-hg")

(defun ag/project-root (file-path)
  "Guess the project root of the given FILE-PATH.

Use `ag-project-root-function' if set, or fall back to VCS
roots."
  (if ag-project-root-function
      (funcall ag-project-root-function file-path)
    (or (ag/longest-string
       (vc-git-root file-path)
       (vc-svn-root file-path)
       (vc-hg-root file-path))
      file-path)))

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

;;;###autoload
(defun ag-kill-buffers ()
  "Kill all ag-mode buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (eq (buffer-local-value 'major-mode buffer) 'ag-mode)
      (kill-buffer buffer))))

;;;###autoload
(defun ag-kill-other-buffers ()
  "Kill all ag-mode buffers other than the current buffer."
  (interactive)
  (let ((current-buffer (current-buffer)))
    (dolist (buffer (buffer-list))
      (when (and
             (eq (buffer-local-value 'major-mode buffer) 'ag-mode)
             (not (eq buffer current-buffer)))
        (kill-buffer buffer)))))

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
                                       'face nil 'font-lock-face 'ag-match-face)
                           t t))
          ;; Delete all remaining escape sequences
          (goto-char beg)
          (while (re-search-forward "\033\\[[0-9;]*[mK]" end 1)
            (replace-match "" t t)))))))

(provide 'ag)
;;; ag.el ends here
