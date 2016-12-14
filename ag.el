;;; ag.el --- Fast and beautiful text search using ag.  -*- lexical-binding: t -*-

;; Copyright (C) 2013-2014 Wilfred Hughes <me@wilfred.me.uk>
;;
;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Created: 11 January 2013
;; Version: 1.0
;; Package-Requires: ((dash "2.8.0") (s "1.9.0") (cl-lib "0.5") (projectile "0.14.0") (spinner "1.7.3") (f "0.18.2"))
;;; Commentary:

;; Please see README.md for documentation, or read it online at
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
(require 'cl-lib) ;; cl-defun
(require 'dired) ;; dired-sort-inhibit
(require 'dash)
(require 'f)
(require 's)
(require 'spinner)
(autoload 'find-dired-filter "find-dired")
(autoload 'projectile-project-root "projectile")

(defgroup ag nil
  "A front-end for ag - The Silver Searcher."
  :group 'tools
  :group 'matching)

(defcustom ag-executable
  "ag"
  "Name of the ag executable to use."
  :type 'string
  :group 'ag)

(defcustom ag-arguments
  (list "--line-number" "--smart-case" "--nogroup" "--column" "--")
  "Default arguments passed to ag.

Ag.el internally uses --column, --line-number and --color
options (with specific colors) to match groups, so options
specified here should not conflict.

--line-number is required on Windows, as otherwise ag will not
print line numbers when the input is a stream."
  :type '(repeat (string))
  :group 'ag)

(defcustom ag-context-lines nil
  "Number of context lines to include before and after a matching line."
  :type 'integer
  :group 'ag)

(defcustom ag-reuse-buffers nil
  "Non-nil means we reuse the existing search results buffer or
dired results buffer, rather than creating one buffer per unique
search."
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

If set to nil, fall back to `projectile-project-root'."
  :type '(choice (const :tag "Default (VCS root)" nil)
                 (function :tag "Function"))
  :group 'ag)

(defcustom ag-ignore-list nil
  "A list of patterns for files/directories to ignore when searching."
  :type '(repeat (string))
  :group 'ag)

;; TODO: rename to ag-search-term-face
(defface ag-match-face
  '((((class color) (background light)) :foreground "DarkGoldenrod4" :weight bold)
    (((class color) (background  dark)) :foreground "LightGoldenrod2" :weight bold))
  "Face for ag matches."
  :group 'ag)

(defvar ag--debug-buf
  (get-buffer-create "*ag debug*"))

(defun ag--find-file (button)
  "Open the file referenced by BUTTON."
  (find-file (button-get button 'path))
  (goto-char (point-min)))

(define-button-type 'ag-path-button
  'action #'ag--find-file
  'follow-link t
  'help-echo "Open file or directory")

(defun ag--path-button (path)
  "Return a button that navigates to PATH."
  (with-temp-buffer
    (insert-text-button
     (f-abbrev path)
     :type 'ag-path-button
     'path path)
    (buffer-string)))

(defun ag--open-debug (_button)
  "Open the file referenced by BUTTON."
  (switch-to-buffer ag--debug-buf)
  (goto-char (point-min)))

(define-button-type 'ag-debug-button
  'action #'ag--open-debug
  'follow-link t
  'help-echo "Open debug buffer")

(defun ag--debug-button (command)
  "Return a button that opens the debug buffer."
  (with-temp-buffer
    (insert-text-button command :type 'ag-debug-button)
    (buffer-string)))

(defvar ag-search-finished-hook nil
  "Hook run when ag completes a search in a buffer.")

(defface ag-dim-face
  '((((class color) (background light))
     :foreground "grey50")
    (((class color) (background dark))
     :foreground "grey50"))
  "Face for metadata in ag results buffers."
  :group 'ag)

(defvar-local ag--redraw-timer nil)

(defvar-local ag--command nil)

(defvar-local ag--search-term nil)
(defvar-local ag--literal-search nil)

(defvar-local ag--remaining-output nil
  "We can't guarantee that our process filter will always receive whole lines.
We save the last line here, in case we need to append more text to it.")

(defvar-local ag--line-match-total nil)
(defvar-local ag--file-match-total nil)
(defvar-local ag--last-file-name nil
  "TODO: docme")

(defvar-local ag--spinner nil)

;; TODO:
;; * Handle errors gracefully, without confusing them with a zero-result exit code.
(cl-defun ag--start-search (search-term directory &key (literal t))
  "Initiate an ag search for SEARCH-TERM in ROOT-DIRECTORY.
If LITERAL is nil, treat SEARCH-TERM as a regular expression."
  (let* ((command (ag--format-command search-term directory :regexp (not literal)))
         (results-buffer (ag--create-results-buffer search-term command directory))
         process)
    (with-current-buffer ag--debug-buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq default-directory directory)
        (insert "$ " command "\n"))
      (setq buffer-read-only t))
    (with-current-buffer results-buffer
      (setq ag--literal-search literal)
      (ag--start-spinner)
      ;; TODO: handle error when buffer has been killed.
      (setq ag--redraw-timer
            (run-with-timer
             0 1
             #'ag--insert-results-heading results-buffer)))

    (let ((default-directory directory))
      (setq process (start-process-shell-command
                     (format "ag %s %s" search-term directory)
                     results-buffer command)))
    (set-process-filter process #'ag--process-filter)
    (set-process-sentinel process #'ag--process-sentinel)

    (switch-to-buffer results-buffer)))

(defun ag--propertize-match (line-number content-line file-name)
  (let* ((dim-number (s-pad-right 3 " " (propertize (format "%s" line-number) 'face 'ag-dim-face))))
    (propertize (format "%s %s" dim-number content-line)
                'mouse-face 'highlight
                'ag-file-name file-name
                'ag-line-number line-number)))

(defun ag--process-filter (process output)
  "Insert OUTPUT into the ag search buffer associated with PROCESS."
  (with-current-buffer ag--debug-buf
    (goto-char (point-max))
    (let ((inhibit-read-only t))
      (insert "\n--- ag--process-filter called ---\n"
              output)))
  (with-current-buffer (process-buffer process)
    ;; ag--remaining-output may contain a partial line from the last
    ;; time we were called, so append.
    (setq output (concat ag--remaining-output output))
    ;; TODO: use with-silent-modifications instead here.
    (let ((inhibit-read-only t)
          (lines (s-lines output)))
      ;; We don't want to count the last line, as it may be a partial line.
      (cl-incf ag--line-match-total (1- (length lines)))
      (setq ag--remaining-output (-last-item lines))

      (save-excursion
        (goto-char (point-max))
        (dolist (line (-butlast lines))

          (-let [(file-name line-number _column-number content-line)
                 (ag--parse-output-line line)]
            (unless (equal file-name ag--last-file-name)
              (insert "\n" (ag--path-button file-name) "\n")
              (setq ag--last-file-name file-name)
              (cl-incf ag--file-match-total))

            (insert
             (ag--propertize-match line-number content-line file-name)
             "\n")))))))

(defun ag--process-sentinel (process string)
  "Update the ag buffer associated with PROCESS as complete."
  (let ((buffer (process-buffer process)))
    ;; We assume that all signals from the ag process mean we're done.
    (cancel-timer ag--redraw-timer)
    (spinner-stop ag--spinner)

    (with-current-buffer ag--debug-buf
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert "\n--- ag--process-sentinel called ---\n"
                string)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        ;; The remaining output must now be a completed line.
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (point-max))
            (insert ag--remaining-output)
            (insert "\n")))

        (ag--insert-results-heading buffer)

        (run-hooks 'ag-search-finished-hook)))))

(defun ag--format-int (integer)
  "Format INTEGER as a string, with , separating thousands."
  (let* ((number (abs integer))
         (parts nil))
    (while (> number 999)
      (push (format "%03d" (mod number 1000))
            parts)
      (setq number (/ number 1000)))
    (push (format "%d" number) parts)
    (concat
     (if (< integer 0) "-" "")
     (s-join "," parts))))

(defun ag--pluralize (number unit)
  "Return UNIT formatted for NUMBER instances."
  (format "%s %s%s"
          (ag--format-int number)
          unit
          (if (= number 1) "" "s")))

(defun ag--insert-results-heading (buffer)
  "Insert or update an ag results heading in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (save-excursion
          ;; If there's already a heading, replace it.
          (when (> (point-max) 1)
            (goto-char (point-min))
            (while (not (looking-at "\n"))
              (forward-line 1))
            (delete-region (point-min) (point)))

          (insert
           (ag--heading-line
            "Search term"
            (format "%s %s"
                    (propertize ag--search-term 'face 'ag-match-face)
                    (if ag--literal-search "(literal string)"
                      "(regular expression, PCRE syntax)")))
           (ag--heading-line "Command"
                             (ag--debug-button ag--command))
           (ag--heading-line "Directory"
                             (ag--path-button default-directory))
           (ag--heading-line "Matches"
                             (format "%s in %s"
                                     (ag--pluralize ag--line-match-total "hit")
                                     (ag--pluralize ag--file-match-total "file")))))))))

(defconst ag--heading-label-width 13)

(defun ag--heading-line (label text)
  "Return an aligned string for a heading."
  (concat
   (s-pad-right ag--heading-label-width " " (format "%s:" label))
   text "\n"))

(defun ag--wipe-buffer (buffer)
  "Delete the contents of BUFFER."
  ;; TODO: stop an associated process.
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (widen)
      (delete-region (point-min) (point-max)))))

(defun ag--create-results-buffer (search-term command directory)
  "Create a buffer for showing ag search results."
  (let* ((buffer-name (ag--buffer-name search-term directory))
         (buffer (get-buffer-create buffer-name)))
    (ag--wipe-buffer buffer)
    
    (with-current-buffer buffer
      (setq buffer-read-only t)
      (setq default-directory directory)
      (ag-mode)
      (setq ag--command command)
      (setq ag--search-term search-term)
      (setq ag--remaining-output "")
      (setq ag--line-match-total 0)
      (setq ag--file-match-total 0))
    buffer))

(defun ag--goto-result ()
  "Goto the search result at point."
  (interactive)
  (let ((file-name (get-text-property (point) 'ag-file-name))
        (line-number (get-text-property (point) 'ag-line-number))
        ;; TODO: factor out this constant
        (column-offset (max (- (current-column) 4) 0)))
    (when file-name
      (find-file file-name)
      (goto-char (point-min))
      (forward-line (1- line-number))
      (forward-char column-offset))))

(defun ag--start-spinner ()
  "Create and start a spinner on this buffer."
  (unless ag--spinner
    (setq ag--spinner (spinner-create 'progress-bar t)))
  (spinner-start ag--spinner))

;; TODO: lines should be truncated by default in this mode.
(define-derived-mode ag-mode fundamental-mode
  '("Ag" (:eval (spinner-print ag--spinner)))
  "Mode for ag results buffers.")

(define-key ag-mode-map (kbd "RET") #'ag--goto-result)
(define-key ag-mode-map (kbd "<mouse-2>") #'ag--goto-result)

(define-key ag-mode-map (kbd "p") #'ag-prev-result)
(define-key ag-mode-map (kbd "n") #'ag-next-result)
(define-key ag-mode-map (kbd "k") '(lambda () (interactive) 
                                     (let (kill-buffer-query-functions) (kill-buffer))))
(define-key ag-mode-map (kbd "g") #'ag-rerun)

(defun ag--buffer-name (search-term directory)
  "Return a buffer name formatted according to ag.el conventions."
  (setq directory (f-abbrev directory))
  (if ag-reuse-buffers "*ag*"
    (format "*ag: %s %s*" search-term directory)))

(defun ag--format-ignore (ignores)
  "Prepend '--ignore' to every item in IGNORES."
  (apply #'append
         (mapcar (lambda (item) (list "--ignore" item)) ignores)))

(cl-defun ag--format-command (string directory
                                     &key (regexp nil) (file-regex nil) (file-type nil))
  "Return ag command string for searching for STRING in DIRECTORY.
If REGEXP is non-nil, treat STRING as a regular expression."
  (let ((default-directory (file-name-as-directory directory))
        (arguments ag-arguments)
        (shell-command-switch "-c"))
    ;; Add double dashes at the end of command line if not specified in
    ;; ag-arguments.
    (unless (equal (car (last arguments)) "--")
      (setq arguments (append arguments '("--"))))
    (setq arguments
          (append '("--line-number" "--column" "--color" "--color-match" "30;43"
                    "--color-path" "1;32")
                  arguments))
    (if ag-group-matches
        (setq arguments (cons "--group" arguments))
      (setq arguments (cons "--nogroup" arguments)))
    (unless regexp
      (setq arguments (cons "--literal" arguments)))
    (when (or (eq system-type 'windows-nt) (eq system-type 'cygwin))
      ;; Use --vimgrep to work around issue #97 on Windows.
      (setq arguments (cons "--vimgrep" arguments)))
    (when (char-or-string-p file-regex)
      (setq arguments (append `("--file-search-regex" ,file-regex) arguments)))
    (when file-type
      (setq arguments (cons (format "--%s" file-type) arguments)))
    (if (integerp current-prefix-arg)
        (setq arguments (cons (format "--context=%d" (abs current-prefix-arg)) arguments))
      (when ag-context-lines
        (setq arguments (cons (format "--context=%d" ag-context-lines) arguments))))
    (when ag-ignore-list
      (setq arguments (append (ag--format-ignore ag-ignore-list) arguments)))
    (unless (file-exists-p default-directory)
      (error "No such directory %s" default-directory))
    (let ((command-string
           (mapconcat #'shell-quote-argument
                      (append (list ag-executable) arguments (list string "."))
                      " ")))
      ;; If we're called with a prefix, let the user modify the command before
      ;; running it. Typically this means they want to pass additional arguments.
      ;; The numeric value is used for context lines: positive is just context
      ;; number (no modification), negative allows further modification.
      (when (and current-prefix-arg (not (and (integerp current-prefix-arg) (> current-prefix-arg 0))))
        ;; Make a space in the command-string for the user to enter more arguments.
        (setq command-string (ag--replace-first command-string " -- " "  -- "))
        ;; Prompt for the command.
        (let ((adjusted-point (- (length command-string) (length string) 5)))
          (setq command-string
                (read-from-minibuffer "ag command: "
                                      (cons command-string adjusted-point)))))
      command-string)))

;; TODO: remove.
(cl-defun ag--search (string directory
                             &key (regexp nil) (file-regex nil) (file-type nil))
  "Run ag searching for the STRING given in DIRECTORY.
If REGEXP is non-nil, treat STRING as a regular expression."
  (compilation-start
   (ag--format-command string directory
                       :regexp regexp :file-regex file-regex :file-type file-type)
   #'ag-mode
   `(lambda (mode-name) ,(ag--buffer-name string directory))))

(defun ag--input-at-point ()
  "If there's an active selection, return that.
Otherwise, get the symbol at point, as a string."
  (cond ((use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end)))
        ((symbol-at-point)
         (substring-no-properties
          (symbol-name (symbol-at-point))))))

(defun ag--buffer-extension-regex ()
  "If the current buffer has an extension, return
a PCRE pattern that matches files with that extension.
Returns an empty string otherwise."
  (let ((file-name (buffer-file-name)))
    (if (stringp file-name)
        (format "\\.%s$" (ag--escape-pcre (file-name-extension file-name)))
      "")))

(defun ag--replace-first (string before after)
  "Replace the first occurrence of BEFORE in STRING with AFTER."
  (replace-regexp-in-string
   (concat "\\(" (regexp-quote before) "\\)" ".*\\'")
   after string
   nil nil 1))

(autoload 'vc-git-root "vc-git")

(require 'vc-svn)
;; Emacs 23.4 doesn't provide vc-svn-root.
(unless (functionp 'vc-svn-root)
  (defun vc-svn-root (file)
    (vc-find-root file vc-svn-admin-directory)))

(autoload 'vc-hg-root "vc-hg")

(autoload 'vc-bzr-root "vc-bzr")

(defun ag--project-root (file-path)
  "Guess the project root of the given FILE-PATH.

Use `ag-project-root-function' if set, or fall back to
`projectile-project-root'."
  (if ag-project-root-function
      (funcall ag-project-root-function file-path)
    (or
     (ignore-errors
       ;; This raises an error if we're not in a project.
       (projectile-project-root))
     file-path)))

(defun ag--dired-align-size-column ()
  (beginning-of-line)
  (when (looking-at "^  ")
    (forward-char 2)
    (search-forward " " nil t 4)
    (let* ((size-start (point))
           (size-end (search-forward " " nil t))
           (width (and size-end (- size-end size-start))))
      (when (and size-end
                 (< width 12)
                 (> width 1))
        (goto-char size-start)
        (insert (make-string (- 12 width) ? ))))))

(defun ag--dired-filter (proc string)
  "Filter the output of ag to make it suitable for `dired-mode'."
  (let ((buf (process-buffer proc))
        (inhibit-read-only t))
    (if (buffer-name buf)
        (with-current-buffer buf
          (save-excursion
            (save-restriction
              (widen)
              (let ((beg (point-max)))
                (goto-char beg)
                (insert string)
                (goto-char beg)
                (or (looking-at "^")
                    (progn
                      (ag--dired-align-size-column)
                      (forward-line 1)))
                (while (looking-at "^")
                  (insert "  ")
                  (ag--dired-align-size-column)
                  (forward-line 1))
                (goto-char beg)
                (beginning-of-line)

                ;; Remove occurrences of default-directory.
                (while (search-forward (concat " " default-directory) nil t)
                  (replace-match " " nil t))

                (goto-char (point-max))
                (if (search-backward "\n" (process-mark proc) t)
                    (progn
                      (dired-insert-set-properties (process-mark proc)
                                                   (1+ (point)))
                      (move-marker (process-mark proc) (1+ (point)))))))))
      (delete-process proc))))

(defun ag--dired-sentinel (proc state)
  "Update the status/modeline after the process finishes."
  (let ((buf (process-buffer proc))
        (inhibit-read-only t))
    (if (buffer-name buf)
        (with-current-buffer buf
          (let ((buffer-read-only nil))
            (save-excursion
              (goto-char (point-max))
              (insert "\n  ag " state)
              (forward-char -1)     ;Back up before \n at end of STATE.
              (insert " at " (substring (current-time-string) 0 19))
              (forward-char 1)
              (setq mode-line-process
                    (concat ":" (symbol-name (process-status proc))))
              ;; Since the buffer and mode line will show that the
              ;; process is dead, we can delete it now.  Otherwise it
              ;; will stay around until M-x list-processes.
              (delete-process proc)
              (force-mode-line-update)))
          (run-hooks 'dired-after-readin-hook)
          (message "%s finished." (current-buffer))))))

(defun ag--kill-process ()
  "Kill the `ag' process running in the current buffer."
  (interactive)
  (let ((ag (get-buffer-process (current-buffer))))
    (and ag (eq (process-status ag) 'run)
         (eq (process-filter ag) (function find-dired-filter))
         (condition-case nil
             (delete-process ag)
           (error nil)))))

(defun ag--escape-pcre (regexp)
  "Escape the PCRE-special characters in REGEXP so that it is
matched literally."
  (let ((alphanum "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
    (apply #'concat
           (mapcar
            (lambda (c)
              (cond
               ((not (string-match-p (regexp-quote c) alphanum))
                (concat "\\" c))
               (t c)))
            (mapcar #'char-to-string (string-to-list regexp))))))

;;;###autoload
(defun ag (string directory)
  "Search using ag in a given DIRECTORY for a given literal search STRING,
with STRING defaulting to the symbol under point.

If called with a prefix, prompts for flags to pass to ag."
  (interactive (list (ag--read-from-minibuffer "Search string")
                     (read-directory-name "Directory: ")))
  (ag--start-search string directory :literal t))

;;;###autoload
(defun ag-files (string file-type directory)
  "Search using ag in a given DIRECTORY for a given literal search STRING,
limited to files that match FILE-TYPE. STRING defaults to the
symbol under point.

If called with a prefix, prompts for flags to pass to ag."
  (interactive (list (ag--read-from-minibuffer "Search string")
                     (ag--read-file-type)
                     (read-directory-name "Directory: ")))
  (apply #'ag--search string directory file-type))

;;;###autoload
(defun ag-regexp (string directory)
  "Search using ag in a given directory for a given regexp.
The regexp should be in PCRE syntax, not Emacs regexp syntax.

If called with a prefix, prompts for flags to pass to ag."
  (interactive "sSearch regexp: \nDDirectory: ")
  (ag--start-search string directory :literal nil))

;;;###autoload
(defun ag-project (string)
  "Guess the root of the current project and search it with ag
for the given literal search STRING.

If called with a prefix, prompts for flags to pass to ag."
  (interactive (list (ag--read-from-minibuffer "Search string")))
  (ag--start-search string (ag--project-root default-directory)
                    :literal t))

;;;###autoload
(defun ag-project-files (string file-type)
  "Search using ag for a given literal search STRING,
limited to files that match FILE-TYPE. STRING defaults to the
symbol under point.

If called with a prefix, prompts for flags to pass to ag."
  (interactive (list (ag--read-from-minibuffer "Search string")
                     (ag--read-file-type)))
  (apply 'ag--search string (ag--project-root default-directory) file-type))

(defun ag--read-from-minibuffer (prompt)
  "Read a value from the minibuffer with PROMPT.
If there's a string at point, offer that as a default."
  (let* ((suggested (ag--input-at-point))
         (final-prompt
          (if suggested
              (format "%s (default %s): " prompt suggested)
            (format "%s: " prompt)))
         ;; Ask the user for input, but add `suggested' to the history
         ;; so they can use M-n if they want to modify it.
         (user-input (read-from-minibuffer
                      final-prompt
                      nil nil nil nil suggested)))
    ;; Return the input provided by the user, or use `suggested' if
    ;; the input was empty.
    (if (> (length user-input) 0)
        user-input
      suggested)))

;;;###autoload
(defun ag-project-regexp (regexp)
  "Guess the root of the current project and search it with ag
for the given regexp. The regexp should be in PCRE syntax, not
Emacs regexp syntax.

If called with a prefix, prompts for flags to pass to ag."
  (interactive (list (ag--read-from-minibuffer "Search regexp")))
  (ag--start-search regexp (ag--project-root default-directory)
                    :literal nil))

(autoload 'symbol-at-point "thingatpt")

;;;###autoload
(defalias 'ag-project-at-point 'ag-project)
(make-obsolete 'ag-project-at-point 'ag-project "0.19")

;;;###autoload
(defalias 'ag-regexp-project-at-point 'ag-project-regexp)
(make-obsolete 'ag-regexp-project-at-point 'ag-project-regexp "0.46")

;;;###autoload
(defun ag-dired (dir string)
  "Recursively find files in DIR matching literal search STRING.

The PATTERN is matched against the full path to the file, not
only against the file name.

The results are presented as a `dired-mode' buffer with
`default-directory' being DIR.

See also `ag-dired-regexp'."
  (interactive "DDirectory: \nsFile pattern: ")
  (ag-dired-regexp dir (ag--escape-pcre pattern)))

;;;###autoload
(defun ag-dired-regexp (dir regexp)
  "Recursively find files in DIR matching REGEXP.
REGEXP should be in PCRE syntax, not Emacs regexp syntax.

The REGEXP is matched against the full path to the file, not
only against the file name.

Results are presented as a `dired-mode' buffer with
`default-directory' being DIR.

See also `find-dired'."
  (interactive "DDirectory: \nsFile regexp: ")
  (let* ((dired-buffers dired-buffers) ;; do not mess with regular dired buffers
         (orig-dir dir)
         (dir (file-name-as-directory (expand-file-name dir)))
         (buffer-name (if ag-reuse-buffers
                          "*ag dired*"
                        (format "*ag dired pattern:%s dir:%s*" regexp dir)))
         (cmd (concat ag-executable " --nocolor -g '" regexp "' "
                      (shell-quote-argument dir)
                      " | grep -v '^$' | sed s/\\'/\\\\\\\\\\'/ | xargs -I '{}' ls "
                      dired-listing-switches " '{}' &")))
    (with-current-buffer (get-buffer-create buffer-name)
      (switch-to-buffer (current-buffer))
      (widen)
      (kill-all-local-variables)
      (if (fboundp 'read-only-mode)
          (read-only-mode -1)
        (setq buffer-read-only nil))
      (let ((inhibit-read-only t)) (erase-buffer))
      (setq default-directory dir)
      (run-hooks 'dired-before-readin-hook)
      (shell-command cmd (current-buffer))
      (insert "  " dir ":\n")
      (insert "  " cmd "\n")
      (dired-mode dir)
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map (current-local-map))
        (define-key map "\C-c\C-k" 'ag--kill-process)
        (use-local-map map))
      (set (make-local-variable 'dired-sort-inhibit) t)
      (set (make-local-variable 'revert-buffer-function)
           `(lambda (ignore-auto noconfirm)
              (ag-dired-regexp ,orig-dir ,regexp)))
      (if (fboundp 'dired-simple-subdir-alist)
          (dired-simple-subdir-alist)
        (set (make-local-variable 'dired-subdir-alist)
             (list (cons default-directory (point-min-marker)))))
      (let ((proc (get-buffer-process (current-buffer))))
        (set-process-filter proc #'ag--dired-filter)
        (set-process-sentinel proc #'ag--dired-sentinel)
        ;; Initialize the process marker; it is used by the filter.
        (move-marker (process-mark proc) 1 (current-buffer)))
      (setq mode-line-process '(":%s")))))

;;;###autoload
(defun ag-project-dired (pattern)
  "Recursively find files in current project matching PATTERN.

See also `ag-dired'."
  (interactive "sFile pattern: ")
  (ag-dired-regexp (ag--project-root default-directory) (ag--escape-pcre pattern)))

;;;###autoload
(defun ag-project-dired-regexp (regexp)
  "Recursively find files in current project matching REGEXP.

See also `ag-dired-regexp'."
  (interactive "sFile regexp: ")
  (ag-dired-regexp (ag--project-root default-directory) regexp))

;;;###autoload
(defun ag-kill-buffers ()
  "Kill all `ag-mode' buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (eq (buffer-local-value 'major-mode buffer) 'ag-mode)
      (kill-buffer buffer))))

;;;###autoload
(defun ag-kill-other-buffers ()
  "Kill all `ag-mode' buffers other than the current buffer."
  (interactive)
  (let ((current-buffer (current-buffer)))
    (dolist (buffer (buffer-list))
      (when (and
             (eq (buffer-local-value 'major-mode buffer) 'ag-mode)
             (not (eq buffer current-buffer)))
        (kill-buffer buffer)))))

(defun ag-rerun ()
  "Re-run the search in the current ag buffer."
  (interactive)
  (ag--start-search ag--search-term default-directory
                    :literal ag--literal-search))

(defun ag--move-result (amount)
  "Move point by AMOUNT matches in a results buffer.
AMOUNT may be negative to move backwards."
  (let ((moved 0)
        (direction (if (< amount 0) -1 1)))
    (beginning-of-line)
    (while (< moved (abs amount))
      (unless (zerop (forward-line direction))
        (user-error "No more results"))
      (when (get-text-property (point) 'ag-line-number)
        (cl-incf moved)))))

(defun ag-next-result ()
  "Move point to the next result in the ag buffer."
  (interactive)
  (ag--move-result 1))

(defun ag-prev-result ()
  "Move point to the previous result in the ag buffer."
  (interactive)
  (ag--move-result -1))

(defun ag--parse-output-line (line)
  "Split LINE into filename, line number, column number and match text."
  (-let [(file-name line-number column-number text)
         (s-split-up-to ":" line 3)]
    (list (ag--strip-shell-highlighting file-name)
          (read (ag--strip-shell-highlighting line-number))
          (read (ag--strip-shell-highlighting column-number))
          (ag--fontify-shell-highlighting text))))

(defun ag--strip-shell-highlighting (text)
  "Remove all ANSI escape codes from TEXT."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (re-search-forward "\033\\[[0-9;]*[mK]" (point-max) 1)
      (replace-match "" t t))
    (buffer-string)))

(defun ag--fontify-shell-highlighting (text)
  "Convert colored text output by the ag process to a fontified string.

Color escape sequences for ag substring matches are
fontified. All other color escape sequences are discarded.

Assumes TEXT is one or more whole lines, so escape sequences
are complete."
  (with-temp-buffer
    (insert text)
    (let ((beg (point-min)))
      (goto-char beg)

      ;; Highlight ag matches and delete marking sequences.
      (while (re-search-forward "\033\\[30;43m\\(.*?\\)\033\\[[0-9]*m" (point-max) 1)
        (replace-match (propertize (match-string 1)
                                   'face 'ag-match-face)
                       t t))
      
      ;; Delete all remaining escape sequences
      (goto-char beg)
      (while (re-search-forward "\033\\[[0-9;]*[mK]" (point-max) 1)
        (replace-match "" t t))

      (buffer-substring beg (point-max)))))

(defun ag--get-supported-types ()
  "Query the ag executable for which file types it recognises."
  (let* ((ag-output (shell-command-to-string (format "%s --list-file-types" ag-executable)))
         (lines (-map #'s-trim (s-lines ag-output)))
         (types (--keep (when (s-starts-with? "--" it) (s-chop-prefix "--" it )) lines))
         (extensions (--map (s-split "  " it) (--filter (s-starts-with? "." it) lines))))
    (-zip types extensions)))

(defun ag--read-file-type ()
  "Prompt the user for a known file type, or let them specify a PCRE regex."
  (let* ((all-types-with-extensions (ag--get-supported-types))
         (all-types (mapcar 'car all-types-with-extensions))
         (file-type
          (completing-read "Select file type: "
                           (append '("custom (provide a PCRE regex)") all-types)))
         (file-type-extensions
          (cdr (assoc file-type all-types-with-extensions))))
    (if file-type-extensions
        (list :file-type file-type)
      (list :file-regex
            (read-from-minibuffer "Filenames which match PCRE: "
                                  (ag--buffer-extension-regex))))))

(provide 'ag)
;;; ag.el ends here
