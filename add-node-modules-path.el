;;; add-node-modules-path.el --- Add node_modules to your exec-path

;; Copyright (C) 2016 Neri Marschik
;; This package uses the MIT License.
;; See the LICENSE file.

;; Author: Neri Marschik <marschik_neri@cyberagent.co.jp>
;; Version: 1.0
;; Package-Requires: ((s "1.12.0"))
;; Keywords: javascript, node, node_modules, eslint
;; URL: https://github.com/codesuki/add-node-modules-path

;;; Commentary:
;;
;; This file provides `add-node-modules-path', which runs `npm bin` and
;; and adds the path to the buffer local `exec-path'.
;; This allows Emacs to find project based installs of e.g. eslint.
;;
;; Usage:
;;     M-x add-node-modules-path
;;
;;     To automatically run it when opening a new buffer:
;;     (Choose depending on your favorite mode.)
;;
;;     (eval-after-load 'js-mode
;;       '(add-hook 'js-mode-hook #'add-node-modules-path))
;;
;;     (eval-after-load 'js2-mode
;;       '(add-hook 'js2-mode-hook #'add-node-modules-path))

;;; Code:
(require 'seq)
(require 's)

(defgroup add-node-modules-path nil
  "Put node_modules binaries into `exec-path'."
  :prefix "add-node-modules-path-"
  :group 'environment)

;;;###autoload
(defcustom add-node-modules-path-command '("npm bin")
  "Command(s) to find the bin path. To add multiple bin paths, simply add
multiple commands to the list, e.g. \\='(\"pnpm bin\" \"pnpm bin -w\")"
  :type '(repeat string)
  :set (lambda (symbol value)
	 "Converts a non-list value to a single-element list of the same value.
This is necessary to be backward compatible, since previous versions of this
custom var were of type string."
	 (set-default symbol (if (listp value) value (list value)))))

;;;###autoload
(defcustom add-node-modules-path-debug nil
  "Enable verbose output when non nil."
  :type 'boolean
  :group 'add-node-modules-path)

(defun add-node-modules-path/trim-list-and-elements (list)
  "Trims all string values in LIST and empty / non-string values are removed."
  (if (listp list)
      (seq-filter 's-present? (mapcar 's-trim (seq-filter 'stringp list)))))

(defun add-node-modules-path/exec-command (command)
  "Executes the given COMMAND and returns a plist containing the command, 
its shell execution result and a boolean indicating, whether the execution
result denotes a valid directory"
  (if (and (stringp command) (s-present? command))
      (let ((result (s-chomp (shell-command-to-string command))))
	(list 'command command 'result result 'directory-p (file-directory-p result)))))

(defun add-node-modules-path/exec-command-list (command-list)
  "Executes all commands in COMMAND-LIST and returns a list of plists
containing the various command execution results. Elements in COMMAND-LIST which
are not strings are ignoredand will not appear in the result."
  (if (listp command-list)
      (seq-filter 'consp (mapcar 'add-node-modules-path/exec-command command-list))))

(defun add-node-modules-path/get-valid-directories (command-executions)
  "Filters the provided COMMAND-EXECUTIONS for entries, whose execution result
denotes an existing directory"
  (if (listp command-executions)
      (let ((filtered (seq-filter '(lambda (elt) (plist-get elt 'directory-p)) command-executions)))
	(mapcar #'(lambda (elt) (plist-get elt 'result)) filtered))))

(defun add-node-modules-path/get-invalid-executions (command-executions)
  "Filters the provided COMMAND-EXECUTIONS for entries, whose execution result
denotes an invalid or non-existing directory"
  (if (listp command-executions)
      (seq-filter #'(lambda (elt) (and (plist-member elt 'directory-p) (not (plist-get elt 'directory-p)))) command-executions)))

(defun add-node-modules-path/warn-about-failed-executions (command-executions)
  "Displays warnings about all failed COMMAND-EXECUTIONS."
  (let ((failed (add-node-modules-path/get-invalid-executions command-executions)))
    (dolist (elt failed)
      (let ((cmd (plist-get elt 'command))
	    (path (plist-get elt 'result)))
	(display-warning 'add-node-modules-path (format-message "Failed to run `%s':\n %s" cmd path))))))

(defun add-node-modules-path/add-to-list-multiple (list to-add)
  "Adds multiple items to LIST."
  (dolist (item to-add)
    (add-to-list list item)))

;;;###autoload
(defun add-node-modules-path ()
  "Run `npm bin` command and add the path to the `exec-path`.
If `npm` command fails, it does nothing."
  (interactive)
  (let* ((commands (add-node-modules-path/trim-list-and-elements add-node-modules-path-command))
         (executions (add-node-modules-path/exec-command-list commands))
         (dirs (add-node-modules-path/get-valid-directories executions)))
    (if (length> dirs 0)
	(progn
	  (make-local-variable 'exec-path)
	  (add-node-modules-path/add-to-list-multiple 'exec-path (reverse dirs))
	  (if add-node-modules-path-debug
	      (message "Added to `exec-path`: %s" (s-join ", " dirs)))))
    (if add-node-modules-path-debug
	(add-node-modules-path/warn-about-failed-executions executions))))
	
(provide 'add-node-modules-path)

;;; add-node-modules-path.el ends here
