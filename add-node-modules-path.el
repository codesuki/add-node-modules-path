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
;; This file provides `add-node-modules-path', which runs `npm bin`
;; recursively through up directory tree and and adds the path to the
;; buffer local `exec-path'.  This allows Emacs to find project based
;; installs of e.g. eslint.
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

(require 's)

(defgroup add-node-modules-path nil
  "Put node_modules binaries into `exec-path'."
  :prefix "add-node-modules-path-"
  :group 'environment)

;;;###autoload
(defcustom add-node-modules-path-command-bin "npm bin"
  "Command to find the bin path."
  :type 'string)

;;;###autoload
(defcustom add-node-modules-path-command-root "npm root"
  "Command to find the root path."
  :type 'string)

;;;###autoload
(defcustom add-node-modules-path-debug nil
  "Enable verbose output when non nil."
  :type 'boolean
  :group 'add-node-modules-path)

;;;###autoload
(defun add-node-modules-path ()
  "Run `npm bin` command and add the path to the `exec-path`.
If `npm` command fails, it does nothing."
  (interactive)

  (let* ((root-directory (s-chomp (shell-command-to-string add-node-modules-path-command-root)))
         (bin-directory (s-chomp (shell-command-to-string add-node-modules-path-command-bin)))
         (exists (file-exists-p bin-directory))
         (isRoot (string= (directory-file-name (file-name-directory root-directory)) "/")))
    (cond
     ((not isRoot)
      (unless (local-variable-p 'exec-path)
        (make-local-variable 'exec-path))
      (when exists
        (add-to-list 'exec-path bin-directory)
        (when add-node-modules-path-debug
          (message "Added to `exec-path`: %s" bin-directory)))
      (cd (concat root-directory "/../.."))
      (add-node-modules-path))
     (t
      (cd (file-name-directory (buffer-file-name)))
      (when add-node-modules-path-debug
        (message "Found root directory"))))))

(provide 'add-node-modules-path)

;;; add-node-modules-path.el ends here
