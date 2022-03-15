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

(require 's)

(defgroup add-node-modules-path nil
  "Put node_modules binaries into `exec-path'."
  :prefix "add-node-modules-path-"
  :group 'environment)

;;;###autoload
(defcustom add-node-modules-path-command "npm bin"
  "Command to find the bin path."
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

  (let* ((res (s-chomp (shell-command-to-string add-node-modules-path-command)))
         (exists (file-exists-p res))
         )
    (cond
     (exists
      (make-local-variable 'exec-path)
      (add-to-list 'exec-path res)
      (when add-node-modules-path-debug
        (message "Added to `exec-path`: %s" res))
      )
     (t
      (when add-node-modules-path-debug
        (message "Failed to run `%s':\n %s" add-node-modules-path-command res))
      ))
    )
  )

(provide 'add-node-modules-path)

;;; add-node-modules-path.el ends here
