;;; add-node-modules-path.el --- Add node_modules to your exec-path

;; Copyright (C) 2016 Neri Marschik
;; This package uses the MIT License.
;; See the LICENSE file.

;; Author: Neri Marschik <marschik_neri@cyberagent.co.jp>
;; Version: 1.0
;; Package-Requires: ()
;; Keywords: javascript, node, node_modules, eslint
;; URL: https://github.com/codesuki/add-node-modules-path

;;; Commentary:
;;
;; This file provides `add-node-modules-path', which searches
;; the current files parent directories for the `node_modules/.bin/' directory
;; and adds it to the buffer local `exec-path'.
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

;;;###autoload
(defcustom add-node-modules-path-debug nil
  "Enable verbose output when non nil."
  :type 'boolean)

;;;###autoload
(defun add-node-modules-path ()
  "Search the current buffer's parent directories for `node_modules/.bin`.
Traverse the directory structure up, until reaching the user's home directory.
Any path found is added to the `exec-path'."
  (interactive)
  (let* ((file (or (buffer-file-name) default-directory))
         (path (locate-dominating-file file "node_modules"))
         (home (expand-file-name "~"))
         (root (and path (expand-file-name path)))
         (roots '()))
    (while root
      (let ((bindir (expand-file-name "node_modules/.bin/" root)))
        (when (file-directory-p bindir)
          (add-to-list 'roots bindir)))
      (if (string= root home)
          (setq root nil)
        (setq root (directory-file-name (file-name-directory root)))))
    (if roots
        (progn
          (make-local-variable 'exec-path)
          (while roots
            (add-to-list 'exec-path (car roots))
            (when add-node-modules-path-debug
              (message (concat "added " (car roots) " to exec-path")))
            (setq roots (cdr roots))))
      (when add-node-modules-path-debug
        (message (concat "node_modules/.bin not found for " file))))))

(provide 'add-node-modules-path)

;;; add-node-modules-path.el ends here
