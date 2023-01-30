;;
;;
;;

(ert-deftest add-node-modules-path/split-comma-separated-list-test ()
  (should (equal (add-node-modules-path/split-comma-separated-list "pnpm bin, pnpm bin -w,  ls -al   ")
		 '("pnpm bin" "pnpm bin -w" "ls -al")))
  (should (equal (add-node-modules-path/split-comma-separated-list "  pnpm bin -w") '("pnpm bin -w")))
  (should (equal (add-node-modules-path/split-comma-separated-list ",,, ls -al ,  ,,") '("ls -al")))
  (should (eq (add-node-modules-path/split-comma-separated-list "") nil))
  (should (eq (add-node-modules-path/split-comma-separated-list "    ") nil))
  (should (eq (add-node-modules-path/split-comma-separated-list "   ,  ,,  ,") nil)))


(ert-deftest add-node-modules-path/exec-command-test ()
  (should (eq (add-node-modules-path/exec-command nil) nil))
  (should (eq (add-node-modules-path/exec-command "") nil))
  (should (eq (add-node-modules-path/exec-command 3) nil))
  (should (eq (add-node-modules-path/exec-command 'a-symbol) nil))
  (if (equal system-type 'gnu/linux)
      (let ((res (add-node-modules-path/exec-command "echo \"/usr/bin\"")))
	(should (equal (plist-get res 'command) "echo \"/usr/bin\""))
	(should (equal (plist-get res 'result) "/usr/bin"))
	(should (equal (plist-get res 'directory-p) t))))
  (if (equal system-type 'gnu/linux)
      (let ((res (add-node-modules-path/exec-command "echo \"ls -al\"")))
	(should (equal (plist-get res 'command) "echo \"ls -al\""))
	(should (equal (plist-get res 'result) "ls -al"))
	(should (equal (plist-get res 'directory-p) nil)))))

(ert-deftest add-node-modules-path/exec-command-list-test ()
  (let ((should-produce-nil '(nil () 42 "a string" (1 2 3))))
    (dolist (elt should-produce-nil)
      (should (eq (add-node-modules-path/exec-command-list elt) nil)))))

(ert-deftest add-node-modules-path/get-valid-directories-test ()
  (let ((should-produce-nil '(nil () 1 "str" (1 2 3) (('directory-p nil) ('directory-p nil))))
	(test-data '(
		     (((directory-p t result "/usr/bin")) ("/usr/bin"))
		     (((directory-p t result "/home") (directory-p t result "/usr")) ("/home" "/usr"))
		     (((directory-p nil result "/not-a-valid-dir") (directory-p t result "/usr")) ("/usr"))
		     )))
    (dolist (elt should-produce-nil)
      (should (eq (add-node-modules-path/get-valid-directories elt) nil)))
    (dolist (elt test-data)
      (should (equal (add-node-modules-path/get-valid-directories (car elt)) (cadr elt))))))

(ert-deftest add-node-modules-path/get-invalid-executions-test ()
  (let ((should-produce-nil '(nil () 1 "str" (('directory-p t) ('directory-p t))))
	(test-data '(
		     (((directory-p nil result "/usr/bin")) ((directory-p nil result "/usr/bin")))
		     (((directory-p t result "/home") (directory-p nil result "/usr")) ((directory-p nil result "/usr")))
		     (((directory-p nil result "/xxx") (directory-p t result "/usr")) ((directory-p nil result "/xxx")))
		     )))
    (dolist (elt should-produce-nil)
      (should (eq (add-node-modules-path/get-invalid-executions elt) nil)))
    (dolist (elt test-data)
      (should (equal (add-node-modules-path/get-invalid-executions (car elt)) (cadr elt))))))

(defun add-node-modules-path/exec-add-node-modules-path-test (command additions)
  ;; remove any local binding of EXEC-PATH, if present
  (kill-local-variable 'exec-path)
  ;; prepare environment
  (make-local-variable 'add-node-modules-path-debug)
  (setq add-node-modules-path-debug nil)
  (make-local-variable 'add-node-modules-path-command)
  (setq add-node-modules-path-command command)
  ;; run interactive command, which will create local binding of EXEC-PATH and add to it
  (add-node-modules-path)
  ;; checks
  (should (eq (local-variable-p 'exec-path) t))
  (let ((i 0))
    (dolist (elt additions)
      (should (equal (nth i exec-path) elt))
      (setq i (1+ i))))
  ;; env cleanup
  (kill-local-variable 'add-node-modules-path-debug)
  (kill-local-variable 'add-node-modules-path-command))

(ert-deftest add-node-modules-path-single-command-test ()
  (add-node-modules-path/exec-add-node-modules-path-test "echo \"/etc\"" '("/etc")))

(ert-deftest add-node-modules-path-multiple-commands-test ()
  (add-node-modules-path/exec-add-node-modules-path-test "echo \"/etc\", echo \"/var\"" '("/etc" "/var")))
  
(ert-deftest add-node-modules-path-multiple-commands-with-failures-test ()
  (add-node-modules-path/exec-add-node-modules-path-test "ls -al, echo \"/var\", date, echo \"/etc\", clear" '("/var" "/etc")))
