;; ruby-test.el
;; Caspar Florian Ebeling, 2007-12-06

;; todo
;;   - output points as feedback on test progress
;;   - color for ok/fail

(defvar last-run-test-file nil)
(defvar ruby-path "/opt/local/bin/ruby" "Set the ruby binary to be used.")
(defvar spec-path nil "Set the spec exectable to be used.")

(defun ruby-spec-p (filename)
  (and (stringp filename) (string-match "spec\.rb$" filename)))

(defun ruby-test-p (filename)
  (and (stringp filename) (string-match "test\.rb$" filename)))

(defun ruby-any-test-p (filename)
  (or (ruby-spec-p filename)
      (ruby-test-p filename)))

(defun odd-p (i) (= 1 (mod i 2)))

(defun even-p (i) (= 0 (mod i 2)))

(defun select (fn ls)
  "Create a list for which fn return non-nil"
  (let ((result nil))
    (dolist (item ls)
      (if (funcall fn item)
	  (setq result (cons item result))))
    (reverse result)))
(defalias 'find-all 'select)

(defun invoke-test-file (command-string category file)
  (message (format "Running %s '%s'..." category file))
  (display-buffer test-output-buffer)
  (setq last-run-test-file file)
  (save-excursion
    (set-buffer test-output-buffer)
    (erase-buffer)
    (start-process "process-ruby-test" test-output-buffer
		   command-string file)
    (goto-char (point-max)))
  (message (format "%s '%s' done." (capitalize category) file)))

(defun run-spec (file)
  (invoke-test-file spec-binary "spec" file))

(defun run-test (file)
  (invoke-test-file ruby-binary "unit test" file))

(defun run-test-file (file)
  (cond
   ((ruby-spec-p file) (run-spec file))
   ((ruby-test-p file) (run-test file))
   (t (error "File is not a known ruby test file"))))

(defun find-ruby-test-file ()
  (setq last-run-test-file
	(car (select 'ruby-any-test-p 
		     (select 'identity
			     (nconc
			      (cons 
			       (buffer-file-name)
			       (mapcar 
				(lambda (win-name) (buffer-file-name (window-buffer win-name)))
				(window-list))) 
			      (list last-run-test-file)))))))

(defun ruby-run-buffer-file-as-test ()
  "Run buffer's file, first visible window file or last-run as ruby test (or spec)."
  (interactive)
  (let ((ruby-binary (or ruby-path "ruby"))
	(spec-binary (or spec-path "spec"))
	(test-output-buffer (get-buffer-create "*Ruby-Tests*"))
	(test-file (find-ruby-test-file)))
    (if test-file 
	(run-test-file test-file)
      (message "No test among visible buffers or run earlier."))))

(global-set-key (kbd "C-x t") 'ruby-run-buffer-file-as-test)
(global-set-key (kbd "C-x SPC") 'ruby-run-buffer-file-as-test)

(provide 'ruby-test)