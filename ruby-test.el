;; ruby-test.el
;; Caspar Florian Ebeling <florian.ebeling@gmail.com>, 2007-12-06
;; This software can be redistributed. GPL v2 applies.

;; todo
;;   - color for ok/fail
;;   - run single test method
;;   - use small window for output

(defvar ruby-test-buffer-name "*Ruby-Test*")

;;; template for key map:
;;(defvar ruby-test-mode-map () 
;;  "Keymap of commands active in the output buffer.")
;; (if ruby-test-mode-map
;;     nil
;;   (setq ruby-test-mode-map (make-keymap))
;;   (define-key ruby-test-mode-map ...))

(defvar ruby-test-mode-hook)

(defvar last-run-test-file)

(defvar ruby-path "/opt/local/bin/ruby" 
  "Set the ruby binary to be used.")

(defvar spec-path nil 
  "Set the spec exectable to be used.")

(defvar ruby-test-buffer)

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

(defun invoke-test-file (command-string category file buffer)
  (message (format "Running %s '%s'..." category file))
  (display-buffer buffer)
  (setq last-run-test-file file)
  (save-excursion
    (set-buffer buffer)
    (erase-buffer)
    (insert "-*- mode: ruby-test -*-\n\n")
    (let ((proc (start-process "ruby-test" buffer command-string file)))
      (set-process-sentinel proc 'runner-sentinel)))
  (message (format "%s '%s' done." (capitalize category) file)))

(defun runner-sentinel (process event)
  (save-excursion
    (set-buffer ruby-test-buffer)
;;    (insert (format "[\n%s]" event))
    ))

(defun run-spec (file buffer)
  (invoke-test-file spec-binary "spec -Dc -c" file buffer))

(defun run-test (file buffer)
  (invoke-test-file ruby-binary "unit test" file buffer))

(defun run-test-file (file buffer)
  (cond
   ((ruby-spec-p file) (run-spec file buffer))
   ((ruby-test-p file) (run-test file buffer))
   (t (error "File is not a known ruby test file"))))

(defun find-ruby-test-file ()
  (setq last-run-test-file
	(car (select 'ruby-any-test-p 
		     (let ((files (nconc
				   (cons 
				    (buffer-file-name)
				    (mapcar 
				     (lambda (win-name) (buffer-file-name (window-buffer win-name)))
				     (window-list))))))
		       (if (boundp 'last-run-test-file)
			   (nconc files '(last-run-test-file)))
		       (mapcar 'message files)
		       (select 'identity files))))))

(defun ruby-run-buffer-file-as-test ()
  "Run buffer's file, first visible window file or last-run as ruby test (or spec)."
  (interactive)
  (setq ruby-test-buffer (get-buffer-create ruby-test-buffer-name))
  (let ((ruby-binary (or ruby-path "ruby"))
	(spec-binary (or spec-path "spec"))
	(test-file (find-ruby-test-file)))
    (if test-file 
	(run-test-file test-file ruby-test-buffer)
      (message "No test among visible buffers or run earlier."))))

(defvar ruby-test-font-lock-keywords
  (list 
   '("^\\(\\(.*\\):\\([0-9]+\\)\\):" 1 font-lock-warning-face)
   ))

(defun ruby-test-mode ()
  "Major mode for running ruby tests and display results."
  (interactive)
  (kill-all-local-variables)
;;  (use-local-map ruby-test-mode-map)
  (set (make-local-variable 'font-lock-defaults) '((ruby-test-font-lock-keywords) nil nil))
  (set (make-local-variable 'font-lock-keywords) 'ruby-test-font-lock-keywords)
;;  (set (make-local-variable 'indent-line-function) 'ruby-test-indent-line) 
  (setq major-mode 'ruby-test-mode)
  (setq mode-name "Ruby-Test")
  (run-hooks 'ruby-test-mode-hook))

;; global, since these bindings should be visible in other windows 
;; operating on the `last-run-test-file'.
(global-set-key (kbd "C-x t") 'ruby-run-buffer-file-as-test)
(global-set-key (kbd "C-x SPC") 'ruby-run-buffer-file-as-test)

(provide 'ruby-test)
