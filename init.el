;; emacs init.el - Florian Ebeling

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

(defun invoke-test-file (command-string category)
  (message (format "Running %s..." category))
  (display-buffer test-output-buffer)
  (setq last-run-test-file file)
  (shell-command (format command-string file) test-output-buffer)
  (save-excursion
    (set-buffer test-output-buffer)
    (goto-char (point-max)))
  (message (format "%s done." (capitalize category))))

(defun run-spec (file)
  (invoke-test-file (concat spec-binary " %s") "spec"))

(defun run-test (file)
  (invoke-test-file (concat ruby-binary " %s") "unit test"))

(defun run-test-file (file)
  (cond
   ((ruby-spec-p file) (run-spec file))
   ((ruby-test-p file) (run-test file))
   (t (error "File is not a known ruby test file"))))

(let ((last-run-test-file))
  (defun find-ruby-test-file ()
    (message "existing value last...: %s" last-run-test-file)
    (setq last-run-test-file
	  (car (select 'ruby-any-test-p 
		       (select 'identity
			       (append
				(cons 
				 (buffer-file-name)
				 (mapcar 
				  (lambda (win-name) (buffer-file-name (window-buffer win-name)))
				  (window-list))) 
				(list last-run-test-file))))))))

(defvar ruby-path "/opt/local/bin/ruby" "Set the ruby binary to be used.")
(defvar spec-path nil "Set the spec exectable to be used.")

;; todo
;;   - output points as feedback on test progress
;;   - color for ok/fail
(defun ruby-run-buffer-file-as-test ()
  "Run buffer's file, first visible window file or last-run as ruby test (or spec)."
  (interactive)
  (let ((fname "ruby-run-buffer-file-as-test") ;; how to find the function name reflectively?
	(ruby-binary (or ruby-path "ruby"))
	(spec-binary (or spec-path "spec"))
	(test-output-buffer (get-buffer-create "*Ruby-Tests*"))
	(test-file (find-ruby-test-file)))
    (if test-file (run-test-file test-file)
      (message "No test among visible buffers or run earlier."))))

(global-set-key (kbd "C-x t") 'ruby-run-buffer-file-as-test)
(global-set-key (kbd "C-x SPC") 'ruby-run-buffer-file-as-test)

(defun pull-line-up ()
  "Drags a line up by one, and moves point accordingly."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(global-set-key [M-up] 'pull-line-up)

(defun pull-line-down ()
  "Drags a line down by one, and moves point accordingly."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(global-set-key [M-down] 'pull-line-down)

(defun indent-buffer ()
  ;; Author: Mathias Creutz
  "Indent every line in the buffer."
  (interactive)
  (indent-region (point-min) (point-max) nil))

;; shadows tab-to-tab-stop binding.
(global-set-key "\M-i" 'indent-buffer)

(defun symq ()
  "symbol quote, puts double quotes around word."
  (interactive)
  (save-excursion 
    (if (not (word-beginning-p)) 
	(backward-word 1))
    (insert-char ?\" 1)
    (forward-word 1)
    (insert-char ?\" 1))
  (forward-word 2))

(global-set-key [f8] 'symq)

(defun word-beginning-p ()
  (interactive)
  (word-boundary 'car))

(defun word-ending-p ()
  (interactive)
  (word-boundary 'cdr))

(defun word-boundary (func)
  (if (equal (funcall func (bounds-of-thing-at-point 'word)) (point))
      (progn 
	(message "cursor is at a word boundary")
	t)
    nil))

(defun scroll-up-1 ()
  (interactive)
  (scroll-up 1))

(define-key global-map [S-down] 'scroll-up-1)
					;(define-key global-map "\C-\S-n" 'scroll-up-1)

(defun scroll-down-1 ()
  (interactive)
  (scroll-down 1))

(define-key global-map [S-up] 'scroll-down-1)
					;(define-key global-map [C-S-P] 'scroll-down-1)

(defun flip-buffer ()
  (interactive)
  (switch-to-buffer nil))

(global-set-key [C-S-down] 'flip-buffer)

(defun buffer-select ()
  "Select a buffer from a buffer-menu-like list, but do not put it into recent-buffer list."
  (interactive)
  (let ((files-only t))
    (switch-to-buffer (list-buffers-noselect files-only) 'norecord)))

(global-set-key [C-S-up] 'buffer-select)

(defun insert-date ()
  "Insert the current date at point"
  (interactive)
  (insert (format-time-string "%d.%m.%y")))

;; configuration section

(setq default-frame-alist '((top . 1) (left . 1) (width . 130) (height . 44)))

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'Info-default-directory-list "/opt/local/share/info/")

;; workstation-specific settings.
(let ((hostname (system-name)))
  (cond
   ((equal hostname "dev14.iconmobile.de")
    (message "Intializing for host %s" hostname)
    (require 'psvn)
    (find-file "~/TODO"))
   (nil ;; todo: test for ubuntu laptop
    (message "Intializing for host %s" hostname)
    (let ((slime-dir-path "~/slime"))
      (if (file-exists-p slime-dir-path)
	  (add-to-list 'load-path slime-dir-path)
	(with-output-to-temp-buffer "*slime init warnings*" 
	  (princ (format "slime-dir-path does not exist: '%s'" slime-dir-path))))))
   ((equal hostname "flomac.local")
    (message "Initializing for host %s" hostname)
    (setq default-frame-alist '((top . 1) (left . 1) 
				(width . 125) (height . 35)))
    (setq mail-host-address "florian.ebeling@gmail.com")
    ;; erlang
    (setq otp-path "/opt/local/lib/erlang/lib/tools-2.5.5/emacs/")
    (setq load-path (cons otp-path load-path))
    (setq erlang-root-dir "/opt/local/bin")
    (setq exec-path (cons "/opt/local/lib/erlang" exec-path))
    (require 'erlang-start))
   ((equal hostname "flomac-work.local")
    (setq default-frame-alist '((top . 1) (left . 1) 
				(width . 220) (height . 30)))
    (message "Initializing for nugg.ad")
    (setq mail-host-address "florian.ebeling@nugg.ad")
    (find-file "~/todo")
    ;;
    )))

;;(setq make-backup-files nil)
(setq default-case-fold-search t)
(setq auto-compression-mode t)
(setq-default uniquify-buffer-name-style 'post-forward)
(setq-default tab-width 8)

(global-font-lock-mode 1)
(show-paren-mode 1)
(setq visible-bell t)

;; for emacsclient
(server-start) 

(defalias 'qrr 'query-replace-regexp)
(defalias 'qr 'query-replace)
(defalias 'cr 'comment-region)
(defalias 'ucr 'uncomment-region)
(defalias 'ir 'indent-region)
(defalias 'bb 'beginning-of-buffer)
(defalias 'eb 'end-of-buffer)

(setq-default abbrev-file-name "~/.emacs.d/abbrev_defs")
(setq-default abbrev-mode t)
(read-abbrev-file)
(setq save-abbrevs nil)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) 
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1)) 
;;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1)) 

;; enable copy-paste within X Window under Linux
(setq x-select-enable-clipboard t)
(if x-no-window-manager
    (progn 
      (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
      (message "cut-and-paste with x enabled")))

;;; general key remapping

;; free strokes
;; C-# -> as new/custom duplicate-line keybinding  
;; M-p 
;; C-. 
;; C-f8 -> make ruby local_var from region

(global-set-key (kbd "C-.") 'find-file-at-point)
(global-set-key [C-S-left] 'previous-buffer)
(global-set-key [C-S-right] 'next-buffer)

(fset 'to-java-string
      [?\C-a ?" ?\C-e ?" ?  ?+ down])
(global-set-key [f6] 'to-java-string)

(fset 'purge-line
      [?\C-a ?\C-  ?\C-n ?\C-c ?\C-k])
(global-set-key [\C-K] 'purge-line)

(fset 'mark-as-done
      [?\C-e ?  ?( ?\M-x ?i ?n ?s ?e ?r ?t ?- ?d ?a ?t ?e return ?) ?\C-a ?\C-k ?\C-k ?\C-s ?D ?O ?N ?E ?\C-m return ?\C-y ?\C-k ?\C-r ?T ?O ?D ?O ?\C-m ?\C-n])
(global-set-key [f2] 'mark-as-done)

(global-set-key [f5] 'call-last-kbd-macro)
(global-set-key [f3] 'edit-last-kbd-macro)
(global-set-key (kbd "C-S-l") 'goto-line)
(global-set-key (kbd "C-+") 'other-window)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)

(autoload 'css-mode "css-mode")

(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)


(add-hook 'nxml-mode-hook 
	  '(lambda ()
	     (define-key nxml-mode-map [C-tab] 'nxml-complete)))
(add-hook 'emacs-lisp-mode-hook 
	  '(lambda () 
	     (define-key emacs-lisp-mode-map [C-tab] 'lisp-complete-symbol)))

(require 'snippet)

(snippet-with-abbrev-table 
 'c-mode-abbrev-table
 ("tc" . "START_TEST ($${test_name})
{
$.fail(\"+++\");
}
END_TEST
")
 ("inc" . "#include \"$${header}.h\"")
 ("ins" . "#include <$${header}.h>")
 ("hf" . "#ifndef $${name}_H
#define $${name}_H

$.

#endif /* $${name}_H */
")
 ("tca" . "tcase_add_test(tc_core, test_$${name});$>"))

(snippet-with-abbrev-table 
 'ruby-mode-abbrev-table
 ("defm" . "def$.

  end"))

(add-hook 'cperl-mode-hook 
	  '(lambda () 
	     (snippet-with-abbrev-table 
	      'cperl-mode-abbrev-table
	      ("head" . "=head3$.\n\n=cut\n"))))

(require 'slime)
;;; Optionally, specify the lisp program to use. Default is "lisp"
;; ;(setq inferior-lisp-program "cmucl") 
;; ;(setq inferior-lisp-program "clisp -K full") 
(setq inferior-lisp-program "sbcl")
;; ;(setq inferior-lisp-program "guile")
;; ;(setq inferior-lisp-program "scheme48")
(slime-setup)

(add-hook 'slime-mode-hook
	  (lambda ()
	    (unless (slime-connected-p)
	      (save-excursion (slime)))))

(setq slime-net-coding-system 'utf-8-unix)

(require 'ruby-mode)

(setq auto-mode-alist (cons '("\\.cap\\'" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rb\\'" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rake\\'" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rhtml\\'" . html-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.smil\\'" . sgml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.asd\\'" . lisp-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.css\\'" . css-mode) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(add-to-list 'auto-mode-alist '("Portfile" . tcl-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((cperl-indent-level . 4) (cperl-indent-level . 2)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :family "apple-monaco")))))
