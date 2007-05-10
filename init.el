(setq load-path (cons "~/.emacs.d" load-path))

(setq default-frame-alist '((top . 1) (left . 1) (width . 120) (height . 52)))

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

(let ((hostname (system-name)))
  (cond
   ((equal hostname "dev14.iconmobile.de")
    (message "Intializing for host %s" hostname)
    (require 'psvn)
    (find-file "~/TODO"))
   (nil ;; todo: somehow test for ubuntu laptop
    (message "Intializing for host %s" hostname)
    (let ((slime-dir-path "~/slime"))
      (if (file-exists-p slime-dir-path)
	  (add-to-list 'load-path slime-dir-path)
	(with-output-to-temp-buffer "*slime init warnings*" 
	  (princ (format "slime-dir-path does not exist: '%s'" slime-dir-path))))))
   ((equal hostname "flomac.local")
    (message "Initializing for host %s" hostname)
    (load "osx" t))))


;;(setq make-backup-files nil)
(setq default-case-fold-search t)
(setq auto-compression-mode t)
(setq-default uniquify-buffer-name-style 'post-forward)
(setq-default tab-width 8)

(global-font-lock-mode 1)
(show-paren-mode 1)

;; for emacsclient
(server-start) 

(defalias 'qrr 'query-replace-regexp)
(defalias 'qr 'query-replace)
(defalias 'cr 'comment-region)
(defalias 'ucr 'uncomment-region)
(defalias 'ir 'indent-region)

;(add-hook 'text-mode-hook 'turn-on-auto-fill)


(setq-default abbrev-file-name "~/.emacs.d/abbrev_defs")
(setq-default abbrev-mode t)
(read-abbrev-file)
(setq save-abbrevs nil)


(require 'ruby-mode)

(setq auto-mode-alist (cons '("\\.rb\\'" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rhtml\\'" . html-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.smil\\'" . sgml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.asd\\'" . lisp-mode) auto-mode-alist))

(defun insert-date ()
  "Insert the current date at point"
  (interactive)
  (insert (format-time-string "%d.%m.%y")))





(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) 
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1)) 
;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1)) 


;; enable copy-paste within X Window under Linux
(setq x-select-enable-clipboard t)
(if x-no-window-manager
    (progn 
      (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
      (message "cut-and-paste with x enabled")))

(defun scroll-up-1 ()
  (interactive)
  (scroll-up 1))

(defun scroll-down-1 ()
  (interactive)
  (scroll-down 1))

(define-key global-map [S-down] 'scroll-up-1)
(define-key global-map [S-up] 'scroll-down-1)

(fset 'to-java-string
      [?\C-a ?" ?\C-e ?" ?  ?+ down])
(global-set-key [f6] 'to-java-string)
(fset 'purge-line
      [?\C-a ?\C-  ?\C-n ?\C-c ?\C-k])
(global-set-key [\C-K] 'purge-line)
(fset 'mark-as-done
      [?\C-e ?  ?( ?\M-x ?i ?n ?s ?e ?r ?t ?- ?d ?a ?t ?e return ?) ?\C-a ?\C-k ?\C-k ?\C-s ?D ?O ?N ?E ?\C-m return ?\C-y ?\C-k ?\C-r ?T ?O ?D ?O ?\C-m ?\C-n])
(global-set-key [f2] 'mark-as-done)
(fset 'ruby-extract-local
   [?\C-x ?\C-k ?\C-p ?\C-e return ?\C-y ?\C-a tab ?= ?  ?\C-b ?\C-b])
(global-set-key [C-f6] 'ruby-extract-local)

(global-set-key [f5] 'call-last-kbd-macro)
(global-set-key [f3] 'edit-last-kbd-macro)
(global-set-key (kbd "C-S-l") 'goto-line)
(global-set-key (kbd "C-+") 'other-window)

;; free strokes
;; C-# -> as new/custom duplicate-line keybinding  
;; M-p 
;; C-. 
;; C-f8 -> make ruby local_var from region

(global-set-key "\C-c\C-m" 'execute-extended-command)
;(global-set-key "\C-x\C-m" 'execute-extended-command)

(global-set-key "\C-w" 'backward-kill-word)
;(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\C-x\C-k" 'kill-region)

(add-hook 'nxml-mode-hook '(lambda () (define-key nxml-mode-map [C-tab] 'nxml-complete)))

;; Snippets
(require 'snippet)
;; C
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
 ("def" . "def$.

  end"))

(add-hook 'cperl-mode-hook 
	  '(lambda () (snippet-with-abbrev-table 'cperl-mode-abbrev-table
						 ("head" . "=head3$.\n\n=cut\n"))))


(require 'slime)
;;; Optionally, specify the lisp program to use. Default is "lisp"
;(setq inferior-lisp-program "cmucl") 
;(setq inferior-lisp-program "clisp -K full") 
(setq inferior-lisp-program "sbcl")
;(setq inferior-lisp-program "guile")
;(setq inferior-lisp-program "scheme48")
(slime-setup)

(add-hook 'slime-mode-hook
	  (lambda ()
	    (unless (slime-connected-p)
	      (save-excursion (slime)))))

(setq slime-net-coding-system 'utf-8-unix)

;; from slime/HACKING:
(defun show-outline-structure ()
  "Show the outline-mode structure of the current buffer."
  (interactive)
  (occur (concat "^" outline-regexp)))



(add-to-list 'load-path "~/Development/emacs-rails-svn")
(defun try-complete-abbrev (old)
  (if (expand-abbrev) t nil))
(setq hippie-expand-try-functions-list
      '(try-complete-abbrev
	try-complete-file-name
	try-expand-dabbrev))
(condition-case ()
    (require 'rails)
  (error (message "  rails not present - error on loading")))

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
 )
