(setq make-backup-files nil)
(setq default-case-fold-search t)
(setq auto-compression-mode t)

(global-font-lock-mode)
(show-paren-mode)


;(add-hook 'text-mode-hook 'turn-on-auto-fill)


(setq load-path (cons "~/.emacs.d" load-path))
(require 'ruby-mode)
(setq auto-mode-alist (cons '("\\.rb\\'" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rhtml\\'" . html-mode) auto-mode-alist))

(defun insert-date ()
  "Insert the current date at point"
  (interactive)
  (insert (format-time-string "%d.%m.%y")))


(setq default-frame-alist '((top . 1) (left . 1) (width . 120) (height . 52)))


(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) 
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1)) 
;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1)) 


;; enable copy-paste within X Window under Linux
(setq x-select-enable-clipboard t)
(if x-no-window-manager
    (progn 
      (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
      (message "cut-and-paste with x enabled")))


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


(add-to-list 'load-path "~/Development/Lisp/slime-cvs")
;; Optionally, specify the lisp program to use. Default is "lisp"
;(setq inferior-lisp-program "cmucl") 
;(setq inferior-lisp-program "clisp -K full") 
(setq inferior-lisp-program "sbcl")
(condition-case ()
    (progn
      (require 'slime)
      (slime-setup))
  (error (message "  slime not present - error on loading")))

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
