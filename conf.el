(setq make-backup-file nil)

(setq default-frame-alist '((top . 1) (left . 1) (width . 120) (height . 52)))

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) 
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1)) 
;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1)) 


;; enable copy-paste within X Window
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)


(fset 'to-java-string
   [?\C-a ?" ?\C-e ?" ?  ?+ down])
(global-set-key [f6] 'to-java-string)
(fset 'purge-line
   [?\C-a ?\C-  ?\C-n ?\C-c ?\C-k])
(global-set-key [\C-K] 'purge-line)


(global-set-key [f5] 'call-last-kbd-macro)
(global-set-key [f3] 'edit-last-kbd-macro)


;; C-# as new/custom duplicate-line keybinding  
;; M-p another free keybinding


(global-set-key "\C-c\C-m" 'execute-extended-command)
;(global-set-key "\C-x\C-m" 'execute-extended-command)


(global-set-key "\C-w" 'backward-kill-word)
;(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\C-x\C-k" 'kill-region)


(add-to-list 'load-path "~/Development/Lisp/slime-cvs")
;; Optionally, specify the lisp program to use. Default is "lisp"
(setq inferior-lisp-program "cmucl") 
;(setq inferior-lisp-program "clisp -K full") 
;(setq inferior-lisp-program "sbcl")
(require 'slime)
(slime-setup)

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
(require 'rails)





