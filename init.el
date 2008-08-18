;; emacs init.el - Florian Ebeling
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/ruby-test-mode")

(require 'ruby-test)

(defun odd-p (i) (= 1 (mod i 2)))

(defun even-p (i) (= 0 (mod i 2)))

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
;;(define-key global-map "\C-\S-n" 'scroll-up-1)

(defun scroll-down-1 ()
  (interactive)
  (scroll-down 1))

(define-key global-map [S-up] 'scroll-down-1)
;;(define-key global-map [C-S-P] 'scroll-down-1)

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
  (insert (format-time-string "%Y-%m-%d")))

(global-set-key (kbd "C-c d") 'insert-date)

(defvar ruby-break-file "~/dev/rptn/b" 
  "*The file to save breakpoint in")

(defun ruby-break ()
  "Toggle ruby-debug breakpoint for current line, writing to a buffer `~/.rdebugrc',
and save it."
  (interactive)
  (let ((breakpoint (format "break %s:%s" (buffer-file-name) (line-number-at-pos)))
	line-seen)
    (save-excursion
      (let ((breakspoint-buffer (find-file-noselect ruby-break-file)))
	(set-buffer breakspoint-buffer)
	(goto-char (point-min))
	(while (and (not line-seen) (< (point) (point-max)))
	  (let ((this-line (buffer-substring (point) (line-end-position))))
	    (setq line-seen (string= this-line breakpoint))
	    (if line-seen
		(progn
		  (delete-region (point) (line-end-position))
		  (if (char-equal (char-after) ?\n)
		      (delete-char 1))))
	    (forward-line)))
	(if (not line-seen)
	    (progn
	      (insert breakpoint)
	      (newline)))
	(basic-save-buffer)))
    (if line-seen
	(message "Breakpoint *off*")
      (message "Breakpoint *on*"))))

(global-set-key (kbd "C-c b") 'ruby-break)
(global-set-key (kbd "C-c C-b") 'ruby-break)

;; configuration section

(setq show-paren-style 'expression)
(setq default-frame-alist '((top . 1) (left . 1) (width . 130) (height . 50)))

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
;;     (setq otp-path "/opt/local/lib/erlang/lib/tools-2.5.5/emacs/")
;;     (setq load-path (cons otp-path load-path))
;;     (setq erlang-root-dir "/opt/local/bin")
;;     (setq exec-path (cons "/opt/local/lib/erlang" exec-path))
;;     (require 'erlang-start)
)
   ((equal hostname (or "ws-febeling.office.nugg.ad" "flomac-work.local"))
    (setq default-frame-alist '((top . 1) (left . 1) 
				(width . 220) (height . 30)))
    (message "Initializing for nugg.ad")
    (setq mail-host-address "florian.ebeling@nugg.ad")
;;    (find-file "~/dev/febeling/TODO")
    (setq otp-path "/opt/local/lib/erlang/lib/tools-2.6.1/emacs/")
    (setq load-path (cons otp-path load-path))
    (setq erlang-root-dir "/opt/local/lib/erlang")
    (setq exec-path (cons "/opt/local/lib/erlang" exec-path))
    (require 'erlang-start)
    ;; This is needed for Distel setup
    (let ((distel-dir "/Users/florianebeling/personal/distel/elisp"))
      (unless (member distel-dir load-path)
	;; Add distel-dir to the end of load-path
	(setq load-path (append load-path (list distel-dir)))))
    (require 'distel)
    (distel-setup)
    ;;
    )))

;;(setq make-backup-files nil)
(setq Man-width 70)
(setq default-case-fold-search t)
(setq auto-compression-mode t)
(setq-default uniquify-buffer-name-style 'post-forward)
(setq-default tab-width 8)
(setq-default scroll-margin 2)

(global-font-lock-mode 1)
(show-paren-mode 1)
(setq visible-bell t)

;; for emacsclient
(server-start) 

(defalias 'qrr 'query-replace-regexp)
(defalias 'qr 'query-replace)
(defalias 'cr 'comment-region)
(defalias 'ur 'uncomment-region)
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
(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-S-c C-S-c") 'uncomment-region)

(autoload 'css-mode "css-mode")

(add-hook 'nxml-mode-hook 
	  '(lambda ()
	     (define-key nxml-mode-map [C-tab] 'nxml-complete)))
(add-hook 'emacs-lisp-mode-hook 
	  '(lambda () 
	     (define-key emacs-lisp-mode-map [C-tab] 'lisp-complete-symbol)))

(require 'snippet)

(add-hook 'c-mode-hook 
	  '(lambda ()
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
	      ("tca" . "tcase_add_test(tc_core, test_$${name});$>"))))

(add-hook 'ruby-mode
	  '(lambda ()
	     (snippet-with-abbrev-table 
	      'ruby-mode-abbrev-table
	      ("defm" . "def$.

  end"))))

(add-hook 'cperl-mode-hook 
	  '(lambda () 
	     (snippet-with-abbrev-table 
	      'cperl-mode-abbrev-table
	      ("head" . "=head3$.\n\n=cut\n"))))

(add-hook 'tcl-mode-hook
	  '(lambda ()
	     (message "on tcl-mode-hook: %s" (current-buffer))
	     (setq tcl-indent-level 8)
	     (setq tcl-continued-indent-level 8)
	     (setq tcl-tab-always-indent 'smart)))

(require 'slime)
;;; Optionally, specify the lisp program to use. Default is "lisp"
;; ;(setq inferior-lisp-program "cmucl") 
;; ;(setq inferior-lisp-program "clisp -K full") 
(setq inferior-lisp-program "sbcl")
;; ;(setq inferior-lisp-program "guile")
;; ;(setq inferior-lisp-program "scheme48")
;;(setq inferior-lisp-program "~/dev/vendor/arc0/arc.sh")
(slime-setup)

(add-hook 'slime-mode-hook
	  (lambda ()
	    (unless (slime-connected-p)
	      (save-excursion (slime)))))

(setq slime-net-coding-system 'utf-8-unix)

(require 'oddmuse)
;; (setq url-proxy-services '(("http" . "your.proxy.host:portnumber")) ; if needed
(oddmuse-mode-initialize)

(require 'ruby-mode)

(setq auto-mode-alist (cons '("\\.cap\\'" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rb\\'" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("Rakefile" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rake\\'" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rhtml\\'" . html-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.smil\\'" . sgml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.asd\\'" . lisp-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.css\\'" . css-mode) auto-mode-alist))
;;(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(add-to-list 'auto-mode-alist '("Portfile" . tcl-mode))
(add-to-list 'auto-mode-alist '("\\.r[hl]\\'" . c-mode))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(Man-width nil)
 '(safe-local-variable-values (quote ((cperl-indent-level . 4) (cperl-indent-level . 2))))
 '(speedbar-show-unknown-files t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :family "apple-monaco"))))
 '(show-paren-match ((((class color) (background light)) (:background "lemon chiffon")))))

(put 'erase-buffer 'disabled nil)
