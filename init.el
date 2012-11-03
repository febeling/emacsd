; emacs init.el - Florian Ebeling

;; please remember eventually:
;; C-M-arrows: up, down, next-sib, prev-sib
;; C-M-SPC: mark sexp

;;; Todos:

(setq smtpmail-stream-type 'ssl)
(setq smtpmail-smtp-server "smtp.gmail.com")
(setq smtpmail-smtp-service 465)

(set-cursor-color "gray46")

(setq js-indent-level 2)

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/elpa")
(add-to-list 'load-path "~/.emacs.d/ruby-test-mode")
(add-to-list 'load-path "~/.emacs.d/zencoding")
(add-to-list 'load-path "~/.emacs.d/markdown-mode")

(autoload 'auto-install-from-emacswiki "auto-install")
(setq auto-install-directory "~/.emacs.d/auto-install-directory/")
(add-to-list 'load-path "~/.emacs.d/auto-install-directory/")

(when (load "package")
  (package-initialize)
  (setq package-archives '(("ELPA" . "http://tromey.com/elpa/") 
                           ("gnu" . "http://elpa.gnu.org/packages/")
                           ("marmalade" . "http://marmalade-repo.org/packages/"))))

(setq indent-tabs-mode nil)

(setq ispell-program-name "aspell")
(setq ispell-dictionary "en")

(defalias 'yes-or-no-p 'y-or-n-p)

(if (locate-library "edit-server")
    (progn
      (require 'edit-server)
;;      (setq edit-server-new-frame nil)
      (edit-server-start)))

(autoload 'git-blame-mode "git-blame" "Minor mode for incremental blame for Git." t)
(autoload 'zencoding-mode "zencoding-mode" "Minor mode for Zencoding HTML." t)

(add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes

(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)

(require 'paredit)
;;(require 'yasnippet)
(require 'ido)
(require 'yaml-mode)

(require 'anything)
;; (require 'anything-match-plugin)
(require 'anything-config)

(defun turn-on-paredit ()
  (interactive)
  (paredit-mode t))

(dolist (x '(scheme emacs-lisp lisp clojure))
  (add-hook
   (intern (concat (symbol-name x) "-mode-hook")) 'turn-on-paredit))

(defun underline ()
  "Underline the current line with = characters, matching the length."
  (interactive)
  (save-excursion
    (end-of-line)
    (let ((right-pos (current-column)))
      (insert "\n")
      (insert (make-string right-pos ?=)))))

;; sudo-edit
(defun sudo-edit (&optional arg)
 (interactive "p")
 (if (or arg (not buffer-file-name))
     (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
   (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
;;(setq org-default-notes-file (concat org-directory "/todo.org"))
;;(define-key global-map "\C-cc" 'org-capture)
(setq org-log-done t)

;; (set-foreground-color "gainsboro")
;; (set-background-color "gray8")

;; from http://blog.zenspider.com/2007/09/emacs-is-ber.html
(defadvice find-file-at-point (around goto-line compile activate)
  (let ((line (and (looking-at ".*:\\([0-9]+\\)")
                   (string-to-number (match-string 1)))))
    ad-do-it
    (and line (goto-line line))))

(defadvice find-file (around goto-line compile activate)
  "Got to a line number in a file, if one is appended to the file
name (separated by a colon).

Example:
  ~/.emacs.d/init.el:19"
  (if (string-match "\\(.*\\):\\([0-9]+\\)$" filename)
      (let ((line (string-to-number (substring filename (match-beginning 2) (match-end 2))))
	    (filename (substring filename (match-beginning 1) (match-end 1))))
	ad-do-it
	(and line (goto-line line)))
    ad-do-it))

(defun port-open (name)
  "Open the portfile for named MacPorts port."
  (interactive "MPort: ")
  (let ((path (substring (shell-command-to-string (format "port file %s" name)) 0 -1)))
    (if (file-exists-p path)
	(find-file-other-window path))))

(defun gem-open (name)
  "Open named the ruby gem directory. Specify the require name,
not the gem's name in cases where they differ."
  (interactive "MGem: ")
  (let ((path (elt (split-string (shell-command-to-string (format "gem which %s" name)) "\n") 1)))
    (if (> (length path) 0)
	(find-file-other-window path)
      (message "Gem not found"))))

(defun rotate-yank-pointer-backward ()
  "Step through the kill-ring, or the emacs clip board, and show
the current content in the mini-buffer. Backwards."
  (interactive)
  (let ((text (current-kill -1)))
    (message "%s" text)))

(global-set-key [C-S-left] 'rotate-yank-pointer-backward)

(defun rotate-yank-pointer-forward ()
  "Step through the kill-ring, or the emacs clip board, and show
the current content in the mini-buffer. Forwards."
  (interactive)
  (let ((text (current-kill 1)))
    (message "%s" text)))

(global-set-key [C-S-right] 'rotate-yank-pointer-forward)

(defun shuffle-list (list)
  "Randomly permute the elements of LIST.
All permutations equally likely."
  (let ((i 0)
        j
        temp
        (len (length list)))
    (while (< i len)
      (setq j (+ i (random (- len i))))
      (setq temp (nth i list))
      (setcar (nthcdr i list) (nth j list))
      (setcar (nthcdr j list) temp)
      (setq i (1+ i))))
  list)

(defun shuffle-line ()
  (interactive)
  (let* ((b (line-beginning-position))
         (e (line-end-position))
         (l (delete-and-extract-region b e))
         (ws (split-string l))
         (ns (shuffle-list ws))
         (nl (mapconcat 'identity ns " ")))
    (insert "  ")
    (insert nl)))

(global-set-key (kbd "M-p") 'shuffle-line)
(global-set-key (kbd "S-M-t") 'transpose-words) ;; Avoid collision with textmate-mode find-in-project

;; Paging through a dired listing with SPC (and Shift-SPC, backwards)

(defun dired-next-item-or-descend ()
  "Easily step through dired view looking at each file or
directory, as a preview."
  (interactive)
  (dired-iterate-item-or-descend 'dired-next-line))

(defun dired-previous-item-or-descend ()
  "Easily step backwards through dired view looking at each file
or directory, as a preview."
  (interactive)
  (dired-iterate-item-or-descend 'dired-previous-line))

(defun dired-iterate-item-or-descend (move-function)
  (let ((file (dired-get-file-for-visit)))
    (cond
     ((file-directory-p file) (find-file file))
     ((and (file-exists-p file) (file-readable-p file))
      (save-selected-window (find-file-other-window file))
      (funcall move-function 1))
     (t (error "Neither readable file nor directory")))))

(add-hook 'dired-mode-hook
	  '(lambda ()
	     (define-key dired-mode-map " " 'dired-next-item-or-descend)
	     (define-key dired-mode-map (kbd "S-SPC") 'dired-previous-item-or-descend)))

(add-hook 'find-file-hook
          '(lambda ()
             (if (string-match "\\.level$" (buffer-file-name (current-buffer)))
                 (whitespace-mode 1))))

(add-hook 'yaml-mode-hook
          '(lambda ()
             (fill-mode)
             (make-local-variable 'fill-column)
	     (setq fill-column 35)))

;; Move rest of line above (mostly for moving comments at line ends)

(defun move-up-rest-of-line ()
  "Moves everthing from point to line end up into a new line
directly above the current one. This is nice for moving comments
after a line to extend them."
  (interactive)
  (insert "\n")
  (transpose-lines 1)
  (forward-line -2))

(global-set-key [S-return] 'move-up-rest-of-line)

(defun insert-defun-in-repl ()
  (interactive)
  (let ((form (slime-defun-at-point)))
    (save-excursion
      (switch-to-buffer-other-window "*slime-repl clojure*")
      (insert form))))

(add-hook 'clojure-mode-hook
	  '(lambda ()
	     (define-key clojure-mode-map "\e\C-z" 'insert-defun-in-repl)))

;;; Requires:

(require 'haml-mode "haml.el")
(require 'ruby-mode)
(require 'ruby-test)
(require 'oddmuse)
(oddmuse-mode-initialize)

(add-to-list 'load-path "~/.emacs.d/coffee-mode")
(require 'coffee-mode)
(add-to-list 'load-path "~/.emacs.d/rhtml")
(require 'rhtml-mode)

(defun coffee-custom ()
  "coffee-mode-hook"
  (define-key coffee-mode-map [(meta r)] 'coffee-compile-buffer))

(add-hook 'coffee-mode-hook '(lambda () (coffee-custom)))

;;;

(defun odd-p (i) (= 1 (mod i 2)))

(defun even-p (i) (= 0 (mod i 2)))

(defun pull-line-up ()
  "Drags a line up by one, and moves point accordingly."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(global-set-key [M-up] 'pull-line-up)
(global-set-key (kbd "C-S-p") 'pull-line-up)

(defun pull-line-down ()
  "Drags a line down by one, and moves point accordingly."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(global-set-key [M-down] 'pull-line-down)
(global-set-key (kbd "C-S-n") 'pull-line-down)

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

;; make mac option key the Hyper
;;(setq mac-option-modifier 'hyper)

(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

(global-set-key (kbd "H-j") (lambda () (interactive) (insert "{}") (backward-char 1)))
(global-set-key (kbd "H-k") (lambda () (interactive) (insert "()") (backward-char 1)))
(global-set-key (kbd "H-l") (lambda () (interactive) (insert "[]") (backward-char 1)))

;(global-set-key (kbd "C-c b") 'ruby-break)
(global-set-key (kbd "C-c b") 'sgml-skip-tag-backward) ;; definition compatible with tmux
(global-set-key (kbd "C-c C-b") 'ruby-break)

(global-set-key (kbd "C-x w") 'whitespace-mode)

;; configuration section

(setq show-paren-style 'expression)
(setq default-frame-alist '((top . 1) (left . 1) (width . 130) (height . 50)))

(add-to-list 'Info-default-directory-list "/opt/local/share/info/")

;; workstation-specific settings.
(let ((hostname (system-name)))
  (cond
   ((or (equal hostname "flomac.local")
        (equal hostname "flomac"))
    (message "Initializing for host %s" hostname)
    (ido-mode)
    (setq default-frame-alist '((top . 1) (left . 1)
				(width . 125) (height . 35)))
    (setq mail-host-address "florian.ebeling@gmail.com")
    ;; erlang
    ;; /opt/local/lib/erlang/lib/tools-2.6.6.5/emacs/erlang.el
;    (setq otp-path "/opt/local/lib/erlang/lib/tools-2.6.6.5/emacs/")
;    (setq load-path (cons otp-path load-path))
;    (setq erlang-root-dir "/opt/local/bin")
;    (setq exec-path (cons "/opt/local/lib/erlang" exec-path))
;    (require 'erlang-start)
    )))

;;(setq make-backup-files nil)
(setq Man-width 70)
(setq default-case-fold-search t)
(setq auto-compression-mode t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
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
(if (boundp 'x-no-window-manager)
    (progn 
      (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
      (message "cut-and-paste with x enabled")))

;;; general key remapping

;; free strokes
;; C-# -> as new/custom duplicate-line keybinding
;; M-p
;; C-f8 -> make ruby local_var from region

(global-set-key (kbd "C-.") 'find-file-at-point)
;;(global-set-key [C-S-left] 'previous-buffer)
;;(global-set-key [C-S-right] 'next-buffer)
(global-set-key (kbd "M-RET") 'magit-status)

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
(autoload 'yaml-mode "yaml-mode")

(add-hook 'css-mode-hook
	  '(lambda ()
         (setq css-indent-level 2)))

(autoload 'yaml-mode "yaml-mode")
(autoload 'feature-mode "feature-mode")

(add-hook 'nxml-mode-hook
	  '(lambda ()
	     (define-key nxml-mode-map [C-tab] 'nxml-complete)))
(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()
	     (define-key emacs-lisp-mode-map [C-tab] 'lisp-complete-symbol)))

;; Modeline from the macports guide:
;; # -*- coding: utf-8; mode: tcl; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
(add-hook 'tcl-mode-hook
	  '(lambda ()
	     (message "[Running tcl-mode hook: '%s']" (current-buffer))
	     (make-local-variable 'coding)
	     (setq coding "utf-8")
	     (make-local-variable 'tab-width)
	     (setq tab-width 4)
	     (make-local-variable 'indent-tabs-mode)
	     (setq indent-tabs-mode nil)
	     (make-local-variable 'c-basic-offset)
	     (setq c-basic-offset 4)
	     ;; (setq tcl-indent-level 8)
	     ;; (setq tcl-continued-indent-level 8)
	     ;; (setq tcl-tab-always-indent 'smart)
	     ))

;;; File extension mode settings

(add-to-list 'auto-mode-alist '("\\.cap$"      . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$"  . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile"     . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile"      . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile"      . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile"  . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$"     . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rhtml$"    . html-mode))
(add-to-list 'auto-mode-alist '("\\.s?css$"    . css-mode))
(add-to-list 'auto-mode-alist '("\\.r[hl]$"    . c-mode))
(add-to-list 'auto-mode-alist '("\\.xml$"      . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.haml$"     . haml-mode))
(add-to-list 'auto-mode-alist '("\\.js$"       . js-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$ ."   . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml$ ."    . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.feature$"  . feature-mode))
(add-to-list 'auto-mode-alist '("\\.level$"    . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.pde$"      . java-mode))
(add-to-list 'auto-mode-alist '("\\.coffee$"   . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.rest$"     . rst-mode))
(add-to-list 'auto-mode-alist '("\\.rst$"      . rst-mode))
(add-to-list 'auto-mode-alist '("\\.md"        . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown"  . markdown-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-width nil t)
 '(blink-cursor-mode nil)
 '(ns-alternate-modifier (quote super))
 '(ns-command-modifier (quote meta))
 '(safe-local-variable-values (quote ((js2-basic-offset . 2) (erlang-indent-level . 4) (sh-basic-offset . 3) (encoding . utf-8) (cperl-indent-level . 4) (cperl-indent-level . 2))))
 '(send-mail-function (quote smtpmail-send-it))
 '(speedbar-show-unknown-files t))


(put 'erase-buffer 'disabled nil)
(put 'upcase-region 'disabled nil)
