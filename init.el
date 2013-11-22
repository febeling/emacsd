; emacs init.el - Florian Ebeling

(add-to-list 'load-path "~/.emacs.d")

(setq smtpmail-stream-type 'ssl)
(setq smtpmail-smtp-server "smtp.gmail.com")
(setq smtpmail-smtp-service 465)

(set-cursor-color "gray46")

(setq js-indent-level 2)

(load "personal/load-path")
(load "personal/external")
(load "personal/support")
(load "personal/commands")
(load "personal/settings")
(load "personal/file-types")

(setq auto-install-directory "~/.emacs.d/auto-install-directory/")

(when (load "package")
  (package-initialize)
  (setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                           ("gnu" . "http://elpa.gnu.org/packages/")
                           ("melpa" . "http://melpa.milkbox.net/")
                           ("marmalade" . "http://marmalade-repo.org/packages/"))))

(setq indent-tabs-mode nil)

(setq ispell-program-name "/usr/local/bin/aspell")
(setq ispell-dictionary "en")

(defalias 'yes-or-no-p 'y-or-n-p)

(if (locate-library "edit-server")
    (progn
      (require 'edit-server)
;;      (setq edit-server-new-frame nil)
      (edit-server-start)))

(add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes

(add-hook 'scss-mode '(lambda ()
                        (setq scss-compile-at-save nil)))

(require 'paredit)
;;(require 'yasnippet)
(require 'ido)
(require 'yaml-mode)

(require 'anything)
(require 'anything-config)

(add-hook 'text-mode-hook
          '(lambda ()
             (define-key text-mode-map (kbd "M-s-^") 'remove-line-breaks)))

(require 'org-install)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
;;(setq org-default-notes-file (concat org-directory "/todo.org"))
;;(define-key global-map "\C-cc" 'org-capture)
(setq org-log-done t)


(global-set-key (kbd "M-p") 'shuffle-line)
(global-set-key (kbd "S-M-t") 'transpose-words) ;; Avoid collision with textmate-mode find-in-project
(global-set-key (kbd "C-x f") 'find-file-in-project)

(add-hook 'yaml-mode-hook
          '(lambda ()
             (fill-mode)
             (make-local-variable 'fill-column)
	     (setq fill-column 35)))


;;; This makes collections indent in a sane way in
;;; ruby-mode.
(add-hook 'ruby-mode-hook
          '(lambda ()
             (setq ruby-deep-indent-paren nil)))

;;; Requires:

(require 'ruby-test)

(defun coffee-custom ()
  "coffee-mode-hook"
  (define-key coffee-mode-map [(meta r)] 'coffee-compile-buffer))

(add-hook 'coffee-mode-hook '(lambda () (coffee-custom)))

;;; Support Functions

;;; Commands


(defun indent-buffer ()
  ;; Author: Mathias Creutz
  "Indent every line in the buffer."
  (interactive)
  (indent-region (point-min) (point-max) nil))

;; shadows tab-to-tab-stop binding.
(global-set-key "\M-i" 'indent-buffer)

(setq ibuffer-formats
      '((mark
         modified
         read-only " "
         ;; More width for name
         (name 40 40 :left :elide) " "
         (size 9 -1 :right) " "
         (mode 16 16 :left :elide) " "
         filename-and-process)
        (mark
         " "
         (name 16 -1)
         " "
         filename)))

(global-set-key [C-S-up] 'ibuffer)

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
;(global-set-key (kbd "C-c C-b") 'ruby-break)

(global-unset-key (kbd "C-z")) ; disable suspend-frame

;(global-set-key (kbd "C-x w") 'whitespace-mode)
(global-set-key (kbd "C-x w") (lambda () (setq 'show-trailing-whitespace t)))

;; show section

(setq show-paren-style 'expression)
(setq default-frame-alist '((top . 1) (left . 1) (width . 130) (height . 50)))

(add-to-list 'Info-default-directory-list "/opt/local/share/info/")

;; Workstation-specific settings.
(let ((hostname (system-name)))
  (cond
   ((or (equal hostname "flomac.local")
        (equal hostname "flomac"))
    (message "Initializing for host %s" hostname)
    (ido-mode)
    (setq default-frame-alist '((top . 1) (left . 1)
				(width . 125) (height . 35)))
    (setq mail-host-address "florian.ebeling@gmail.com"))))

(if (equal 'ns (window-system))
    (progn
      (load-theme 'wombat t)
      (set-face-attribute 'default nil :height 140)))

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

(add-hook 'css-mode-hook
	  '(lambda ()
         (setq css-indent-level 2)))

(add-hook 'nxml-mode-hook
	  '(lambda ()
	     (define-key nxml-mode-map [C-tab] 'nxml-complete)))

(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()
	     (define-key emacs-lisp-mode-map [C-tab] 'lisp-complete-symbol)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-width nil t)
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(blink-cursor-mode nil)
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes (quote ("5e067e71f4bfe1e1a696370dd861b7939ac283f19e1584f8e01e61c8c0bc729d" default)))
 '(grep-find-ignored-directories (quote ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "vendor" "log")))
 '(ns-alternate-modifier (quote super))
 '(ns-command-modifier (quote meta))
 '(safe-local-variable-values (quote ((js2-basic-offset . 2) (erlang-indent-level . 4) (sh-basic-offset . 3) (encoding . utf-8) (cperl-indent-level . 4) (cperl-indent-level . 2))))
 '(scss-compile-at-save nil)
 '(send-mail-function (quote smtpmail-send-it))
 '(show-trailing-whitespace t)
 '(speedbar-show-unknown-files t))

(put 'erase-buffer 'disabled nil)
(put 'upcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-item-highlight ((t nil))))
