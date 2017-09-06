; emacs init.el - Florian Ebeling

(add-to-list 'load-path "~/.emacs.d/lisp")

(load "personal/load-path")
(load "personal/autoload")
(load "personal/external")
(load "personal/support")
(load "personal/commands")
(load "personal/settings")
(load "personal/file-types")
(load "personal/mail-setup")
(load "personal/languages")

(set-cursor-color "gray46")

(setq ns-pop-up-frames nil)

(when (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t))

(ido-mode)

(global-hl-line-mode)

(defun xah-syntax-color-hex ()
  "Syntax color text of the form 「#ff1100」 in current buffer.
URL `http://ergoemacs.org/emacs/emacs_CSS_colors.html'
Version 2015-06-11"
  (interactive)
  (font-lock-add-keywords
   nil
   '(("#[ABCDEFabcdef[:digit:]]\\{6\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background (match-string-no-properties 0)))))))
  (font-lock-fontify-buffer))

(add-hook 'web-mode-hook 'xah-syntax-color-hex)
(add-hook 'less-css-mode-hook 'xah-syntax-color-hex)

(defun febeling-eldoc-argument-list (string)
  "Upcase and fontify STRING for use with `eldoc-mode'."
  (propertize (downcase string)
              'face 'font-lock-variable-name-face))
(setq eldoc-argument-case 'febeling-eldoc-argument-list)

(add-hook 'objc-mode-hook
          '(lambda ()
             (setq-default c-basic-offset 4)))

(setq auto-install-directory "~/.emacs.d/auto-install-directory/")

(setq indent-tabs-mode nil)

(setq ispell-program-name "/usr/local/bin/aspell")
(setq ispell-dictionary "en")

;;(defalias 'yes-or-no-p 'y-or-n-p)

;; Edit Chromium textareas with Emacs
(when (locate-library "edit-server")
  (require 'edit-server)
  ;;(setq edit-server-new-frame nil)
  (edit-server-start))

(add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes

(add-hook 'scss-mode '(lambda ()
                        (setq scss-compile-at-save nil)))

(add-hook 'org-mode-hook
          '(lambda () (local-set-key (kbd "C-S-<up>") 'ibuffer)))

(add-hook 'haskell-mode-hook
          '(lambda ()
             (turn-on-haskell-indentation)
             (turn-on-haskell-doc)
             (turn-on-haskell-decl-scan)))

(require 'paredit)
;;(require 'yasnippet)
(require 'ido)
(require 'yaml-mode)
(require 'editorconfig)

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

;;; This makes collections indent in a sane way in
;;; ruby-mode.
(add-hook 'ruby-mode-hook
          '(lambda ()
             (setq ruby-deep-indent-paren nil)))

;(add-hook 'org-)

;;; Requires:

(require 'ruby-test)

(defun coffee-custom ()
  "coffee-mode-hook"
  (define-key coffee-mode-map [(meta r)] 'coffee-compile-buffer))

(add-hook 'coffee-mode-hook '(lambda () (coffee-custom)))

;;; Support Functions

;;; Commands

(defun insert-line-number ()
  (interactive)
  (insert " " (number-to-string (line-number-at-pos))))

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

(global-set-key (kbd "C-x w") 'whitespace-mode)

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

(setq make-backup-files nil)
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

;; (setq visible-bell t) broken for osx 10.11

(setq visible-bell nil)
(setq ring-bell-function
      (lambda ()
        (invert-face 'mode-line)
        (run-with-timer 0.1 nil 'invert-face 'mode-line)))


;; emacsclient
(require 'server)
(unless (server-running-p)
  (server-start))

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
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(blink-cursor-alist (quote ((nil))))
 '(blink-cursor-mode nil)
 '(custom-enabled-themes (quote (misterioso)))
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "ff02e8e37c9cfd192d6a0cb29054777f5254c17b1bf42023ba52b65e4307b76a" "3dafeadb813a33031848dfebfa0928e37e7a3c18efefa10f3e9f48d1993598d3" "e80932ca56b0f109f8545576531d3fc79487ca35a9a9693b62bf30d6d08c9aaf" "a530c409754f051ff406059ab7199d3fc6f14183426734ca5739a76cf9989236" "5e067e71f4bfe1e1a696370dd861b7939ac283f19e1584f8e01e61c8c0bc729d" default)))
 '(fci-rule-color "#383838")
 '(flycheck-disabled-checkers (quote (javascript-eslint)))
 '(grep-find-ignored-directories
   (quote
    ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "vendor" "log")))
 '(js2-missing-semi-one-line-override t)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(ns-alternate-modifier (quote super))
 '(ns-command-modifier (quote meta))
 '(package-selected-packages
   (quote
    (magit green-screen-theme ac-js2 rnc-mode rjsx-mode package-build shut-up epl commander f dash s zencoding-mode zenburn-theme yasnippet yari yard-mode yaml-mode web-mode toml-mode textmate swiper swift-mode sr-speedbar smart-mode-line scss-mode sass-mode rust-mode ruby-tools ruby-test-mode ruby-hash-syntax ruby-end ruby-electric ruby-block rubocop rinari reveal-in-finder restclient relax rails-log-mode projectile project-local-variables php-mode paredit nyan-mode multiple-cursors markdown-mode less-css-mode jumpc json-mode js2-mode jasminejs-mode highline helm-cmd-t haskell-mode hackernews groovy-mode grizzl go-mode gist full-ack free-keys fm flycheck flx-ido find-file-in-project expand-region ess enh-ruby-mode emoji-display edts editorconfig-core editorconfig dockerfile-mode debbugs csv-mode cmake-mode cm-mode cider cask babel avy anything-complete anything ansible-doc ansible angular-snippets anaphora alchemist ag ack 2048-game)))
 '(projectile-global-mode t)
 '(safe-local-variable-values
   (quote
    ((js-run . "swank-handler-tests.js")
     (Syntax . ANSI-Common-Lisp)
     (Base . 10)
     (eval when
           (require
            (quote rainbow-mode)
            nil t)
           (rainbow-mode 1))
     (eval when
           (and
            (buffer-file-name)
            (string-match-p "\\.h\\'"
                            (buffer-file-name))
            (not
             (string-match-p "/gnulib/"
                             (buffer-file-name))))
           (c++-mode)
           (c-set-style "gnu"))
     (js2-basic-offset . 2)
     (js-indent-level . 2)
     (erlang-indent-level . 4)
     (sh-basic-offset . 3)
     (encoding . utf-8)
     (cperl-indent-level . 4)
     (cperl-indent-level . 2))))
 '(scss-compile-at-save nil)
 '(send-mail-function (quote smtpmail-send-it))
 '(show-trailing-whitespace nil)
 '(speedbar-show-unknown-files t)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))

(put 'erase-buffer 'disabled nil)
(put 'upcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "IndianRed3"))))
 '(magit-item-highlight ((t nil))))

;; smart mode line
(sml/setup)
(put 'downcase-region 'disabled nil)
