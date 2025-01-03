
(ido-mode)

;;(require 'yasnippet)
;;(yas-global-mode)

;; Find In Project - fuzzy find
;; https://github.com/grizzl/fiplr
;;
(global-set-key (kbd "C-x f") 'fiplr-find-file)

(setq fiplr-ignored-globs '((directories (
                                          "_build"
                                          "deps"
                                          "node_modules"
                                          ".git"
                                          ".svn"
                                          ))
                            (files (
                                    "*.jpg"
                                    "*.png"
                                    "*.zip"
                                    "*~"
                                    ))))

(setq enh-ruby-deep-indent-paren nil)

;; (define-derived-mode fundamental-ansi-mode fundamental-mode "fundamental ansi"
;;   "Fundamental mode that understands ansi colors."
;;   (require 'ansi-color)
;;   (ansi-color-apply-on-region (point-min) (point-max)))
;;; -- still showing ^M characters, why?

;; (global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-M-;") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "M-g g") 'avy-goto-line)

;; (require 'jumpc)
;; (jumpc)
;; (global-set-key (kbd "C-i") 'jumpc-jump-backward)
;; (global-set-key (kbd "<f9>") 'jumpc-jump-forward)

(setq debug-on-error nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq sr-speedbar-auto-refresh nil)
(global-set-key (kbd "s-s") 'sr-speedbar-toggle)
(setq sr-speedbar-right-side nil)

(global-set-key (kbd "M-s") 'er/expand-region)

(global-set-key (kbd "M-T") 'helm-cmd-t)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; web-mode

(add-hook 'web-mode-hook
          (lambda ()
            ;; Hooks for Web mode.
            (message "* web-mode-hook settings running")
            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-css-indent-offset 2)
            (setq web-mode-code-indent-offset 2)))

;; org

(require 'org)

(setq org-directory "~/org")

(setq org-default-notes-file
      (concat org-directory "/notes.org"))

(setq org-capture-templates
      '(("t" "TODO" entry
         (file+headline (concat org-directory "/todos.org") "Task")
         "* TODO %?\n %i\n"
         :kill-buffer t)
        ("r" "To research" checkitem
         (file+headline (concat org-directory "/todos.org") "To Research")
         "%?\n\n%T\n"
         :kill-buffer t :prepend t)
        ("l" "Link" plain
         (file (concat org-directory "/links.org"))
         "- %x%?\n"
         :kill-buffer t)
        ("n" "Note" plain
         (file "~/Schreiben/notes.org")
         "\n%?\n\n%T\n"
         :kill-buffer t :prepend t)
        ("z" "Quote" plain
         (file+headline (concat "~/Schreiben/quotes.org") "Quotes")
         "%c%x%?\n\n  --"
         :kill-buffer t :prepend t :empty-lines 1)))

(define-key global-map "\C-cc" 'org-capture)

;; (helm :sources
;;       (helm-build-sync-source "test"
;;         :fuzzy-match t)
;;       :buffer "*helm test*")

;;(require 'projectile)
;;(projectile-global-mode)
