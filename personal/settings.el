(setq debug-on-error t)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq sr-speedbar-auto-refresh nil)
(global-set-key (kbd "s-s") 'sr-speedbar-toggle)
(setq sr-speedbar-right-side nil)

(dolist (x '(scheme emacs-lisp lisp clojure))
  (add-hook
   (intern (concat (symbol-name x) "-mode-hook")) 'turn-on-paredit))

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
