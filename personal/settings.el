
;;; Turn on paredit mode for any lisp mode
(dolist (x '(scheme emacs-lisp lisp clojure))
  (add-hook
   (intern (concat (symbol-name x) "-mode-hook")) 'turn-on-paredit))

(setq org-default-notes-file "~/Schreiben/notes.org")
(define-key global-map "\C-cc" 'org-capture)
