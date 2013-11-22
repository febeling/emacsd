
;;; Turn on paredit mode for any lisp mode
(dolist (x '(scheme emacs-lisp lisp clojure))
  (add-hook
   (intern (concat (symbol-name x) "-mode-hook")) 'turn-on-paredit))

