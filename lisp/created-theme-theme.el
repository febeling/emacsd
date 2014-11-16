(deftheme created-theme
  "Created 2014-11-15.")

(custom-theme-set-variables
 'created-theme
 '(Man-width nil)
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-safe-themes (quote ("5e067e71f4bfe1e1a696370dd861b7939ac283f19e1584f8e01e61c8c0bc729d" default)))
 '(grep-find-ignored-directories (quote ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "vendor" "log")))
 '(ns-alternate-modifier (quote super))
 '(ns-command-modifier (quote meta))
 '(safe-local-variable-values (quote ((eval when (and (buffer-file-name) (string-match-p "\\.h\\'" (buffer-file-name)) (not (string-match-p "/gnulib/" (buffer-file-name)))) (c++-mode) (c-set-style "gnu")) (js2-basic-offset . 2) (erlang-indent-level . 4) (sh-basic-offset . 3) (encoding . utf-8) (cperl-indent-level . 4) (cperl-indent-level . 2))))
 '(send-mail-function (quote smtpmail-send-it))
 '(show-trailing-whitespace nil)
 '(column-number-mode t)
 '(blink-cursor-mode nil))

(custom-theme-set-faces
 'created-theme
 '(magit-item-highlight ((t nil))))

(provide-theme 'created-theme)
