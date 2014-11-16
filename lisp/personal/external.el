;;; PATH environment variable and exec-path list setup to run external
;;; tools from inside emacs.

(setenv "PATH"
        (mapconcat 'identity
                   `("/usr/local/bin"
                     "/usr/local/share/npm/bin"
                     ,(getenv "HOME") "/.rbenv/shims"
                     ,(getenv "HOME") "/.rbenv/bin"
                     ,(getenv "PATH"))
                   ":"))

(add-to-list 'exec-path "/.rbenv/shims")
(add-to-list 'exec-path "/.rbenv/bin")
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/usr/local/share/npm/bin")
