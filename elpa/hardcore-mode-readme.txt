Entering hardcore-mode will disable arrow keys, backspace and return.

* Use C-f/b/p/n instead of right/left/up/down
* Use C-h instead of backspace
* Use C-m or C-j instead of return

To use C-h instead of backspace, you might need to redefine C-h:

    ;; Use shell-like backspace C-h, rebind help to F1
    (define-key key-translation-map [?\C-h] [?\C-?])
    (global-set-key (kbd "<f1>") 'help-command)

If hardcore-mode is too hardcore for you, you can add these before
you require the mode:

    (setq too-hardcore-backspace t)
    (setq too-hardcore-return t)
    (require 'hardcore-mode)
    (global-hardcore-mode)

These are the settings I am using at the time. Still not hardcore enough. ^^

Code:

(defcustom too-hardcore-backspace nil
  "On non-nil value, don't disable backspace in hardcore mode.")

(defcustom too-hardcore-return nil
  "On non-nil value, don't disable return in hardcore mode.")

(defvar hardcore-mode-map nil
  "Keymap for hardcore emacs minor mode.")

(if hardcore-mode-map
    nil
  (setq hardcore-mode-map (make-sparse-keymap))
  (define-key hardcore-mode-map
    (kbd "<up>") (lambda ()
                   (interactive)
                   (message "Arrow key navigation is disabled. Use C-p instead.")))
  (define-key hardcore-mode-map
    (kbd "<down>") (lambda ()
                     (interactive)
                     (message "Arrow key navigation is disabled. Use C-n instead.")))
  (define-key hardcore-mode-map
    (kbd "<left>") (lambda ()
                     (interactive)
                     (message "Arrow key navigation is disabled. Use C-b instead.")))
  (define-key hardcore-mode-map
    (kbd "<right>") (lambda ()
                      (interactive)
                      (message "Arrow key navigation is disabled. Use C-f instead.")))
  (unless too-hardcore-backspace
    (define-key hardcore-mode-map
      (kbd "<backspace>") (lambda ()
                            (interactive)
                            (message "Backspace is disabled. Use C-h instead."))))
  (unless too-hardcore-return
    (define-key hardcore-mode-map
      (kbd "<return>") (lambda ()
                         (interactive)
                         (message "Return is disabled. Use C-m or C-j instead.")))))

(define-minor-mode hardcore-mode
  "Hardcore emacs minor mode."
  nil " hc" hardcore-mode-map)

(define-globalized-minor-mode global-hardcore-mode
  hardcore-mode hardcore-mode)

(provide 'hardcore-mode)
hardcore-mode.el ends here