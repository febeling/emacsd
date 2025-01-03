
;; http://www.emacswiki.org/emacs/ExecuteExternalCommand
;;
;; `standard --format --stdin`
;;
;; Still needs to be send back to buffer for replacing the original.
(defun shell-command-on-buffer ()
  "Asks for a command and executes it in inferior shell with current buffer
as input."
  (interactive)
  (shell-command-on-region
   (point-min) (point-max)
   (read-shell-command "Shell command on buffer: ")))

(defun resort ()
  "sort dashed string components"
  (interactive)
  (save-excursion
    (kill-sexp)
    (insert "\""
            (mapconcat 'identity
                       (sort (split-string (substring (car kill-ring-yank-pointer)
                                                      1
                                                      -1)
                                           "-")
                             'string<)
                       "-")
            "\"")))

(defun touch ()
  "updates mtime on the file for the current buffer"
  (interactive)
  (let ((command (concat "touch " (shell-quote-argument (buffer-file-name)))))
    (shell-command command)
    (clear-visited-file-modtime)
    (message command)))

(global-set-key (kbd "s-<return>") 'touch)

(defun remove-line-breaks ()
  "Remove line endings in a paragraph."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

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

;; from http://blog.zenspider.com/2007/09/emacs-is-ber.html
(defadvice find-file-at-point (around forward-line compile activate)
  (let ((line (and (looking-at ".*:\\([0-9]+\\)")
                   (string-to-number (match-string 1)))))
    ad-do-it
    (and line (forward-line line))))

(defadvice find-file (around forward-line compile activate)
  "Got to a line number in a file, if one is appended to the file
name (separated by a colon).

Example:
  ~/.emacs.d/init.el:19"
  (if (string-match "\\(.*\\):\\([0-9]+\\)$" filename)
      (let ((line (string-to-number (substring filename (match-beginning 2) (match-end 2))))
	    (filename (substring filename (match-beginning 1) (match-end 1))))
	ad-do-it
	(and line (forward-line line)))
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

(defun shuffle-line ()
  "Shuffle the words in a line. Useful for creating Keys.app
levels."
  (interactive)
  (let* ((b (line-beginning-position))
         (e (line-end-position))
         (l (delete-and-extract-region b e))
         (ws (split-string l))
         (ns (shuffle-list ws))
         (nl (mapconcat 'identity ns " ")))
    (insert "  ")
    (insert nl)))

;;; Paging through a dired listing with SPC (and Shift-SPC,
;;; backwards), showing each file's in another buffer

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

;;; Move rest of line above (mostly for moving comments at line ends
;;; to separate new line above)

(defun move-up-rest-of-line ()
  "Moves everthing from point to line end up into a new line
above the current one. This is nice for moving comments after a
line to extend them."
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

;;(global-set-key [C-S-up] 'buffer-select)

(defun insert-date ()
  "Insert the current date at point"
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(global-set-key (kbd "C-c d") 'insert-date)

(defun ido-goto-symbol ()
  "Will update the imenu index and then use ido to select a
   symbol to navigate to"
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))

                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))

                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))

                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))

(setq ido-enable-flex-matching t)

(global-set-key (kbd "M-n") 'ido-goto-symbol)

;; http://pages.sachachua.com/.emacs.d/Sacha.html#sec-1-3-3

(defun sacha/vsplit-last-buffer (prefix)
  "Split the window vertically and display the previous buffer."
  (interactive "p")
  (split-window-vertically)
  (other-window 1 nil)
  (unless prefix
    (switch-to-next-buffer)))

(defun sacha/hsplit-last-buffer (prefix)
  "Split the window horizontally and display the previous buffer."
  (interactive "p")
  (split-window-horizontally)
  (other-window 1 nil)
  (unless prefix (switch-to-next-buffer)))
;(bind-key "C-x 2" 'sacha/vsplit-last-buffer)
;(bind-key "C-x 3" 'sacha/hsplit-last-buffer)

(defun sacha/search-word-backward ()
  "Find the previous occurrence of the current word."
  (interactive)
  (let ((cur (point)))
    (skip-syntax-backward "w_")
    (goto-char
     (if (re-search-backward (concat "\\_<" (current-word) "\\_>") nil t)
         (match-beginning 0)
       cur))))

(defun sacha/search-word-forward ()
  "Find the next occurrence of the current word."
  (interactive)
  (let ((cur (point)))
    (skip-syntax-forward "w_")
    (goto-char
     (if (re-search-forward (concat "\\_<" (current-word) "\\_>") nil t)
         (match-beginning 0)
       cur))))

(defadvice search-for-keyword (around sacha activate)
  "Match in a case-insensitive way."
  (let ((case-fold-search t))
    ad-do-it))
(global-set-key '[s-up] 'sacha/search-word-backward)
(global-set-key '[s-down] 'sacha/search-word-forward)

;; Transpose stuff with M-t
;(bind-key "M-t" nil) ;; which used to be transpose-words
;(bind-key "M-t l" 'transpose-lines)
;(bind-key "M-t w" 'transpose-words)
;(bind-key "M-t t" 'transpose-words)
;(bind-key "M-t M-t" 'transpose-words)
;(bind-key "M-t s" 'transpose-sexps)

;; (helm :sources
;;       (helm-build-sync-source "test"
;;         :candidates '("foo" "foo bar" "foo bar foo"
;;                       "foo bar foo bar" "bar" "baz")
;;         :fuzzy-match t)
;;       :buffer "*helm test*")
