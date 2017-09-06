(message "* javascript.el running")

;;; Reset the hook
;;;
(setq js-mode-hook nil)

(setq js-indent-level 2)

(add-hook 'js-mode-hook
          (lambda ()
            (message "* js-mode-hook running")

            (setq js-indent-level 2)
            
            ;;(setq flycheck-checker 'javascript-standard)
            ;;; This is necessary to only have flycheck check in JS, not
            ;;; .json files
            ;; (when (equal (file-name-extension (buffer-file-name (current-buffer)))
            ;;                 "js")
            ;;   (message "** activate flycheck javascript-standard")
            ;;   (flycheck-select-checker 'javascript-standard)
            ;;   (flycheck-mode)
            ;;   )
            ))

;;; React

(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

;(flycheck-define-checker jsxhint-checker
;  "A JSX syntax and style checker based on JSXHint."
;
;  :command ("jsxhint" "--harmony" source)
;  :error-patterns
;  ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
;  :modes (web-mode))

(add-hook 'web-mode-hook
          (lambda ()
            (message "* web-mode-hook running")
            (setq web-mode-code-indent-offset 2)
            (when (equal web-mode-content-type "jsx")
              ;; enable flycheck for .jsx
              (flycheck-select-checker 'jsxhint-checker)
              (flycheck-mode))))
