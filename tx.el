;; tool extension functions
;; 01.03.2007

(defconst *whitespace* '(?  ?\t ?\n))

(defun is-whitespace (chr)
  (typecase chr
    (character (member chr *whitespace*))
    (string (if (= 1 (length chr))
		(is-whitespace (string-to-char chr))
	      (error "Pass character or string of length 1")))))

(defun trim-left (s)
  (if (not (is-whitespace (aref s 0)))
      s
    (trim-left (substring s 1))))

(defun trim-right (s)
  (let ((i (1- (length s))))
  (if (not (is-whitespace (aref s i)))
      s
    (trim-right (substring s 0 i)))))

(defun trim (s)
  (trim-right (trim-left s)))
  
(defun strip-last (s)
  (substring s 0 (1- (length s))))

(defun got (s)
  (and s (> (length s) 0)))

(defun del-to-char (arg char)
  (delete-and-extract-region (point)
			     (progn
			       (search-forward
				(char-to-string char) nil nil arg)
			       (point))))

(defun tr (s from to &optional (start 0))
  (if (< start (length s))
      (progn
	(when (char-equal from (aref s start))
	  (aset s start to))
	(tr s from to (1+ start))))
  s)

;; max 64 characters
;; no hyphens, rather underscores
(defun colname (name)
  (downcase (concat "\"j2me_" (tr (substring (tr name ?- ?_ 0) 1) ?. ?_ 0))))

(defun tx-disp ()
  (interactive)
  (unless (bolp) 
    (beginning-of-line))
  (let (name
	type
	sample
	displayname
	display
	comment)
    (setf name (strip-last (del-to-char 1 ?,)))
    (setf type (strip-last (del-to-char 1 ?,)))
    (del-to-char 1 ?,)
    (setf sample (strip-last (del-to-char 1 ?,)))
    (setf comment (strip-last (del-to-char 1 ?,)))
    (delete-and-extract-region (point)
			       (progn
				 (end-of-visible-line)
				 (point)))
    (message "name: %s type: %s sample: %s displayname: %s comment: %s" name type sample displayname comment)
    (when (got name)
      (insert (concat (format "\t<attr name=%s type=%s " (colname name) (if (string= "\"boolean\"" type) "\"tinyint(1)\"" type))
		      (if (got sample) (format "sample=%s " sample) "")
		      (if (got comment) (format "comment=%s " comment) "")
		      (if (string= "\"boolean\"" type) "display=\"checkbox\" " "")
		      "/>")))
    (forward-line 1)))

(defun tx-java ()
  (interactive)
  (unless (bolp) 
    (beginning-of-line))
  (let (name
	type
	sample
	displayname
	display
	comment)
    (setf name (strip-last (del-to-char 1 ?,)))
    (setf type (strip-last (del-to-char 1 ?,)))
    (del-to-char 1 ?,)
    (setf sample (strip-last (del-to-char 1 ?,)))
    (setf comment (strip-last (del-to-char 1 ?,)))
    (delete-and-extract-region (point)
			       (progn
				 (end-of-visible-line)
				 (point)))
    (message "name: %s type: %s sample: %s displayname: %s comment: %s" name type sample displayname comment)
    (when (got name)
      (insert (concat (format "\t<attr name=%s type=%s " (colname name) (if (string= "\"boolean\"" type) "\"tinyint(1)\"" type))
		      (if (got sample) (format "sample=%s " sample) "")
		      (if (got comment) (format "comment=%s " comment) "")
		      (if (string= "\"boolean\"" type) "display=\"checkbox\" " "")
		      "/>")))
    (forward-line 1)))

(global-set-key [f4] 'tx-disp)







