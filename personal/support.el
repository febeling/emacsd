;;; Utilities supporting elisp programming

;; This should have authorship credit, but I don't know anylonger
;; where it came from.
(defun shuffle-list (list)
  "Randomly permute the elements of LIST.
All permutations equally likely."
  (let ((i 0)
        j
        temp
        (len (length list)))
    (while (< i len)
      (setq j (+ i (random (- len i))))
      (setq temp (nth i list))
      (setcar (nthcdr i list) (nth j list))
      (setcar (nthcdr j list) temp)
      (setq i (1+ i))))
  list)

(defun odd-p (i) (= 1 (mod i 2)))

(defun even-p (i) (= 0 (mod i 2)))

(defun join-string (list &optional separator omit-nulls)
  "Join list to string using SEPARATOR, and drop NIL elements if
OMIT-NULLS is given."
  (if omit-nulls
      (setq list (compact list)))
  (message "%s" list)
  (mapconcat 'identity list separator))

(defun compact (list)
  "Copy of LIST without NIL elements"
  (delq nil (copy-tree list))
  list)

(defun string-prefix-p (string prefix)
  "Does STRING start with PREFIX?"
  (string= (substring string 0 (length prefix))
           prefix))

(defun string-suffix-p (string suffix)
  "Does STRING end with SUFFIX?"
  (string= (substring string (- (length suffix)))
           suffix))
