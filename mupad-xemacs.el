(provide 'mupad-xemacs)

(defun line-end-position (&optional n)
  (save-excursion
    (end-of-line)
    (point)))

(defun line-beginning-position (&optional n)
  (save-excursion
    (beginning-of-line)
    (point)))

(defun match-string-no-properties (num &optional string)
  (match-string num string))

;; Oh! The enormous hack!!!!
(provide 'lazy-lock)
