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


(defface font-lock-builtin-face
  '((((type tty) (class color)) (:foreground "blue" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "Orchid"))
    (((class color) (background dark)) (:foreground "LightSteelBlue"))
    (t (:bold t)))
  "Font Lock mode face used to highlight builtins."
  :group 'font-lock-highlighting-faces)

(defface font-lock-constant-face
  '((((type tty) (class color)) (:foreground "magenta"))
    (((class grayscale) (background light))
     (:foreground "LightGray" :bold t :underline t))
    (((class grayscale) (background dark))
     (:foreground "Gray50" :bold t :underline t))
    (((class color) (background light)) (:foreground "CadetBlue"))
    (((class color) (background dark)) (:foreground "Aquamarine"))
    (t (:bold t :underline t)))
  "Font Lock mode face used to highlight constants and labels."
  :group 'font-lock-highlighting-faces)
