;;; mupad-fontification.el --- part of mupad.el dedicated to
;;;                            font-lock stuff.

;; Corresponds to mupad2-0.el
(provide 'mupad-fontification)

(defgroup mupad-font-lock nil
"MuPAD customization subgroup concerning colors and completion"
:group 'mupad :prefix "mupad-")

(defcustom mupad-fontify t
  "Set it to nil if you do not want hilighting, to nil otherwise."
:type 'boolean :group 'mupad-font-lock)
;; Also modified internally and becomes nil if no colors are available.
(defvar mupad-can-fontify t)
(defcustom mupad-fontify-function-names nil
  "t means that user defined functions used for completion are also
highlighted, not only when defined but also when used.
Faces used are mupad-function-name in definitions and mupad-user-def otherwise."
:type 'boolean :group 'mupad-font-lock)
(defcustom mupad-fontify-global-var nil
  "t means that user defined global variables used for completion are also
highlighted, not only when defined but also when used.
Faces used are mupad-global-var in definitions and mupad-user-def otherwise."
:type 'boolean :group 'mupad-font-lock)

(defconst mupad-regexp-identifier  "[a-zA-Z_][a-zA-Z_0-9]*"
"Regexp to match identifiers in mupad.")

(defconst mupad-function-def-start
  "^\\([a-zA-Z_]\\(\\w\\|::\\)*\\)[ \t\n]*:=[ \t\n]*\\(proc\\|func?\\)[ \t\n]*("
  "regexp that matches the beginning of a function definition (proc, fun, func).")
(defconst mupad-global-var-start
  "^\\([a-zA-Z]\\w*\\)[ \t\n]*:=[ \t\n]*\\([^ \t\npf]\\|p[^r]\\|pr[^o]\\|pro[^c]\\|proc[\\w]\\|f[^u]\\|fu[^n]\\|func[\\w]\\|fun[a-zA-Z0-9_]\\)"
  "regexp that matches the beginning of a global var definition.")
(defconst mupad-proc-def-start
  "\\<\\([a-zA-Z_]\\(::\\|\\w\\)*\\)[ \t\n]*:=[ \t\n]*\\(proc\\)[ \t\n]*("
  "regexp that matches the beginning of a procedure.")

(defvar mupad-function-defp nil
"Used for fontification. While parsing definitions,
a t value means we are looking at a function definition, while
a nil value means we are looking at a global variable definition.")
(defvar mupad-last-inputp nil
"Used for fontification. While parsing inputs a t value means we
are looking at the last input, while a nil value means we are
looking at a usual input.")
(defvar mupad-hash-parity nil
"Used for fontification. While parsing hash-comments,
a t value means we are within such a comment.")

(defvar mupad-loaded-libraries nil
"List of the packages names that have been exported in the heading
of a script. This variable is used only by font-lock.")
(defvar mupad-loaded-libraries-on-line nil
"List of the packages names that have been exported via the online
commands export/unexport. This variable is used only by font-lock.")
;; On top of this two variables, we also have
(defvar mupad-prefix-regexp nil
"Regexp matching all words in mupad-librairies-list.")
(defvar mupad-type-regexp nil
"Regexp matching all words in mupad-types-list.")
(defvar mupad-option-regexp nil
"Regexp matching all words in mupad-options-list.")
(defvar mupad-keyword-regexp nil
"Regexp matching all words in mupad-keywords-list.")
;; And mupad-all-completions which contains all words except compounds like Ax::normalRep.
;; And mupad-libraries-list which contains all libraries
;; And mupad-libraries-completion-alist, a list of conses: domaine.(methods)


(eval-and-compile
(defconst mupad-places
  (list
    ''mupad-comment  ''mupad-string  ''mupad-keyword ''mupad-options
    ''mupad-domain ''mupad-function-name  ''mupad-variable-name ''mupad-global-var
    ''mupad-type     ''mupad-info ''mupad-prompt ''mupad-last-prompt ''mupad-primitive-name
    ''mupad-user-def ''mupad-input ''mupad-last-input ''mupad-output))
(mapcar (lambda (aplace) (eval (list 'defvar (eval aplace) aplace)))
     (append mupad-places
       (list ''shade1 ''shade2 ''shade3 ''shade4 ''shade5))))

(defconst mupad-color-scheme-alist
 ;; Each scheme should at least contain 'mupad-prompt 'mupad-input 'mupad-type
 ;; 'mupad-info 'mupad-domain and 'mupad-options.
'(("Minimal" ; any title would do but check value of mupad-color-scheme exists.
    ('mupad-prompt
     '((((class grayscale) (background light)) (:foreground "LightGray" :bold t))
       (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
       (((class color) (background light)) (:foreground "Orchid"))
       (((class color) (background dark)) (:foreground "LightSteelBlue"))
       (t (:bold t)))
      "Face used in MuPAD to fontify prompt except the last one.")
    ('mupad-last-prompt
     '((((class grayscale) (background light)) (:foreground "LightGray" :bold t))
       (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
       (((class color) (background light)) (:foreground "Red"))
       (((class color) (background dark)) (:foreground "Red"))
       (t (:bold t)))
      "Face used in MuPAD to fontify last prompt.")
    ('mupad-input
     '((((class grayscale) (background light)) (:foreground "DimGray" :italic t))
       (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
       (((class color) (background light)) (:foreground "RosyBrown"))
       (((class color) (background dark)) (:foreground "LightSalmon"))
       (t (:italic t)))
     "Face used in MuPAD to fontify intputs except last one.")
    ('mupad-last-input
     '((((class grayscale) (background light)) (:foreground "DimGray" :italic t))
       (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
       (((class color) (background light)) (:foreground "Brown"))
       (((class color) (background dark)) (:foreground "Red"))
       (t (:italic t)))
     "Face used in MuPAD to fontify last intput.")
    ('mupad-output
     '((((class grayscale) (background light)) (:foreground "DimGray" :italic t))
       (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
       (((class color) (background light)) (:foreground "RosyBrown"))
       (((class color) (background dark)) (:foreground "LightSalmon"))
       (t (:italic t)))
     "Face used in MuPAD to fontify outputs.")
    ('mupad-type
     '((((class grayscale) (background light)) (:foreground "Gray90" :bold t))
      (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
      (((class color) (background light)) (:foreground "#20A040"))
      (((class color) (background dark)) (:foreground "#20A040"))
      (t (:bold t :underline t)))
     "Face used in MuPAD to fontify types.")
    ('mupad-info
     '((((class grayscale) (background light)) (:foreground "LightGray" :bold t))
       (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
       (((class color) (background light)) (:foreground "Orchid"))
       (((class color) (background dark)) (:foreground "LightSteelBlue"))
       (t (:bold t)))
      "Face used in MuPAD to fontify info messages in *MuPAD* buffer.")
    ('mupad-domain
     '((((class grayscale) (background light)) (:foreground "DimGray" :italic t))
       (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
       (((class color) (background light)) (:foreground "RosyBrown"))
       (((class color) (background dark)) (:foreground "LightSalmon"))
       (t (:italic t)))
     "Face used in MuPAD to fontify domains.")
    ('mupad-options
     '((((class grayscale) (background light)) (:foreground "LightGray" :bold t))
      (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
      (((class color) (background light)) (:foreground "Purple"))
      (((class color) (background dark)) (:foreground "Cyan"))
      (t (:bold t)))
     "Face used in MuPAD to fontify options."))
  ("MuPAD's own" ; copied from mupad-mode.el
    ('mupad-last-prompt
     '((((class grayscale) (background light)) (:foreground "LightGray" :bold t))
       (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
       (((class color) (background light))
        (:bold nil :inverse-video t :foreground "RoyalBlue" :background "yellow"))
       (((class color) (background dark)) (:foreground "LightSteelBlue"))
       (t (:bold t)))
      "Face used in MuPAD to fontify promptthe last one.")
    ('mupad-prompt
     '((((class grayscale) (background light)) (:foreground "LightGray" :bold t))
       (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
       (((class color) (background light)) (:foreground "Orchid"))
       (((class color) (background dark)) (:foreground "Red"))
       (t (:bold t)))
      "Face used in MuPAD to fontify last prompt.")
    ('mupad-input
     '((((class grayscale) (background light)) (:foreground "DimGray" :italic t))
       (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
       (((class color) (background light)) (:foreground "RosyBrown"))
       (((class color) (background dark)) (:foreground "LightSalmon"))
       (t (:italic t)))
     "Face used in MuPAD to fontify intputs except last one.")
    ('mupad-last-input
     '((((class grayscale) (background light)) (:foreground "DimGray" :italic t))
       (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
       (((class color) (background light)) (:foreground "Brown"))
       (((class color) (background dark)) (:foreground "Red"))
       (t (:italic t)))
     "Face used in MuPAD to fontify last intput.")
    ('mupad-output
     '((((class grayscale) (background light)) (:foreground "DimGray" :italic t))
       (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
       (((class color) (background light)) (:foreground "RosyBrown"))
       (((class color) (background dark)) (:foreground "LightSalmon"))
       (t (:italic t)))
     "Face used in MuPAD to fontify outputs.")
    ('mupad-type
     '((((class grayscale) (background light)) (:foreground "LightGray" :bold t))
      (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
      (((class color) (background light)) (:foreground "Purple"))
      (((class color) (background dark)) (:foreground "Cyan"))
      (t (:bold t)))
     "Face used in MuPAD to fontify types.")
    ('mupad-info
     '((((class grayscale) (background light))
       (:foreground "DimGray" :bold t :italic t))
      (((class grayscale) (background dark))
       (:foreground "LightGray" :bold t :italic t))
      (((class color) (background light)) (:foreground "#DAA520"))
      (((class color) (background dark)) (:foreground "#DAA520"))
      (t (:bold t :italic t)))
      "Face used in MuPAD to fontify info messages in *MuPAD* buffer.")
    ('mupad-domain
     '((((class grayscale) (background light))
       (:foreground "DimGray" :bold t :italic t))
      (((class grayscale) (background dark))
       (:foreground "LightGray" :bold t :italic t))
      (((class color) (background light)) (:foreground "#C07030"))
      (((class color) (background dark)) (:foreground "#C07030"))
      (t (:bold t :italic t)))
     "Face used in MuPAD to fontify domains.")
    ('mupad-options
      '((((class grayscale) (background light))
       (:foreground "DimGray" :bold t :italic t))
      (((class grayscale) (background dark))
       (:foreground "LightGray" :bold t :italic t))
      (((class color) (background light)) (:foreground "#C03030"))
      (((class color) (background dark)) (:foreground "#BBBBBB"))
      (t (:bold t :italic t)))
     "Face used in MuPAD to fontify options.")
   ('mupad-comment
     '((((class grayscale) (background light)) (:foreground "DimGray" :bold t :italic t))
	(((class grayscale) (background dark))  (:foreground "LightGray" :bold t :italic t))
	(((class color) (background light)) (:foreground "#B0B0B0"))
	(((class color) (background dark)) (:foreground "#AAAAAA"))
	(t (:bold t :italic t))) "Face used in MuPAD to fontify comments.")
   ('mupad-string
    '((((class grayscale) (background light)) (:foreground "DimGray" :italic t))
      (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
      (((class color) (background light)) (:foreground "#38D238"))
      (((class color) (background dark)) (:foreground "#38D238"))
      (t (:italic t))) "Face used in MuPAD to fontify string.")
   ('mupad-keyword
    '((((class grayscale) (background light)) (:foreground "LightGray" :bold t))
      (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
      (((class color) (background light)) (:foreground "#3030DD"))
      (((class color) (background dark)) (:foreground "#3030DD"))
      (t (:bold t))) "Face used in MuPAD to fontify keywords.")
   ('mupad-primitive-name
    '((((class grayscale) (background light)) (:foreground "Gray90" :bold t))
      (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
      (((class color) (background light)) (:foreground "#7080A0"))
      (((class color) (background dark)) (:foreground "#7080A0"))
      (t (:inverse-video t :bold t))) "Face used in MuPAD to fontify names of primitive.")
   )))

(defcustom mupad-color-scheme "Minimal" ; a key in mupad-color-scheme-alist
"Defines a set of colors to be used."
:set 'mupad-use-color-scheme
:initialize 'custom-initialize-default ;if you use :set, you should specify :initialize!
:type (append '(choice) (mapcar (lambda (scheme) (list 'const (car scheme)))
                                mupad-color-scheme-alist))
:group 'mupad-font-lock)


(defun mupad-precomputes-locally (limit)
  (mupad-reads-mupad-loaded-libraries)
  (when mupad-fontify-function-names
    (mupad-add-regexp-names mupad-function-def-start 1
      t 'mupad-fn-names-regexp))
  (when mupad-fontify-global-var
    (mupad-add-regexp-names mupad-global-var-start 1
      t 'mupad-global-var-regexp))
  nil)

(defconst mupad-script-fontification-keywords-1
  (purecopy
   (list
    (list 'mupad-find-def '(0 (if mupad-function-defp 'mupad-function-name 'mupad-global-var)))
    '("\\(\\<userinfo\\>(\\)\\([^;]+\\));"  (2 mupad-info t t))
    '(mupad-find-keywords (0 mupad-keyword))
   ))
  "Subdued level of fontification for mupad-mode.")

(defconst mupad-fontification-keywords-2
  (purecopy
   (list
    '(mupad-precomputes-locally (0 mupad-domain)) ; un precalcul en fait !
    '(mupad-find-user-fn (1 mupad-user-def))
    '(mupad-find-user-global-var (1 mupad-user-def))
    '(mupad-find-options (0 mupad-options))
    '(mupad-find-types (0 mupad-type))
    )))

(defconst mupad-script-fontification-keywords-2
  (append
   mupad-script-fontification-keywords-1
   mupad-fontification-keywords-2)
  "Medium level of fontification for mupad-mode.")

(defconst mupad-fontification-keywords-3
  (purecopy
   (list
    '(mupad-find-simple-primitive-name (0 mupad-primitive-name))
    '(mupad-find-simple-loaded-primitive-name (0 mupad-primitive-name))
    '(mupad-find-composed-primitive-name (0 mupad-primitive-name))
    '(eval . (cons mupad-prefix-regexp '(0 mupad-domain)))
    '(mupad-find-ops (0 mupad-keyword append t))
    )))

(defconst mupad-script-fontification-keywords-3
  (append
   mupad-script-fontification-keywords-2  
   mupad-fontification-keywords-3)
  "Gaudy level of fontification for mupad-mode.")

(defconst mupad-color-scheme-default-alist
  '(('mupad-comment font-lock-comment-face
     "Face used in MuPAD to fontify comments.
Default is `font-lock-comment-face'.")
    ('mupad-string font-lock-string-face
       "Face used in MuPAD to fontify string.
Default is `font-lock-string-face'.")
    ('mupad-function-name font-lock-function-name-face
       "Face used in MuPAD to fontify function names in definitions.
Default is `font-lock-function-name-face'.")
    ('mupad-variable-name font-lock-variable-name-face
       "Face used in MuPAD to fontify function arguments in definitions.
Default is `font-lock-variable-name-face'.")
    ('mupad-global-var font-lock-constant-face
       "Face used in MuPAD to fontify global variables.
Default is `font-lock-constant-face'.")
    ('mupad-primitive-name font-lock-builtin-face
       "Face used in MuPAD to fontify names of primitive.
Default is `font-lock-builtin-face'.")
    ('mupad-keyword font-lock-keyword-face
       "Face used in MuPAD to fontify keywords.
Default is `font-lock-keyword-face'.")
    ('mupad-user-def font-lock-builtin-face
      "Face used in MuPAD to fontify user defined functions,
when used and `mupad-fontify-function-names' is t
(modify it via menu-item MuPAD/Environnement/Customize).
The same face is used to fontify user defined global variables,
when used and `mupad-fontify-global-vars' is t.
Default is `font-lock-builtin-face'.")))

(defsubst mupad-default-face (face default-face doc)
  (when (not (facep face))
    (copy-face default-face face)
    (set-face-documentation face doc)))

(defun mupad-face-spec-set (face spe doc)
  (face-spec-set face spe)
  (set-face-documentation face doc))

(defun mupad-set-color-scheme (symbol scheme fn)
  (let (spe)
  (mapcar (lambda (aplace)
            (if (setq spe
                   (cdr (assoc aplace
                     (cdr (assoc (or scheme mupad-color-scheme) mupad-color-scheme-alist)))))
                (progn
                   (eval (list fn (eval aplace) (nth 0 spe) (nth 1 spe))))
              (setq spe (cdr (assoc aplace mupad-color-scheme-default-alist)))
              (mupad-default-face (eval aplace) (nth 0 spe) (nth 1 spe))))
     mupad-places)))

(defun mupad-use-color-scheme (symbol scheme)
  (mupad-set-color-scheme symbol scheme 'mupad-face-spec-set))

(defun mupad-first-use-color-scheme nil
  (mupad-set-color-scheme 'dummy mupad-color-scheme 'defface))

(defun mupad-fontification-common-init nil
  (when (setq mupad-can-fontify
              (and mupad-fontify
                   (eq window-system 'x) (x-display-color-p)))
    (require 'font-lock)
    (mupad-first-use-color-scheme)
    (make-local-variable 'font-lock-comment-face)
    (setq font-lock-comment-face '(eval mupad-comment))
    (make-local-variable 'font-lock-string-face)
    (setq font-lock-string-face '(eval mupad-string))))

;; Parsers :

(defun mupad-find-def (limit)
  "A fontification parser. See `mupad-function-defp'."
  ;; Note: Comments are fontify syntactically, thus BEFORE.
  ;; and since there is no overriding of fontification, we do not have
  ;; to worry about the definitions we find to be within a comment.
  (if (re-search-forward "^\\([a-zA-Z_]\\(\\w\\|::\\)*\\)[ \t\n]*:=" limit 0)
      (let ((beg (match-beginning 1)) (end (match-end 1)))
        ;; decide whether it is a proc/fun/func def :
        (setq mupad-function-defp (looking-at "[ \t\n]*\\(proc\\|func?\\)[ \t\n]*("))
        (set-match-data (list beg end))
        t)
      nil))

(defun mupad-regexp-parser (my-regexp limit &optional level)
  ;; We skip comments, and that saves some time since comments typically
  ;; contain lots of words.
  (unless level (setq level 0))
  (let ((has-been-foundp nil) (st (point)) (case-fold-search nil))
    (while (if (setq has-been-foundp
                     (re-search-forward my-regexp limit t))
               (progn
                 (goto-char (match-beginning level))
                 (mupad-skip-comments st limit))))
    (if has-been-foundp
        (progn (set-match-data (list (match-beginning level) (goto-char (match-end level)))) t)
        ;; End of parsing:
        nil)))

(defun mupad-find-options (limit)
   (mupad-regexp-parser mupad-option-regexp limit 1))

(defun mupad-find-types (limit)
   (mupad-regexp-parser mupad-type-regexp limit 1))

(defun mupad-find-keywords (limit)
   (mupad-regexp-parser mupad-keyword-regexp limit 1))

(defun mupad-find-ops (limit)
   (mupad-regexp-parser "\\([]\\[\\.^+\\-*/,<>:=@$;()]\\|\\<&\\w*\\>\\)+" limit 0))

(defun mupad-find-simple-primitive-name (limit)
  (mupad-regexp-parser mupad-primitive-regexp-simple limit 1))

(defun mupad-find-simple-loaded-primitive-name (limit)
  (unless (null mupad-primitive-regexp-simple-from-libraries)
    (mupad-regexp-parser mupad-primitive-regexp-simple-from-libraries limit 1)))

(defun mupad-find-composed-primitive-name (limit)
  ;; The time saving idea is that few contruct of the shape lib::fn
  ;; will be used.
  (let ((has-been-foundp nil) beg (st (point)) (case-fold-search nil))
    (while (if (setq has-been-foundp
                     (if (re-search-forward (concat mupad-prefix-regexp "::") limit t)
                         (progn
                           (setq beg (match-end 0))
                           (goto-char (match-beginning 0))
                           (re-search-forward
                             (nth 1 (assoc (buffer-substring-no-properties
                                                  (match-beginning 0) (- beg 2))
                                           mupad-primitive-regexp-prefix-alist))
                             limit t))))
               (progn
                 (goto-char (match-beginning 1))
                 (mupad-skip-comments st))))
    (if has-been-foundp
        (progn (set-match-data (list beg (goto-char (match-end 1)))) t)
        ;; End of parsing:
        nil)))

(defun mupad-find-user-fn (limit)
  "A fontification parser."
  (let ((case-fold-search nil))
    (if (and (not (string= mupad-fn-names-regexp ""))
             mupad-fontify-function-names)
        (re-search-forward mupad-fn-names-regexp limit 0)
        nil)))
       
(defun mupad-find-user-global-var (limit)
  "A fontification parser."
  (let ((case-fold-search nil))
  (if (and (not (string= mupad-global-var-regexp ""))
           mupad-fontify-global-var)
      (re-search-forward mupad-global-var-regexp limit 0)
      nil)))

(defun mupad-looking-at-identifierp nil
"T if point is located at the beginning of a mupad-identifier."
  (and (looking-at mupad-regexp-identifier)
       (or (bobp)
           (save-excursion
             (forward-char -1)
             (looking-at "[^a-zA-Z_0-9]")))))

(defun mupad-looking-at-fn-call nil
"Return end of fn-call if point is located at the beginning of a
fn-call, and nil otherwise."
  (if (mupad-looking-at-identifierp)
      (save-excursion
        (skip-chars-forward "\w")
        (if (not (looking-at "[ \t\n]*("))
            nil
          (goto-char (- (match-end 0) 1))
          (mupad-forward-sexp)  ;; Error if expression is unbalanced.
          (point)))
    nil))

(defun mupad-reads-mupad-loaded-libraries nil
  "Sets `mupad-loaded-libraries' and `mupad-primitive-regexp-simple-from-libraries'.
Do *not* extend mupad-completion-array."
  (condition-case err
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "\\<Exported Libraries:" nil t)
          (let ((end (line-end-position))
                prefix)
            (setq mupad-primitive-regexp-simple-from-libraries nil)
            (while (re-search-forward "\\<\\w+\\>" end t)
              (setq prefix (match-string-no-properties 0))
              (cond
               ((not (member prefix mupad-libraries-list))
                (message "Invalid Library Name: %s" prefix) (sit-for 1))
               (t (add-to-list 'mupad-loaded-libraries prefix)
                  (mapcar 'mupad-add-symbol
                          (cdr (assoc prefix mupad-libraries-completion-alist)))
                  (mupad-extends-primitive-regexp-simple-from-libraries prefix))))
	    (mapcar 'mupad-extends-primitive-regexp-simple-from-libraries mupad-loaded-libraries-on-line)
            )))
    (error (princ err) nil)))

(defun mupad-force-update-fontification () "Update fontify AND some completion if required."
  (interactive)
  (condition-case err
      (when mupad-can-fontify
        (when (eq major-mode 'mupad-mode)
          (mupad-reads-mupad-loaded-libraries))
        (setq mupad-hash-parity nil)
        (font-lock-fontify-buffer) ;(print "Through font-lock-fontify-buffer")
        (message "done"))
    (error (princ err) nil)))

(defvar mupad-has-changed-fontification-levelp nil
  "t if mupad-fontification-level has changed.
Used in mupad-update-fontification-buffers.")

(defun mupad-update-fontification-buffers nil
"  Update hilighting/unhilighting on all the buffers
   that are in mupad-mode or in mupad-shell-mode."
  (interactive)
  (condition-case err
      (save-excursion
        (mapcar
         (lambda (abuffer)
           (set-buffer abuffer)
           (when (memq major-mode '(mupad-shell-mode mupad-mode))
	     (if mupad-can-fontify
		 (progn
		   (setq font-lock-set-defaults nil)
		   (font-lock-set-defaults) ; in case the level of fontification has changed
		   (setq mupad-has-changed-fontification-levelp nil)
		   (font-lock-fontify-buffer))
	       (font-lock-unfontify-buffer))))
         (buffer-list))
        (message ""))
    (error (princ err) nil)))

;;; Menu bar

(defun mupad-turn-on-lazy-font-lock nil ""
  (interactive)
  (condition-case err
      (progn
        (require 'lazy-lock)
        (when (featurep 'lazy-lock) (lazy-lock-mode)))
    (error (princ err) nil)))

(defun mupad-customize-faces nil
  (interactive)
  (if mupad-tutorial-requiredp
      (let ((wind (selected-window)) s
            (msg "Feeling adventurous ? First select the kind of objects you want to `color'. If this type is said to have a default value, you then have to decide either to change this default value (a change that will concern all modes; the recommended strategy) either to change only the local value (for instance mupad-string locally and font-lock-string-face globally). If you choose a global modification then you should leave emacs and re-enter it for the new value to take effect. Local modifications take precedence over global ones, and thus to go back to global (or default) determination once you've changed it locally, you should erase the corresponding line in your `.emacs' file.\n\nChoose a color among the list below (you can also have a look at the file rgb.txt if it exists). You can require your characters to be in bold face, underlined or in italic. Simply select the proper square and use Toggle to make the symbol t appear (instead of nil).\n Colors List:\n")
            (colors-list (if (fboundp 'x-defined-colors)
                             (sort (x-defined-colors) 'string-lessp)
                             '("No colours found !!!"))))
           (mupad-window-manager "*MuPAD Help*" 'mupad-beginning-temp)
           (insert msg)
           (fill-region (point-min) (point-max) 'left)
           ;; Following taken from list-colors-display of facemenu.el:
           (while colors-list
             (setq s (point))
             (insert (car colors-list))
             (indent-to 20)
             (put-text-property s (point) 'face 
                                (cons 'background-color (car colors-list)))
             (setq s (point))
             (insert "  " (car colors-list) "\n")
             (put-text-property s (point) 'face 
                                (cons 'foreground-color (car colors-list)))
             (setq colors-list (cdr colors-list)))
           (goto-char (point-min))
           (mupad-info-wind-conf)
           (select-window wind)))
  (customize-apropos-faces "font-lock-.*\\|mupad-.*"))

(defun mupad-choose-color-scheme nil
  (interactive)
  (mupad-store-wind-conf)
  (customize-variable 'mupad-color-scheme)
  (mupad-info-wind-conf))

(defun mupad-set-fontification-level (sym val)
  (setq sym val ; sym is mupad-fontification-level
	mupad-has-changed-fontification-levelp t)
  (if (listp font-lock-maximum-decoration)
      (if (assoc 'mupad-mode font-lock-maximum-decoration)
	  (setcdr (assoc 'mupad-mode font-lock-maximum-decoration)  val)
	(setq font-lock-maximum-decoration
	      (append (list (cons 'mupad-mode val))
		      font-lock-maximum-decoration)))
    (setq font-lock-maximum-decoration
	  (list (cons 'mupad-mode val)
		(cons t  font-lock-maximum-decoration))))
  (mupad-update-fontification-buffers))

(defcustom mupad-fontification-level 1
"Subdued means almost nothing is fontified,
Average is ... average, and Gaudy means most
things are fontified."
:type '(choice (const :tag "Subdued" 0)
	       (const :tag "Average" 1)
	       (const :tag "Gaudy" 2))
:set 'mupad-set-fontification-level
:initialize 'custom-initialize-default ;if you use :set, you should specify :initialize!
:group 'mupad-font-lock)

(defun mupad-customize-fontification-level nil
  (interactive)
  (customize-variable 'mupad-fontification-level))

(defun mupad-color-cpl-menu nil
  "Build the Colors menu"
    (if (and (eq window-system 'x) (x-display-color-p))
    (append
      (list 
        ["Update" mupad-force-update-fontification :active t :included mupad-can-fontify]
        ["Refontify All Buffers" mupad-update-fontification-buffers :active t
                         :included mupad-can-fontify :key-sequence nil])
      (if mupad-can-fontify
          (list (list "Customize" 
           ["Choose a color scheme" mupad-choose-color-scheme :active t :key-sequence nil]
            ["Choose each color" mupad-customize-faces :active t :key-sequence nil]
	    ["Choose a fontification level"
	     mupad-customize-fontification-level :active t :key-sequence nil]))
         '())
      )))

(add-hook 'menu-bar-update-hook
  '(lambda nil
     (when (eq major-mode 'mupad-mode)
       (easy-menu-change '("MuPAD") "Colors/Completion" (mupad-color-cpl-menu) "Debug..."))))

(add-hook 'mupad-mode-hook
  '(lambda nil
       (make-variable-buffer-local 'mupad-function-defp) ; not really required
       (make-variable-buffer-local 'mupad-hash-parity)   ; not really required
       (define-key mupad-mode-map   "\C-l" (function mupad-force-update-fontification))
       (set (make-local-variable 'font-lock-defaults)
	    (list (list mupad-script-fontification-keywords-1
			mupad-script-fontification-keywords-2
			mupad-script-fontification-keywords-3)
		  nil nil nil 'mupad-get-safe-place))
       (setq mupad-safe-place-regexp "^\\(//--+\\|/\\*-+-\\*/\\)$")
       (or (not mupad-hash-comment)
	   (set (make-local-variable 'font-lock-syntactic-keywords)
		'(((lambda (limit) (if (search-forward "#" limit t) t
				     (setq mupad-hash-parity nil)))
		   0 (cons (if (setq mupad-hash-parity (not mupad-hash-parity))
			       11 12) ?#)))))
       (mupad-fontification-common-init)
       (mupad-turn-on-lazy-font-lock) ;(print "Before font-lock")
       (mupad-force-update-fontification)    ;(print "After font-lock")
       ))

;-------------------------end !!
