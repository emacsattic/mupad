;;; mupad.el --- mupad editing support package

;; Copyright (C) 2002, Fran\c cois Maltey, Nicolas Thiery, Olivier Ramar\'e

;; Maintainer: Olivier Ramare <ramare@agat.univ-lille1.fr>
;; keywords: progmodes

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation version 2.1.

;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Commentary:

;; Major mode for MuPAD editing. It provides functions for editing
;; MuPAD code and interacting with MuPAD. See the documentation
;; of mupad-mode.

;; This code was initialy stolen by ?? from "maple.el" which is the work
;; of Bruno Salvy (Bruno.Salvy@inria.fr). Since then a huge part of
;; "pari.el" of Annette Hoffman/David Carlisle/Karim Belabas/Olivier Ramare
;; has been incorporated as well as most of "mupad-mode.el" of Juergen Billing.
;; 15/09/1995: First release of mupad.el
;; Till December 2002, combined efforts/comments of
;;        Henning von Bargen (h.vonbargen@cityweb.de)
;;        Juergen Billing (bij@plato.uni-paderborn.de)
;;        Francois Maltey (Francois.Maltey@enst-bretagne.fr)
;;        Michel Quercia (quercia@cal.enst.fr)
;;        Nicolas Thiery (nthiery@jonas.univ-lyon1.fr)
;;        Winfried Truemper (winni@xpilot.org)
;;        Olivier Ramare (ramare@agat.univ-lille1.fr)
;;        Paul Zimmermann (Paul.Zimmermann@loria.fr)
;; have brought this version to life.

(provide 'mupad)
;(require 'mupad-xemacs)
;;----------------------------------------------------
;; Part I   : Variables and Constants, except Keymaps.
;;----------------------------------------------------
(defconst mupad-mode-version "3.00" "Version of `mupad.el'.")
;;----------------------------------------------------
;; USER DEPENDENT VARIABLES AND CONSTANTS:

(defgroup mupad nil
"Major mode for editing mupad scripts"
:group 'languages :prefix "mupad-")

(defgroup mupad-indentation nil
"MuPAD customization subgroup concerning indentation
and furthering of constructs"
:group 'mupad :prefix "mupad-")

(defgroup mupad-miscellana nil
"MuPAD customization subgroup dedicated to less important switches"
:group 'mupad :prefix "mupad-")

(defun mupad-set-mupad-directory (sym val)
  (set sym val)
  (setq mupad-manual-command (concat mupad-directory "/share/bin/manual"))
  (when (featurep 'mupad-help)
    (setq  mupad-help-tree (concat mupad-directory "/share/doc/"))))

;; This variable should be set by a CONFIGURE if it ever exists...
(defcustom mupad-directory "/usr/local/src/MuPAD/"
"Used for initializing `mupad-manual-command' and `mupad-help-tree'."
:initialize 'custom-initialize-default
:set 'mupad-set-mupad-directory
:type 'string :group 'mupad)

(defcustom mupad-manual-command
;"netscape file:/usr/local/MuPAD/mupad_html_help/Automated/index.html"
(concat mupad-directory "/share/bin/manual")
"The manual command. If you prefer the html documentation,
put for instance \"netscape file:/usr/local/MuPAD/mupad_html_help/index.html\""
:type 'string :group 'mupad)

(defcustom mupad-el-info "/home/ramare/lisp/first-look/MuPAD/mupad.el-info"
"Place of the mupad.el-info file."
:type 'string :group 'mupad)

(defcustom mupad-temp-directory "/tmp/"
  "Directory in which to create temporary files."

:type 'string :group 'mupad)

(defcustom mupad-electric-p t
"Non-nil means emacs will automatically insert closing braces, and so on.
It *does* concern indentation for choosing between
sli-electric-terminate-line (indent-then-newline-and-reindent)
and sli-newline (newline-and-indent). See also `mupad-auto-indent' and
`mupad-closed-brace'." ; `mupad-auto-indent' is defined at the end of this file.
:type 'boolean :group 'mupad)

(defcustom mupad-tab-always-indent t
"Non-nil means TAB in MuPAD-mode should always reindent the current line,
regardless of where in the line point is when the TAB command is used."
:type 'boolean :group 'mupad-indentation)

(defcustom mupad-closed-brace t
  "A refinement `mupad-electric-p': Turn on/off self-closed braces when
mupad-electric-p is on, does nothing otherwise."
:type 'boolean :group 'mupad)

(defcustom mupad-hash-comment nil
  "Set it to nil if you do not use #--# comments, which is highly
recommended. It will speed up fontification. Note that already
automatic indentation does not deal with such comments.
To convert all hash-comments of a program into usual C-one, use
M-x mupad-replace-hash-comment."
:type 'boolean :group 'mupad)

(defcustom mupad-javadoc-stylep nil
"t means an additionnal item will be added to the menu-bar:
[MuPAD/Shapes/Describe]. When used at the beginning of a procedure, it triggers
insertion of javadoc-style description of it.
See `mupad-describe-this-proc' and `mupad-user-mail-address'."
:type 'boolean :group 'mupad-miscellana)

(defcustom mupad-user-mail-address
  user-mail-address
  "What it says it is. See `mupad-javadoc-stylep'."
  :type 'string :group 'mupad-miscellana)

(defcustom mupad-tutorial-requiredp t
"If non-nil, more information will be given."
:type 'boolean :group 'mupad-miscellana)

(defcustom mupad-indent-level 2
"Indentation of Mupad statements with respect to containing block."
:type 'integer :group 'mupad-indentation)

(defcustom mupad-case-indent 2
"Indentation for case statements."
:type 'integer :group 'mupad-indentation)

(defun mupad-set-and-recompute-indentation (sym val)
"Used to set things dynamically in some customizable variable."
  (set sym val)
  (save-current-buffer
   (mapcar
    (lambda (bf)
      (set-buffer bf)
      (cond
       ((eq major-mode 'mupad-mode) (mupad-learns-indentation))))
      (buffer-list))))

(defcustom mupad-structures
  '((["if" head 3]
     ["then" soft mupad-indent-level]
      (["elif"   strong 5]
       ["then" soft mupad-indent-level])
       (["else"   strong mupad-indent-level])
       ["end_if" end]
       ["end" end])
    (["%if" head 4]
     ["then" soft mupad-indent-level]
      (["elif"   strong 5]
       ["then" soft mupad-indent-level])
       (["else"   strong mupad-indent-level])
       ["end_if" end]
       ["end" end])
    (["for" head 4]
      (["from" beacon 5]
       (["to" beacon 3]
        (["step" beacon 5]))
       (["downto" beacon 7]
        (["step" beacon 5])))
     ["do"  soft mupad-indent-level]
     (["private" special-head 8 ";"])
     (["parallel" strong mupad-indent-level])
     ["end_for"  end]
     ["end" end])
    (["while"     head 6]
     ["do"        soft mupad-indent-level]
     ["end_while" end]
     ["end" end])
    (["proc"     head mupad-indent-level]
     (["local" special-head 6 ";"])
     (["name" special-head 5 ";"])
     (["option" special-head 7 ";"])
     ["begin"    strong mupad-indent-level]
     ["end_proc" end])
    (["category" head '(absolute . 2)]
     ["begin" strong mupad-indent-level] ; should be the same as above !!
     ["end_category" end])
    (["domain" head '(absolute . 2)]
     (["category" special-head 10 ";"])
     (["axiom" special-head 10 ";"])
     ["begin" strong mupad-indent-level] ; should be the same as above !!
     ["end_domain" end])
    (["axiom" head '(absolute . 2)]
     ["begin" strong mupad-indent-level] ; should be the same as above !!
     ["end_axiom" end])
    (["repeat"     head 7]
     ["until"      strong 6]
     ["end_repeat" end]
     ["end" end])
    (["case"      head mupad-indent-level]
     ["of"        strong 3]
     ["do"        soft mupad-indent-level]
     (["otherwise" strong mupad-indent-level])
     ["end_case"  end]
     ["end" end])
    (["parbegin" head mupad-indent-level]
     ["end_par"  end])
    (["seqbegin" head mupad-indent-level]
     ["end_seq"  end])
    (["(" head 1] [")" end])
    (["[" head 1] ["]" end])
    (["{" head 1] ["}" end])
    (["=" math-relation 1]) ; that's the last item of any relation, like in ':='
    (["<" math-relation 1])
    ([">" math-relation 1])
    (["and" math-relation 4])
    (["$" math-relation 1])
    (["or" math-relation 3])
    (["in" math-relation 3])
    )
"See `sli-structures'.
If you want C-c C-e to add \"end\" instead of \"end_for\" for instance,
simply exchange both strings in this definition."
:type '(repeat (repeat (restricted-sexp :match-alternatives (vectorp listp))))
:initialize 'custom-initialize-default
:set 'mupad-set-and-recompute-indentation
:group 'mupad-indentation)

(defcustom mupad-shift-alist
  '((["case" "of"] . mupad-case-indent)
    (["domain" "begin"] . -2)
    (["category" "begin"] . -2))
"See `sli-shift-alist'."
:type '(repeat (cons (vector string string) sexp))
:initialize 'custom-initialize-default
:set 'mupad-set-and-recompute-indentation
:group 'mupad-indentation)

(defcustom mupad-no-heredity-list
  '(["case" "end_case"])
"See `sli-no-heredity-list'."
:type '(repeat (vector string string))
:initialize 'custom-initialize-default
:set 'mupad-set-and-recompute-indentation
:group 'mupad-indentation)
 
(defvar mupad-separators '(";" ":" ",")
"See `sli-separators'.")

(defcustom mupad-fixed-keys-alist
  '(("proc" . mupad-indent-level))
"See `sli-fixed-keys-alist'."
:type '(repeat (cons string sexp))
:initialize 'custom-initialize-default
:set 'mupad-set-and-recompute-indentation
:group 'mupad-indentation)

(defcustom mupad-keys-with-newline
'("begin" "proc" "repeat" "seqbegin"
  "parbegin" "then" "do" ";" "category" "domain")
"See `sli-keys-with-newline'."
:type '(repeat string)
:initialize 'custom-initialize-default
:set 'mupad-set-and-recompute-indentation
:group 'mupad-indentation)

(defcustom mupad-keys-without-newline
'(")" "]" "from" "to" "in")
"See `sli-keys-with-newline'."
:type '(repeat string)
:initialize 'custom-initialize-default
:set 'mupad-set-and-recompute-indentation
:group 'mupad-indentation)

(defcustom mupad-add-to-key-alist
  '(("end_case" . ";")        ("end_if" . ";")
    ("end_for" . ";")         ("end_while" . ";")
    ("end_repeat" . ";")      ("end_par" . ";")
    ("end_seq" . ";")         ("end_proc" . ":")
    ("if" . "")               ("%if" . "")
    ("*/" . "")               ("begin" . "")
    ("proc" . "")             ("repeat" . "")
    ("seqbegin" . "")         ("parbegin" . "")
    (";" . "")                ("then" . "")
    ("do" . "")               ("end_domain" . ":")
    ("end_category" . ":"))
"See `sli-add-to-key-alist'."
:type '(repeat (cons string string))
:initialize 'custom-initialize-default
:set 'mupad-set-and-recompute-indentation
:group 'mupad-indentation)

(defcustom mupad-correction-alist
  '(("for" . "from")
    ("from" . "to"))
"This variable corresponds to `sli-maid-correction-alist'. See `sli-tools'."
:type '(repeat (cons string string))
:initialize 'custom-initialize-default
:set 'mupad-set-and-recompute-indentation
:group 'mupad-indentation)

(defcustom mupad-more-maidp t
"If non-nil, `sli-maid' will use `mupad-add-to-key-alist' to add a ':'
after 'end_proc' and so on. See `sli-more-maidp'."
:type 'boolean
:initialize 'custom-initialize-default
:set 'mupad-set-and-recompute-indentation
:group 'mupad-indentation)

(defun mupad-is-a-separatorp (&optional pt)
"See `sli-is-a-separatorp-fn'."
  (save-excursion
    (when pt (goto-char pt))
    (save-match-data
      (or (member (char-after) (list ?; ?,))
          (and (= (char-after) ?:)
               (not (looking-at ":[=<>:]"))
               (not (= (preceding-char) ?:)))))))

;;----------------------------------------------------------------------
;;  Other variables and Constants.

(defalias 'MuPAD-mode 'mupad-mode)
;; So that if the first line of a mupad program contains
;; "-*- MuPAD -*-", it triggers automatically mupad-mode.

(defsubst mupad-print-if-compiling (messg)
  (when (get-buffer "*Compile-Log*")
    (with-current-buffer "*Compile-Log*"
      (insert messg "\n"))))

(eval-and-compile 
(defun mupad-setup nil
  "Common packages required while compiling and running."
  (require 'disp-table)   ;; Almost always required.
  (require 'backquote)    ;; For macros.
  (require 'gud)          ;; For the debugger.
  (require 'regexp-opt)
  (require 'imenu)
  (require 'mupad-fontification)
  (require 'mupad-cpl)
  (require 'mupad-run)
  (require 'mupad-bus)
  (require 'mupad-help)
  
  (or (featurep 'easymenu) (load "easymenu" t))
  (or (featurep 'easymenu)
      (progn
        ;; This part is no crap ! 'easymenu has to be present
        ;; at compilation time.
        (mupad-print-if-compiling "No menu-bar: easymenu.el not found.")
        (fset 'easy-menu-define nil)))))

(mupad-setup)
(eval-when-compile  ;; Carefull !! the shell script configure uses strict syntax here.
  ; for developpement:
  ;(setq byte-compile-warnings '(free-vars unresolved callargs redefine obsolete))
  ; for users:
  (setq byte-compile-warnings '(unresolved redefine obsolete))
  (setq byte-optimize t)
  (mupad-setup))

(defcustom mupad-mode-hook nil
"Hook when entering mupad-mode"
:group 'mupad :type 'hook)

(defvar MuPAD-menu-map nil
"Keymap used for the menu-bar item MuPAD in mupad-mode")

(defvar zap-file (concat mupad-temp-directory (make-temp-name "mupad-"))
"Temporary file name used for text being sent as input to MuPAD.")
(defvar mupad-el-temp-file
  (concat mupad-temp-directory (make-temp-name "mupad-"))
  "Temporary file name used for text being sent as input to emacs.")

(defvar mupad-create-completions-donep nil "t if `mupad-completion-array' is already created.")
(defvar mupad-completion-array (make-vector 5003 0) ;3776 symbols in MuPAD 2.0
"Obarray used for completion.")  ;; A prime number as a length is a good thing !

(defvar mupad-primitive-regexp-simple-from-libraries nil
"Regexp matching all the primitive names that do come from a library.
Compare with `mupad-primitive-regexp-simple' and look at `mupad-loaded-libraries'.")
(defvar mupad-primitive-regexp-simple nil
"Regexp matching all the primitive names that do not come from a library.")
(defvar mupad-primitive-regexp-prefix-alist nil
"List of elements of the form (prefix aregexp), where prefix is a
library name and aregexp the regexp matching all the primitive names
coming from this library.")
(defvar mupad-fn-names-regexp ""
"Regexp matching user-defined functions.
Functions enter this regexp once they have been added to completion.")
(defvar mupad-global-var-regexp ""
"Regexp matching user-defined functions.
Functions enter this regexp once they have been added to completion.")

 ;;;
 ;;; Some regular expressions
 ;;;
;;; Strings used to mark beginning and end of excluded text. The start
;;  should start with /* and the end end with */.
(defconst mupad-exclude-str-start "/*----\\/----- EXCLUDED -----\\/-----")
(defconst mupad-exclude-str-end   " -----/\\----- EXCLUDED -----/\\----*/")

;; On lengthy file, mupad.el has trouble finding that ... no recursive
;; editing is in process and that the user is simply in an open area, simply
;; because it looks for the first unclosed construct till the very beginning of
;; the buffer, were it finds none... The following regexp found at a beginning of
;; a line indicates that we are neither in a definition, neither in a commented area,
;; and that mupad.el should not worry about what is on top.
(defvar mupad-safe-place-regexp ;;"\\(^\\)[a-zA-Z]\\([a-zA-Z0-9_]\\|::\\)*[ \t\n]*:="
                                "\\(^\\)\\(//--+\\|/\\*-+-\\*/\\)$"
"Marker used to tell emacs this point is outside a commented area or a sexp.")
(defvar mupad-noft-safe 1 "Number of times `mupad-safe-place-regexp' should appear.")
(defvar mupad-string-opened 0)

;;----------------------------------------------------------------------

(defconst mupad-menu-separator (list "--------------"))
;; 100% internal. It is used for the menu-bar.

(require 'mupad-fontification)
(require 'mupad-cpl)
(require 'mupad-help)

(require 'mupad-run)
(require 'mupad-bus)
;;--------------------------------------
;; Part II  : Keymaps and syntax tables.
;;--------------------------------------

(defvar mupad-mode-map nil "Keymap used in Mupad mode.")

(defun mupad-toggle-electric-behavior (symbol val)
  "Change RET/M-RET from `sli-electric-terminate-line'
to `sli-newline' and reciprocally. Note that C-j is always
newline-and-indent in mupad-mode."
  (set symbol val)
  (let*((key (if val '("\r" "\M-\C-m") '("\M-\C-m" "\r"))))
    (cond
     ((eq symbol 'mupad-auto-indent) ; for mupad-mode
      (define-key mupad-mode-map (nth 0 key)
        (if mupad-electric-p 'sli-electric-terminate-line 'sli-newline))
      (define-key mupad-mode-map (nth 1 key) 'newline)))))

(defcustom mupad-auto-indent t
"Non-nil means emacs will try to indent properly each line ended
by a carriage return in mupad-mode."
:type 'boolean
:initialize 'custom-initialize-default ;if you use :set, you should specify :initialize!
:set 'mupad-toggle-electric-behavior
:group 'mupad-indentation)

(unless mupad-mode-map
  (let ((map (make-sparse-keymap)))
  (define-key map "\""       'mupad-electric-open-quote)
  (define-key map "("        'mupad-electric-open-brace)
  (define-key map "["        'mupad-electric-open-brace)
  (define-key map "{"        'mupad-electric-open-brace)
  (define-key map [(meta ?i)]          'mupad-complete)
  (define-key map [(meta control ?i)]  'mupad-complete) ; taken by linux !!
  (define-key map "\d"     'backward-delete-char-untabify)
  (define-key map [(meta ?*)]                 'mupad-star-comment)
  (define-key map [(control ?c) (control ?c)] 'comment-region)
  (define-key map [(control ?c) (control ?e)] 'sli-maid)
  (define-key map [(control ?c) (control ?f)] 'sli-tutor)
  (define-key map [(control ?>)]     'mupad-push-region)
  (define-key map [(control ?<)]     'mupad-pull-region)
  (define-key map [(meta ?p)]        'mupad-backward-to-same-indent)
  (define-key map [(meta ?n)]        'mupad-forward-to-same-indent)
  (define-key map [(control ?c) ?F]  'mupad-fun-to-proc)
  (define-key map [(control ?c) ?h]  'mupad-help-emacs-search)
  (define-key map [(control ?c) ?f]  'mupad-for)
  (define-key map [(control ?c) ?w]  'mupad-while)
  (define-key map [(control ?c) ?t]  'mupad-title)
  (define-key map [(control ?c) ?m]  'mupad-modify)
  (define-key map [(control ?c) ?e]  'mupad-else)
  (define-key map [(control ?c) ?l]  'mupad-local)
  (define-key map [(control ?c) ?p]  'mupad-proc)
  (define-key map [(control ?c) (control ?k)] 'mupad-kill-job)
  (define-key map [(meta ?l)]         'mupad-force-update-fontification)
  (define-key map [(meta control ?f)] 'mupad-forward-sexp)
  (define-key map [(meta control ?b)] 'mupad-backward-sexp)
  (define-key map [(control ?i)]     'mupad-tab)
  (setq mupad-mode-map map)
  (mupad-toggle-electric-behavior 'mupad-auto-indent mupad-auto-indent)))

(defvar mupad-mode-syntax-table nil
  "Syntax table in use in mupad-mode buffers.")

(unless mupad-mode-syntax-table
  (setq mupad-mode-syntax-table (make-syntax-table))
  (mapcar (lambda (acons) (modify-syntax-entry (car acons) (cdr acons) mupad-mode-syntax-table))
          '((?( . "()") (?) .  ")(") (?[ . "(]") (?] . ")[") (?{ . "(}") (?} . "){") ; parenthesis
            (?/ . ". 124b") (?* . ". 23") (?\n . "> b") (?\^m . "> b")         ; comments
            (?~ . "_") (?! . "_") (?% . "_")                          ; symbol constituent
            (?> . "." ) (?| . "." ) (?+ . ".") (?- . ".") (?= . ".") (?< . "." ) (?$ . ".") ; ponctuation
            (?_ . "w") (?` . "w")                 ; word constituent
            (?\\ . "\\") ; the escape character (to quote strings in strings)
           ))
  (set-syntax-table mupad-mode-syntax-table))

;; Global keys. They *should* be global.

(define-key esc-map "o" (function mupad-restore-wind-conf))

(define-key completion-list-mode-map [mouse-2] (function mupad-mouse-2))

(define-key minibuffer-local-completion-map " " 'self-insert-command)
;; It is usually 'minibuffer-complete-word, but C-i does that.

;; To remove temp-files even if we quit a bit violently:
(custom-add-option 'kill-emacs-hook (function mupad-clear-temp-files))

(defsubst safe-delete-file (afile)
  (if (file-exists-p afile) (delete-file afile)))

;;-----------------------------
;; Part III : Finders/Parsers.
;;-----------------------------

(defun mupad-backward-sexp nil
  (interactive)
  (if (and mupad-hash-comment (eq (following-char) ?#))
      (search-backward "#" nil t) (backward-sexp)))

(defun mupad-forward-sexp nil
  (interactive)
  (if (and mupad-hash-comment (eq (char-after (point)) ?#))
      (search-forward "#" nil t 2) (forward-sexp)))

;; Parsers related to 'mupad-mode:

(defsubst mupad-within-string nil
  (nth 3 (parse-partial-sexp (save-excursion
			       (beginning-of-line) (point)) (point))))

;; Comments : between # #, or /* */ or starting with //

(defun mupad-get-safe-place nil
  (save-excursion
    (if (re-search-backward mupad-safe-place-regexp nil t mupad-noft-safe)
        (match-end 1) (point-min))))

(defsubst mupad-within-emacs-comment (&optional starting-point)
  (let ((aux (mupad-get-safe-place)))
    (nth 4 (parse-partial-sexp (if starting-point (max aux starting-point) aux) (point)))))

(defsubst mupad-within-emacs-long-comment (&optional starting-point)
    (let* ((aux (mupad-get-safe-place))
           (res (parse-partial-sexp (if starting-point (max aux starting-point) aux) (point))))
      (and (nth 4 res) (not (nth 7 res)))))

(defsubst mupad-within-hash-comment (&optional starting-point)
  (and mupad-hash-comment
       (save-excursion
         (let* ((howmany 0) (pt (point))
                (aux (mupad-get-safe-place))
                (st (if starting-point (max aux starting-point) aux)))
           (goto-char st)
           (while (search-forward "#" pt t)
             (setq howmany (1+ howmany)))
           (not (zerop (mod howmany 2)))))))

(defun mupad-within-comment (&optional starting-point)
" t if point is within a commented area, nil otherwise.
The comment starts at the first character of the comment sequence."
  (save-match-data
   (or (mupad-within-emacs-comment starting-point)
       (and mupad-hash-comment
            (mupad-within-hash-comment starting-point)))))

(defun mupad-skip-comments (&optional starting-point limit)
  "Skips comments, white spaces, tab and newline characters.
Answers nil if no comment has been skipped."
  (interactive)
  (save-restriction
    (unwind-protect
        (save-match-data
          (when limit
            (narrow-to-region (or starting-point (point-min)) limit))
          (let ((has-been-used nil) (incomment nil))
            ;; Looking for empty stuff or comment start:
            (while (and (not (eobp))
                        (or (looking-at "[ \t\n]+\\(#\\|/[/\\*]\\)?\\|#\\|/[/\\*]")
                            (setq incomment (mupad-within-comment starting-point))))
              (setq has-been-used t)
              (if incomment
                  (re-search-forward "\n\\|\\'\\|\\*/\\|#" limit t)
                ;; We are outside comments:
                (skip-chars-forward " \t\n" limit)
                ;; A comment may start next door:
                (if (looking-at "#")
                    (re-search-forward "#" limit 1 2)))
              (forward-comment 100))
            has-been-used))  ;; 100 is as good a value as any, provided
                             ;; it is large.
      (when limit (widen)))))

(defun mupad-find-closing-one (regexp-beg expr-end)
  ;; Should be called from outside a commented area.
  ;; Answer nil if no proper end_proc is found.
  (save-excursion
    (let*((pt (point)) (case-fold-search nil)
          (regexp (concat "\\<" expr-end "\\>"))
          (pt-end (re-search-forward regexp nil t)))
      (while (mupad-within-comment)
       (setq pt-end (re-search-forward regexp nil t)))
      ;; pt-end is the candidate. Look whether there is another proc
      ;; before it:
      (if pt-end
          (progn
            (goto-char pt)
            (while (and pt-end
                        (re-search-forward
                          (concat "\\<" regexp-beg) pt-end t))
              (if (not (mupad-within-comment))
                (progn
                  ;; We have found another BEG before the closing END
                  ;; which should close before this one. Find a new candidate:
                  (setq pt (mupad-find-closing-one regexp-beg expr-end))
                  (if pt
                      (progn
                         (goto-char pt)
                         (setq pt-end (re-search-forward regexp nil t))
                         (goto-char pt))
                      ;; else trouble:
                      (setq pt-end nil)))
                ;; a proc within comment counts for nothing. Continue.
                ))
            (if pt-end (goto-char pt-end))
            (if (looking-at "[:;]") (forward-char 1))
            (point))
          nil))))

(defun mupad-find-closing-end_proc nil
  ;; Answers nil if no proper end_proc is found.
  ;; Should be called from outside a commented area.
  (mupad-find-closing-one "proc[ \n\t]*(" "end_proc"))

;;--------------------------------
;; Part IV  : Highlighting stuff.
;;--------------------------------

(defun mupad-remove-empty-strings (alist)
  (let ((new-list '()))
    (while alist
      (or (and (stringp (car alist)) (string-equal (car alist) ""))
          (setq new-list (cons (car alist) new-list)))
      (setq alist (cdr alist)))
    new-list))

(defun mupad-make-primitive-regexp-simple nil
   (let ((simples (mapcar 'list mupad-all-completions)))
     (setq mupad-primitive-regexp-simple
           (concat
            "\\<\\("
            (mapconcat
             (lambda (a) a)
             (mupad-remove-empty-strings
              (mapcar
               (lambda (achar)
                 (let ((aa (all-completions achar simples)))
                   (if aa (regexp-opt aa) "")))
               (mapcar 'char-to-string
                       (string-to-list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"))))
             "\\|")
            "\\)\\>"))))

(defun mupad-make-primitive-regexp-prefix-alist nil
  (setq mupad-primitive-regexp-prefix-alist
    (mupad-remove-empty-strings
      (mapcar
        (lambda (prefix)
          (let ((aa (cdr (assoc prefix mupad-libraries-completion-alist))))
            (if aa (list prefix (concat "\\<\\(" (regexp-opt aa) "\\)\\>")) "")))
        mupad-libraries-list))))

(defun mupad-make-regexp (name alist)
  (unless (eval name) ; auquel cas, c'est vrai, vaut mieux pas l'allouer !
    (set name (concat "\\<\\(" (regexp-opt alist) "\\)\\>"))))

;;----------------------------
;; Part V   : Window manager.
;;----------------------------

(defun mupad-mouse-2 (event)
  "A kind of hook for 'mouse-choose-completion."
  (interactive "e")
  (funcall 'mouse-choose-completion event)
  ;; 'mouse-choose-completion comes from the standard file "mouse.el".
  (mupad-restore-wind-conf))


;;-------------------------------------------
;; Part VI  : mupad-mode
;;            and some other main functions.
;;-------------------------------------------

(defun mupad-clear-temp-files nil
  (if zap-file
      (safe-delete-file zap-file))
  (safe-delete-file mupad-el-temp-file))

(defun mupad-add-imenu-index nil
   (if (progn (require 'easymenu) (featurep 'easymenu))
       (imenu-add-to-menubar "MuPAD-Index")))

(defsubst mupad-compact-list (lst)
  ; remove same consecutive occurences.
  (let* ((old (car lst)) (nlst (list old))  (lst (cdr lst)))
    (while lst
      (if (string-equal (car lst) old)
          (setq lst (cdr lst))
          (setq nlst (cons (setq old (car lst)) nlst) lst (cdr lst))))
    (nreverse nlst)))

(defun mupad-learns-indentation nil
  (require 'sli-tools)
  (sli-tools mupad-structures mupad-shift-alist mupad-separators
             'mupad-is-a-separatorp
             mupad-fixed-keys-alist
             mupad-safe-place-regexp
             mupad-keys-with-newline mupad-keys-without-newline
             mupad-add-to-key-alist
             '("//") mupad-no-heredity-list nil mupad-correction-alist)
  (setq sli-more-maidp mupad-more-maidp
        sli-tab-always-indent mupad-tab-always-indent))

;;;###autoload
(defun mupad-mode nil
  "Major mode version `mupad-mode-version' for editing Mupad code.
\\<mupad-mode-map>
The main work is to indent code correctly while editing,
to colour the code and to provide a menu-bar item.

Available special keys:
\\{mupad-mode-map}

\\[comment-region] will place '//' at beginning of each line in region

Main variable controlling indentation/edit style:

 `mupad-indent-level'      (default 2)
    Indentation of Mupad statements with respect to containing block.

Turning on Mupad mode calls the value of the variable mupad-mode-hook with
no args, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table mupad-mode-syntax-table)
  (setq case-fold-search nil) ; always local.
;bij - comments
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-start-skip) "^\\(\\(//\\)+ *\\)")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'block-comment-start) "/\*")
  (set (make-local-variable 'block-comment-end) "\*/")
  (set (make-local-variable 'words-include-escapes) t)
  (make-variable-buffer-local 'mupad-safe-place-regexp)
  (make-variable-buffer-local 'mupad-noft-safe)
  (make-variable-buffer-local 'mupad-string-opened) 

  (setq imenu-case-fold-search nil)
  (setq imenu-generic-expression
        (list (list "Functions:" mupad-function-def-start 1)
              (list "Global Vars:" mupad-global-var-start 1)))
  (mupad-create-completions)
  (mupad-make-regexp 'mupad-prefix-regexp mupad-libraries-list)
  (mupad-make-regexp 'mupad-type-regexp mupad-types-list)
  (mupad-make-regexp 'mupad-option-regexp mupad-options-list)
  (mupad-make-regexp 'mupad-keyword-regexp mupad-keywords-list)

  (unless mupad-primitive-regexp-simple (mupad-make-primitive-regexp-simple))
  (unless mupad-primitive-regexp-prefix-alist (mupad-make-primitive-regexp-prefix-alist))
  (use-local-map mupad-mode-map)
  (setq major-mode 'mupad-mode mode-name "MuPAD")

  (require 'cc-mode)
  (c-initialize-cc-mode)
  (set (make-local-variable 'fill-paragraph-function) 'c-fill-paragraph)
  (setq comment-line-break-function 'c-comment-line-break-function)
  (mupad-learns-indentation)
  ;; mupad-learns-indentation binds 'indent-line-function to 'sli-electric-tab
  ;; which in turn is bound to \C-i. We want a momre tricky behaviour:
  ;(setq indent-line-function mupad-select-tab)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  
  (run-hooks 'mupad-mode-hook) ;; fontification is done there
  (mupad-add-imenu-index)
  (mupad-init-menu-bar))

(defun mupad-toggle nil
  "Change RET/M-RET from `sli-electric-terminate-line'
to `sli-newline' and reciprocally"
  (interactive)
  (when (eq major-mode 'mupad-mode)
    (mupad-toggle-electric-behavior 'mupad-auto-indent (not mupad-auto-indent)))
  (message "Exchange bindings of RET/M-RET"))

;;----------------------------------------------
;;   Part VII : Functions for writing programs.
;;----------------------------------------------

(defun mupad-go-to-this-indent (step indent-level)
  "Move point repeatedly by <step> lines till the current line
has given indent-level or less, or the start/end of the buffer is hit.
Ignore blank lines and comments."
  (while (and
          (zerop (forward-line step))
          (or (looking-at "[ \t]*$")
              (looking-at "[ \t]*#")
              (> (current-indentation) indent-level)))
    nil))

(defun mupad-backward-to-same-indent ()
  "Move point backwards to nearest line with same indentation or less.
If not found, point is left at top of buffer."
  (interactive)
  (mupad-go-to-this-indent -1 (current-indentation))
  (back-to-indentation))

(defun mupad-forward-to-same-indent ()
  "Move point forwards to nearest line with same indentation or less.
If not found, point is left at start of last line in buffer."
  (interactive)
  (mupad-go-to-this-indent 1 (current-indentation))
  (back-to-indentation))

(defun mupad-for ()
  "Build skeleton for-loop statment, prompting for the loop parameters."
  (interactive)
  (let ((for (read-string "var: ")))
    (if (string-equal for "")
        (let ((to (read-string "to: ")))
          (if (not (string-equal to ""))
              (insert " to " to)))
      (insert "for " for)
      (let ((in (read-string "in: ")))
        (if (not (string-equal in ""))
            (insert " in " in)
          (let ((from (read-string "from: ")))
            (if (not (string-equal from ""))
                (insert " from " from)))
          (let ((to (read-string "to: ")))
            (if (not (string-equal to ""))
                (insert " to " to))))))
    (insert " do")
    (sli-electric-terminate-line (point))
    (insert "end_for;")
    (sli-electric-terminate-line (point))
    (mupad-force-update-fontification)
    (forward-line -2)
    (sli-indent-line)))

(defun mupad-while ()
  "Build skeleton while-loop statment, prompting for the loop parameters."
  (interactive)
  (insert "while " (read-string "conditional: "))
  (insert " do")
  (sli-electric-terminate-line (point))
  (sli-newline (point))
  (insert "end_while;")
  (mupad-force-update-fontification)
  (sli-electric-terminate-line (point))
  (forward-line -2)
  (sli-indent-line))

(defun mupad-title ()
  "Insert a comment block containing the module title, author, etc."
  (interactive)
  (let ((st (point)) (the-title (read-string "Title: " (buffer-name))))
    (goto-char (point-min))
    (insert "\n\n")
    (forward-char -2)
    (insert "/*    -*-MuPAD-*-\n"
            "\n     Title:     " the-title
            "\n     Created:   " (current-time-string)
            "\n     Author:    " (user-full-name)
            "\n                <" mupad-user-mail-address ">\n"
            "\n Description: \n"
            "\n Exported Libraries:\n\n*/\n")
    (mupad-force-update-fontification)
    (goto-char (1- (+ (point) st)))))

(defun mupad-modify ()
  "Insert a comment block containing the modification, author, etc."
  (interactive)
  (let ((st (point)) beg (modif (read-string "Modification: ")))
    (goto-char (point-min))
    (if (not (looking-at "[ \\w]")) (mupad-skip-comments))
    (while (char-equal (preceding-char) ?\n) (forward-char -1))
    (forward-char 1)
    (setq beg (point))
    (insert "\n/*    Modified: "
            (current-time-string)
            "\n      Author:   "
            (user-full-name)
            "\n      Modification: "
            modif "\n*/\n")
    (mupad-force-update-fontification)
    (goto-char st)))

(defun mupad-else ()
  "Add an elif clause to an if statement, prompting for the condition.
   When no condition is given, put an else."
  (interactive)
  (let ((condition (read-string "else/elif: "))
        (st (point)))
    (if (string-equal condition "")
        (insert "else ")
      (insert "elif " condition "then")
      (sli-electric-terminate-line (point)))
    (mupad-force-update-fontification)))

(defun mupad-local ()
  "Add a new local variable, inserting the word local if necessary."
  (interactive)
  (save-excursion
  (let ((newvar (read-string "New variable: ")))
    (beginning-of-line)
    (while (and (not (looking-at mupad-proc-def-start))
                (not (looking-at "[ \t]*proc[ \t]*("))
                (not (looking-at "[ \t]*local "))
                (not (bobp)))
      (forward-line -1))
    (let ((first-time (not (looking-at "[ \t]*local ")))
          (st (point)))
      (cond
       ((bobp) (message "I didn't find where to insert this variable"))
       (first-time
        (end-of-line)
        (sli-electric-terminate-line (point))
        (insert "local " newvar ";"))
       (t
        (search-forward ";")
        (forward-char -1)
        (insert ", " newvar)))
      (mupad-force-update-fontification)))))

(defun mupad-proc ()
  (interactive)
  ;; In this function, we let 'sli-electric-terminate-line do
  ;; the indentation job.
  (let ((name (read-string "Name: " )))
    (insert name ":=proc (" (read-string "Arguments: ") ")")
    (let ((options (read-string "Options: ")))
      (if (not (string-equal options ""))
        (progn
          (sli-electric-terminate-line (point))
          (insert "option " options ";"))))
    (sli-electric-terminate-line (point))
    (insert "begin")
    (sli-electric-terminate-line (point))
    (sli-newline (point))
    (insert "end_proc: /* End of " name " */")
    (mupad-force-update-fontification)
    (sli-electric-terminate-line (point))
    (forward-line -2)
    (sli-indent-line) (end-of-line)))

(defun mupad-display-comment ()
  "Inserts 3 comment lines, making a display comment."
  (interactive)
  (save-excursion
    (let ((st (point)))
      (insert "/*\n\n*/")
      (end-of-line)
      (mupad-force-update-fontification)
  (forward-char 3))))

(defun mupad-star-comment ()
  "Insert Mupad star comment at point."
  (interactive)
  (if (not (mupad-within-comment))
      (progn (if (or (bolp)
		     (string= (char-to-string
				(char-syntax (preceding-char))) " "))
		 (insert "/* ")
	       (insert " /* "))
	     (insert " */"))
    (message "Stay always in commented-area or parenthesis mismatch.")
    ))

(defun mupad-exclude-area (beg end)
  "Exclude an area of text."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward mupad-safe-place-regexp end t)
      (beginning-of-line) (insert " ") (end-of-line)
      (setq end (1+ end)))
    (goto-char end)
    (insert "\n" mupad-exclude-str-end "\n")
    (goto-char beg)
    (insert "\n" mupad-exclude-str-start "\n")
    (mupad-force-update-fontification)))

;;;
;;;  Electric functions
;;;

(defun mupad-electric-open-brace ()
  "Insert closed brace."
  (interactive)
  (if (or (not mupad-closed-brace) (not mupad-electric-p))
      (insert last-command-char)
    (let ((br (char-to-string last-command-char)))
         (insert br)
         (save-excursion
          (if (not (or (mupad-within-string)
		       (mupad-within-comment)
		       (string=
                         (char-to-string (char-syntax (following-char))) "w")))
	    (insert (cond ((string= br "(") ")")
		       	  ((string= br "[") "]")
			  ((string= br "{") "}"))))
      ))))

(defun mupad-electric-open-quote ()
  "Insert closed quote."
  (interactive)
  (if mupad-electric-p
    (if (mupad-within-string)
        (if (= mupad-string-opened 0)
	    (insert "\\\"")
	  (insert "\"")
     	  (setq mupad-string-opened 0))
      (insert "\"")
      (if (not (string= (char-to-string (char-syntax (following-char))) "w"))
	  (save-excursion
	    (insert "\""))
        (setq mupad-string-opened 1)))
    (insert "\"")))

(defun mupad-push-region (start end)
  "Ein Zeichen nach rechts einr=FCcken.
Adds one identation to the whole region."
  (interactive "r")
  (save-excursion
    (goto-char end)
    (setq end (point-marker))
    (goto-char start)
    (save-excursion
      (while (< (point) end)
	(progn (beginning-of-line)
	       (skip-chars-forward " \t")
	       (insert-char ?  1)
	       (if (not (eobp)) (forward-line 1)))))))

(defun mupad-pull-region (start end)
  "Ein Zeichen nach rechts einr=FCcken.
Removes one indentation to the right for the whole region."
  (interactive "r")
  (save-excursion
    (goto-char end)
    (setq end (point-marker))
    (goto-char start)
    (save-excursion
      (while (< (point) end)
	(progn (beginning-of-line)
	       (skip-chars-forward " \t")
	       (if (not (bolp))
		   (backward-delete-char-untabify 1))
	       (forward-line 1))))))

(defun mupad-indent-comment (&optional arg)
  "Indent current line as comment.
If optional arg is non-nil, just return the
column number the line should be indented to."
    (let* ((stcol (save-excursion
		    (re-search-backward (regexp-opt (list block-comment-start comment-start)) nil t)
		    (1+ (current-column)))))
      (if arg stcol
	(delete-horizontal-space)
	(indent-to stcol))))

;;---------------------------------
;; Part VIII: Completion mecanism.
;;---------------------------------
;; Three steps for completion: The usual one, <<-- also for fontify
;;   Adding exported libraries methods names, <<-- also for fontify
;;   Adding user-defined function/global-var names. <<-- choose if they should be fontified.

(defsubst mupad-add-symbol (x)
  (intern x mupad-completion-array))

(defsubst mupad-remove-symbol (x)
  (unintern x mupad-completion-array))

(defun mupad-simplify-cpl-lst (lst)
"  Simple enough: if a word appears alone and also followed by ::
then it is a library name and its companions should be skipped
and replaced by one single string, which is the library name.
The list is then sorted."
  (let ((new-lst '()) aux) ;(print (list "Let's simplify ..." lst))
    (mapcar
     (lambda (wd)
       (if (null (cadr (setq aux (split-string wd "::"))))
           (add-to-list 'new-lst wd)
         (unless (member (car aux) lst)
           (add-to-list 'new-lst wd))))
     lst)
     (sort new-lst 'string-lessp)))

(defun mupad-string-to-list (astring)
  "ASTRING is a succession of gp-words separated by , spaces or newlines.
The answer is the list of these words."
  (let ((lst nil) (beg 0) (end 1))
    (while (<= end (length astring))
      (cond ((member (aref astring (1- end)) '(?\  ?\n ?,))
             (if (not (= beg (1- end)))
                 (setq lst (nconc lst
                                  (list (substring astring beg (1- end))))))
             (setq beg end end (1+ end)))
            (t (setq end (1+ end)))))
    ;; taking care of the last one:
    (if (not (= beg (1- end)))
        (setq lst (nconc lst (list (substring astring beg (1- end))))))
    lst))

(defun mupad-sort-and-minimise (list1 list2)
  "Take two lists of strings and build the list of all their
elements with no repetition and sorted out."
   (let ((lst (sort (nconc list1
                           (mapcar
                             (lambda (elt) (if (member elt list1) "" elt))
                             list2))
                    'string-lessp)))
    (if (string= (car lst) "") (cdr lst) lst)))

(defsubst mupad-standard-lst (word comp)
   (cond ((and (string= (car comp) "") (null (nth 1 comp)))
          (list ""))
         ((null (nth 1 comp))
          (list (concat word (car comp))))
         (t (nth 1 comp))))

(defun mupad-merge-cpls (word comp1 comp2)
  (let* ((lst1 (mupad-standard-lst word comp1))
         (lst2 (mupad-standard-lst word comp2))
         (a-local-cpl-list (mapcar 'list (mupad-sort-and-minimise lst1 lst2))))
    ;(print a-local-cpl-list)
    (mupad-ask-cpl-via-list word 'a-local-cpl-list)))

(defun mupad-ask-cpl-via-list (word lst)
  "Careful! LST is a symbol whose value is a list of completion type,
ie a list of lists whose cars are strings used for completion."
  ;; LST can be an array also.
  (setq lst (symbol-value lst))
  (let ((comp (try-completion word lst))
        to-insert fun-list) ;(print (list "mupad-ask-cpl :: " comp))
    (cond ((equal comp nil)         ; No completion.
           (list "" nil))
          ((equal comp t)   ; Already complete.
           (list "" nil))
          ((> (length comp) (length word)) ; Some completion with a kernel.
           (setq to-insert (substring comp (length word)))
           (setq fun-list
                 (all-completions comp lst))
           (if (< (length fun-list) 2)
               (list to-insert nil)  ; Unique completion.
               (list to-insert fun-list)))
          (t (setq fun-list 
                   (all-completions comp lst))
             (if (< (length fun-list) 2)
                 (list "" nil)       ; Unique completion.
                 (list "" fun-list))))))

(defun mupad-needs-a-file-namep nil
  "Decide if a file name is required. Returns nil
if not and the beginning of the filename if it is."
  (save-excursion
    (let (word (pt (point)))
      (setq word "")
      (if (and (mupad-within-string)
               (progn
                 (search-backward "\"" nil t)
                 (setq word (buffer-substring-no-properties (+ 1 (point)) pt))
                 (skip-chars-backward " \n\r\t")
                 (= (preceding-char) ?())
               (progn
                 (forward-char -1)
                 (skip-chars-backward " \n\r\t")
                 (= (preceding-char) ?d))
               (progn
                 (forward-char -1)
                 (= (preceding-char) ?a))
               (progn
                 (forward-char -1)
                 (= (preceding-char) ?e))
               (progn
                 (forward-char -1)
                 (= (preceding-char) ?r))
               )
          word
        nil))))

(defun mupad-complete nil
" Attempts to complete a partially typed command.
Displays possible completions in the completion buffer if no
unique completion can be done."
 ;; We use a buffer named "*Completions*" which is the buffer usually
 ;; used, like when reading a file name from the minibuffer.
  (interactive)
  (condition-case err
      (let (word comp myname dir)
        (cond
         ((setq word (mupad-needs-a-file-namep))
          (if (file-name-absolute-p word)
              (setq dir (file-name-directory word)
                    word (file-name-nondirectory word))
            (setq dir "./")) ;(print (list dir (file-name-completion word dir)))
          (setq comp (list (if (stringp (setq myname (file-name-completion word dir)))
                               (substring myname (length word))
                             "")
                           (file-name-all-completions word dir))))
         (t
          (mupad-bus-backward-extended-mupadword)
          (setq word (buffer-substring-no-properties
                      (point) (mupad-bus-forward-extended-mupadword))
                comp (mupad-ask-cpl-via-list word 'mupad-completion-array))
          (setq comp (list (car comp) (mupad-simplify-cpl-lst (cadr comp))))))
        ;(print (list word comp))
          ;; Insert the beginning of the completion
          ;; BEFORE any window change :    
          (if (not (string= (car comp) ""))
              (insert (car comp))

            (if (or (equal (nth 1 comp) nil) ;; de try-completion
		    (equal (nth 1 comp) (list word))) ;; de mupad-simplify-cpl-lst
                ;; no match:
                (progn
		  (when (and (get-buffer "*Completions*")
			     (get-buffer-window "*Completions*"))
		    ;; Occurs whenever an earlier completion has
		    ;; been asked for.
					;(print "Restoring")
		    (mupad-restore-wind-conf))
		  (message "No completion found or already complete"))
              ;; more than two matches:
              (when (string= (car comp) "")
                ;; We do not display anything if a partial completion was possible.
                (if (not (and (get-buffer "*Completions*")
                              (get-buffer-window "*Completions*")))
                    ;; No use storing wind-conf if some completion is in
                    ;; progress.
                    (mupad-store-wind-conf))
                (with-output-to-temp-buffer "*Completions*"
                  (display-completion-list (nth 1 comp))))
              )) ) ; To check it, use "to" "beg" "be" "gen"
    (error (princ "An error occured in mupad-complete: ")(princ err) nil)))


(defun mupad-tab nil
  "First indent on odd number of hits, complete on even numbers ..."
  (interactive)
  (if (eq last-command 'mupad-tab)
      (progn
	(mupad-complete)
	(setq this-command 'mupad-even-tab))
    (sli-electric-tab)))

;;;###autoload
(defun mupad-create-completions ()
  ""
  (unless mupad-create-completions-donep
    (mapcar 'mupad-add-symbol mupad-all-completions)
    (mapcar 'mupad-add-symbol mupad-types-list)
    (mapcar 'mupad-add-symbol mupad-options-list)
    (mapcar 'mupad-add-symbol mupad-libraries-list)
    (mupad-add-symbol "stdlib") ; The only library with no methods attached
                                ; in mupad-libraries-completion-alist
    (mapcar 'mupad-add-symbol mupad-keywords-list)
    (mapcar (lambda (x)
              (let ((dom (cdr (assoc x mupad-libraries-completion-alist))))
                (if (not (eq dom nil))
                    (mapcar (lambda (m) (mupad-add-symbol (concat x "::" m)))
                            dom))))
            mupad-libraries-list)
    (setq mupad-create-completions-donep t)))


(defun mupad-extends-primitive-regexp-simple-from-libraries (prefix)
  (setq mupad-primitive-regexp-simple-from-libraries
        (if (null mupad-primitive-regexp-simple-from-libraries)
            (nth 1 (assoc prefix mupad-primitive-regexp-prefix-alist))
          (concat (substring (nth 1 (assoc prefix mupad-primitive-regexp-prefix-alist)) 0 -4)
                "\\|"
                (substring mupad-primitive-regexp-simple-from-libraries 4) "\\)\\>"))))

(defun mupad-add-regexp-names (regexp num fontifyp regexp-name)
  (save-excursion
    (let ((words '()) (case-fold-search nil))
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (add-to-list 'words (match-string-no-properties num)))
      (mapcar 'mupad-add-symbol words)
      (set regexp-name (if fontifyp (concat "\\<\\(" (regexp-opt words) "\\)\\>") "")))))

;;-------------------------------
;; Part IX  : Help and Examples.
;;-------------------------------

(defun mupad-show-mupad-info nil
  "Show mupad.el-info on another window."
  (interactive)
  (condition-case err
      (let ((wind (selected-window))
            (where-it-is "")
            (to-be-tested (list "/usr/local/lib/MuPAD/emacs/"
                                "/usr/local/share/lib/MuPAD/emacs/"
                                "/usr/share/lib/MuPAD/emacs/"
                                "/usr/local/lib/MuPAD/"
                                "/usr/local/share/lib/MuPAD/"
                                "/usr/share/lib/MuPAD/")))
        ;; Locate mupad.el-info:
        (mapcar (lambda (afile) (if (file-exists-p afile) (setq where-it-is afile)))
                (mapcar (lambda (apath) (expand-file-name (concat apath "/mupad.el-info")))
                        (append to-be-tested load-path)))
        (if (and mupad-el-info (file-exists-p mupad-el-info))
            (setq where-it-is mupad-el-info))
        
        (if (not (string-equal where-it-is ""))
            (progn
              ;; We switch to the buffer *MuPAD Help* and erase its content:
              (set-buffer (get-buffer-create "*MuPAD Help*"))
              (erase-buffer)
              (message where-it-is)  ;; tell *Messages* which version is used.
              (insert-file where-it-is)
              ;; Show the help buffer and tell user how to remove help window:
              (mupad-bus-window-manager "*MuPAD Help*" 'mupad-show-help)
              (setq buffer-read-only t)
              (search-forward "Usage" nil t)
              (beginning-of-line) (set-window-start (selected-window) (point))
              (mupad-info-wind-conf)
              (select-window wind))
          ;; Tell the user the file was not found:
          (mupad-bus-window-manager "*MuPAD Help*" 'mupad-beginning-temp)
          (insert "The file mupad.el-info was not found. You should discover where it is, say in the directory /usr/local/lib/MuPAD/emacs/ and add the line\n (setq load-path (concat load-path \"/usr/local/lib/MuPAD/emacs/\"))\nto your .emacs file (create it if it doesn't already exist).")
          (setq fill-column (1- (window-width (get-buffer-window "*MuPAD Help*"))))
          (fill-individual-paragraphs (point-min) (point-max) 'left)
          ;; Remove help window :
          (mupad-bus-window-manager "*MuPAD Help*" 'mupad-remove-help-old-config)
          (mupad-restore-wind-conf)))
    (error (princ "An error occured in mupad-info: ")(princ err) nil)))

(defun mupad-start-manual nil
  (interactive)
  (condition-case err
      (start-process-shell-command "Manual" "*Messages*" mupad-manual-command)
    (error (princ "Problem with the manual: ")(princ err) nil)))

;;---------------------------
;; Part XII  : Writing Tools
;;---------------------------

(defun mupad-comment-proc (name arguments options)
"Inserts javadoc-style comments."
  (let ((startpos 0))
    (insert "/**\n* Procedure " name " is not yet documented.\n* Insert description for procedure here.\n* @AUTHOR " "<A>href=\"mailto:"  mupad-user-mail-address "\">" (user-full-name) "</A>\n")
    (while (string-match " *\\([A-Za-z_]\\w*\\) *"
                         arguments startpos)
        (insert "* @PARAM "
          (substring arguments (match-beginning 1) (match-end 1))
          " not documented.\n")
        (setq startpos (match-end 0))
        (setq startpos (if (string-match "," arguments startpos)
                           (match-end 0) (length arguments))))
    (insert "* @RETURN " "return value not documented.\n*/")
    (sli-electric-terminate-line (point))
    (mupad-force-update-fontification)))

(defun mupad-describe-this-proc ()
  "Generate JavaDoc-style comment-header for procedure at cursor."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (looking-at " *\\([A-Za-z_]\\w*\\) *:= *proc*(\\(.*\\))")
       (let ((name (match-string-no-properties 1))
             (arguments (match-string-no-properties 2))
             (options nil)
            )
          (beginning-of-line)
          (mupad-comment-proc name arguments options))
     (message "Cursor is not on a function definition line."))))

(defun mupad-replace-hash-comment nil
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((endp t))
      (while (search-forward "#" nil t)
        (replace-match (if (setq endp (not endp)) "*/" "/*"))))))

(defun mupad-next-balanced-brace ()
  "Gibt die Position der nächsten geschlossenen Klamer zurück
 (innerhalb der Zeile)"
  (interactive)
  (save-excursion
    (nth 0 (parse-partial-sexp (point) (buffer-size) 0))
    (point-marker)))

;;; bij 
(defun mupad-fun-to-proc ()
  "Schneidet 'fun(...)' aus und gibt '(()->(...))' zurück"
  (interactive)
  (let ((now (point-max)) ende anf reg (case-fold-search nil))
  (while (progn (re-search-forward "\\\<fun\\\>" nil t)
                (re-search-backward "\\\<fun\\\>" 1 t))
    (if (string= (char-to-string (char-syntax (preceding-char))) "\"")
        (progn (message "fun als Zeichenkette")
               (sit-for 1)
               (goto-char (1+ (point))))
      (save-excursion (setq ende (mupad-next-balanced-brace)))
      (setq anf (point-marker))
      (replace-match "" t t)     (print (list anf ende "OOOOOOO"))
      (kill-region anf ende)     (print "OOOOOOO")
      (setq reg (substring (current-kill 0) 1 -1))
      (goto-char ende)
      (insert (concat "(()->(" (concat reg) "))"))))))

(defun mupad-clean-script nil
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (mupad-fun-to-proc )
    (mupad-replace-hash-comment)))

;;--------------------------------
;; Part XIII : Menu-bar builders.
;;--------------------------------

(defun mupad-customize-mupad-group nil
  (interactive)
  (mupad-store-wind-conf)
  (customize-group "mupad")
  (mupad-info-wind-conf))

(defmacro mupad-build-main-syn-menu nil
   (` (list
        (append
          (list "Shapes" :help "Templates"
            ["Procedure"  mupad-proc :active t :help "A procedure template"]
            ["Describe"   mupad-describe-this-proc :active t :included mupad-javadoc-stylep]
            ["Local"  mupad-local :active t :help "Declare another local variable"]
            ["Else/Elif"  mupad-else :active t :help "Insert an if/else/elif/end_if block"]
            ["While"  mupad-while :active t :help "Insert a while/end_while block"]
            ["For"  mupad-for :active t :help "Insert a for/end_for block"]
            "----------------------"
            ["Title"  mupad-title :active t :help "Insert heading to your program"]
            ["Modification"  mupad-modify :active t 
	     :help "Insert the description of modification to the heading to your program"]
            "----------------------"
            ["Exclude Area" mupad-exclude-area :active mark-active :help "Comment out the region"]
            ["Display Comments" mupad-display-comment :active t :help "Insert a large comment template"]
            ["/*...*/" mupad-star-comment :active t :help "Insert a comment template"]
            "----------------------"
            ["Fun to Proc" mupad-fun-to-proc :active t :help "Convert fun definitions to proc definitions"]
            ["Cleans" mupad-clean-script :active t :key-sequence nil
	     :help "Convert all fun to proc and replace hash comments by c-style comments"]))
        (list "Indentation"
          ["Push Region" mupad-push-region :active mark-active]
          ["Pull Region" mupad-pull-region :active mark-active]
          ["Indent Region" sli-indent-region :active mark-active :key-sequence nil]
          ["Forward to same indent"  mupad-forward-to-same-indent :active t]
          ["Backward to same indent" mupad-backward-to-same-indent :active t]))))

(defun mupad-environment-menu nil
  (list
   (list "Environment"
	["Set DIGITS..." mupad-bus-set-digits :active (processp mupad-bus-my-mupad-run-process)]
	["Adapt TEXTWIDTH" mupad-bus-adapt-textwidth :active (processp mupad-bus-my-mupad-run-process)
	 :help "Set the textwidth of the mupad process to the actual width of your window"]
        ["PrettyPrint switch" mupad-bus-prettyprint-switch :active (processp mupad-bus-my-mupad-run-process)
         :help "Toggle the value of PRETTYPRINT"]
	"--------------------"
	["Exchange Keys" mupad-toggle :active t :help "Exchange bindings of RET/M-RET" ]
	["Customize" mupad-customize-mupad-group
	 :active t :key-sequence nil])))

;;----------------------------
;; Add a menu to the menu-bar.
;;---------------------------

(defun mupad-menu-bar nil
  "Menu-bar item MuPAD"
   (append
    (list "MuPAD"
	  ["Start MuPAD" mupad-bus-start :included (featurep 'mupad-run)
	   :active t
	   :help "Start a mupad process in another buffer"]
	  (list
	   "Send file to MuPAD..."
	   ["Silently"  mupad-bus-file :active t :included (featurep 'mupad-run)
	    :help "Send a file to the mupad-process by `read(...):'"]
	   ["Openly"    mupad-bus-execute-file :active t :included (featurep 'mupad-run)
	    :help "Send a file to the mupad-process by `read(...);'"])
	  ["Send region to MuPAD"  mupad-bus-region :included (featurep 'mupad-run)
	   :active (and (featurep 'mupad-run) mark-active)]
	  "---------------------")
    (mupad-build-main-syn-menu)
    (list
     (list
      "Shortcuts"
      ["Further Statements" sli-maid :active t :help "Strive to continue the present construct"]
      ["Closes All Statements" sli-tutor :active t]
      ["Word completion" mupad-complete :active t :help "Also available via by pressing twice TAB"])
     "---------------------")
    (list
     ["Manual" mupad-start-manual :active t :key-sequence nil :help "Open the hytex manual"]
     ["Info on this mode" mupad-show-mupad-info :active t :key-sequence nil]
     "-----------------------"
     ["Help on ..." mupad-help-emacs-ask :key-sequence nil :help "Text help on a mupad object"])
    mupad-menu-separator
    (list ["Restore windows" mupad-restore-wind-conf :active (not (null mupad-registers-list))
	   :help "Go to previous window configuration"])
    (mupad-environment-menu)
    ;;"Colors" sub-menu is added here.
    ))

(defun mupad-init-menu-bar ()
  "Add menu-bar item MuPAD in mupad-mode"
  (when (and (featurep 'easymenu)
             (eq MuPAD-menu-map nil))
     (easy-menu-define MuPAD-menu-map mupad-mode-map
       "Menu-bar item used under mupad-mode"
       (mupad-menu-bar))))

;; mupad.el ends here.    1825 lines;;; mupad.el --- mupad editing support package

