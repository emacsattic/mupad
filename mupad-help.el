;; $Id$
;;; mupad-help.el -- online help for mupad-run-mode and mupad-mode

;; Copyright (C) 2002, Fran\c cois Maltey, Nicolas Thiery, Olivier Ramar\'e

;; Maintainer: Francois Maltey <Francois.Maltey@enst-bretagne.fr>
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

(provide 'mupad-help)
(require 'mupad-bus)
(defconst mupad-help-mode-version "3.00" "Version of `mupad-help.el'.")

(defun mupad-set-mupad-directory (sym val)
  (set sym val)
  (setq mupad-manual-command (concat mupad-directory "/share/bin/manual"))
  (when (featurep 'mupad-help)
    (setq  mupad-help-tree (concat mupad-directory "/share/doc/"))))

;; This variable should be set by a CONFIGURE if it ever exists...
(defcustom mupad-directory "//"
"Used for initializing `mupad-manual-command' and `mupad-help-tree'."
:initialize 'custom-initialize-default
:set 'mupad-set-mupad-directory
:type 'string :group 'mupad)

(defcustom mupad-help-tree (concat mupad-directory "/share/doc/")
"Location of the help files.
Used to set `mupad-help-file-name-tar' and `mupad-help-file-name-toc'."
:type 'string :group 'mupad-run)

(defcustom mupad-help-mode-hook nil
"Hook corresponding to help in mupad(-run)-mode."
:type 'hook :group 'mupad)

(defcustom mupad-help-examples-comment
  "///------ [extrait de l'aide en ligne]\n"
  "A string that is inserted first when an example is sent from the MuPAD help buffer to the MuPAD run buffer"
  :type 'string :group 'mupad-run)

(defcustom mupad-help-examples-execute
  nil
  "If non nil, the examples sent from the MuPAD help buffer to the MuPAD run buffer are immediately executed"
  :type 'boolean :group 'mupad-run)

(defvar mupad-help-file-name-tar      (concat mupad-help-tree "ascii.tar"))
(defvar mupad-help-file-name-indextty "ascii/.mupadhelpindextty")
(defvar mupad-help-file-name-toc      (concat mupad-help-tree "ascii.toc"))
(defvar mupad-help-file-name-index    "ascii/.mupadhelpindex")

(eval-and-compile
  (mapcar (lambda (sym) (eval (list 'defvar sym nil)))
  '(mupad-help-item-to-file mupad-help-toc-to-item 
    mupad-help-file-to-offset)))

(defvar xemacsp (string= (substring (version) 0 6) "XEmacs"))

(defvar mupad-help-mode-map 
  nil 
  "Touches d�finies dans l'aide en ligne de mupad.")

(when (not mupad-help-mode-map) 
  (let ((map (make-sparse-keymap)))
    (define-key map "q" (function mupad-help-quit))
    (define-key map "Q" (function mupad-help-quit))
    (define-key map "\r" (function mupad-help-return))
    (define-key map ">" (function end-of-buffer))
    (define-key map "<" (function beginning-of-buffer))
    (define-key map [f5] (function mupad-help-emacs-search))
    (define-key map "\C-c\C-h" (function mupad-help-emacs-search))
    (define-key map [f6] (function mupad-help-emacs-ask))
    (define-key map "\C-c\C-i" (function mupad-help-emacs-ask))
    (define-key map [mouse-2] (function mupad-help-mouse-2))
    (define-key map "\M-o" (function mupad-restore-wind-conf))
    (define-key map [(control left)] (function mupad-help-previous-example))
    (define-key map [(control right)] (function mupad-help-next-example))
    (define-key map [(control ?c) ?0]  (function mupad-help-reset))
    (setq mupad-help-mode-map map)))

;; 3776 symbols in MuPAD 2.0 ; a prime number as a length is a good thing !
(defvar mupad-help-completion-array 
  (make-vector 5003 0) "Obarray used for completion.")  
;;  
;;
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;
;;  Fonctions de l'aide en ligne 
;;    (y compris en cours de saisie, et recopie d'examples)
;;
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;
;; mupad-help-init-item-to-file 
;;   construit la liste (sous-titre de l'aide . nom du fichier de l'aide) ;
;;   elle est affect�e � la variable mupad-help-item-to-file
;;
;; mupad-help-init-toc-to-item 
;;   construit la liste (nom dans l'interface graphique . sous-titre) ;
;;   elle est affect�e � la variable mupad-help-toc-to-file
;;
;; mupad-help-init-file-to-offset
;;   construit la liste (fichier de l'aide . position dans l'archive) ;
;;   elle est affect�e � la variable mupad-help-file-to-offset.
;;
(defun mupad-help-init-item-to-file () 
  (let (br)
    (with-temp-buffer   
      (setq mupad-help-item-to-file ())
      (call-process "tar" nil t nil 
        "-Oxf" mupad-help-file-name-tar mupad-help-file-name-indextty)
      (goto-char 1)
      (while (re-search-forward "\\(.*\\)\\\\\\(.*\\)" nil t)
        (setq br (buffer-substring (match-beginning 1) (1- (match-end 1))))
        (setq mupad-help-item-to-file 
          (cons (cons br (match-string 2)) mupad-help-item-to-file))
        (intern br mupad-help-completion-array)
        (match-string 2))
      (setq mupad-help-item-to-file (reverse mupad-help-item-to-file)))))

(defun mupad-help-init-toc-to-item () 
  (with-temp-buffer   
    (setq mupad-help-toc-to-item ())
    (call-process "tar" nil t nil 
      "-Oxf" mupad-help-file-name-tar mupad-help-file-name-index)
    (goto-char 1)
    (while (re-search-forward "\\(.*\\)\\\\\\(.*\\)" nil t)
      (setq 
        mupad-help-toc-to-item 
        (cons 
          (cons 
            (match-string 2)
            (buffer-substring (match-beginning 1) (1- (match-end 1))))
          mupad-help-toc-to-item))))
  (setq mupad-help-toc-to-item (reverse mupad-help-toc-to-item)))

(defun mupad-help-init-file-to-offset () 
  (with-temp-buffer 
    (setq mupad-help-file-to-offset ())
    (call-process "cat" nil t nil mupad-help-file-name-toc)
    (goto-char 1) 
    (while 
      (re-search-forward 
        "^\\([^ ]+\\)[ ]+\\([^ ]+\\)[ ]+\\([^ ]+.\\)$" 
        nil t)
        (setq mupad-help-file-to-offset
          (cons 
            (cons (match-string 3) (cons (match-string 1) (match-string 2)))
            mupad-help-file-to-offset))))
  (setq mupad-help-file-to-offset (reverse mupad-help-file-to-offset)))

(defun mupad-help-from-file-to-buffer (ind)
  (let (br brb br1 bra brp br3 brm)
; recherche du fichier d'aide en ligne dans le fichier ascii.tar,
; br contient la paire point�e indiquant les positions de d�but et fin
    (and 
      (setq br (assoc (concat "ascii/" ind ".help") mupad-help-file-to-offset))
      (setq br (cdr br)))
    (unless br (error (concat "Help file `" ind "` is missing in ascii format")))
; cr�ation d'un tampon d'aide et remise � z�ro s'il existe d�j�.
    (mupad-bus-window-manager "*MuPAD*Help*" 'mupad-show-help)
    (set-buffer "*MuPAD*Help*")
    (setq buffer-read-only nil)
    (erase-buffer)
; insertion de l'aide en ligne
      (call-process "sh" nil t nil "-c" 
        (concat 
         "head -c" (cdr br) " " mupad-help-file-name-tar
         "| tail +" (number-to-string (+ (string-to-number (car br)) 1)) "c"))
; bascule en mode mupad-help
      (mupad-help-mode)
  ))

; We probably can rewrite most of the functions that deal with tar
; files using just this one-window
(defun mupad-help-display-help-file (file)
  ; Take as argument a file name, and displays it in the *MuPAD*Help* 
  ; buffer. The file name may be of the form file.tar#subfile.
  ; example: (mupad-help-display-help-file "/usr/local/MuPAD-dev/share/doc/ascii.tar#ascii/combinat_partitions.help")
    (mupad-bus-window-manager "*MuPAD*Help*" 'mupad-show-help)
    (set-buffer "*MuPAD*Help*")
    (setq buffer-read-only nil)
    (erase-buffer)
    (if (string-match "^\\(.*.tar\\)#\\(.*\\)$" file)
	(let ((tarfile (match-string 1 file))
	      (file    (match-string 2 file)))
	  (call-process "tar" nil t nil "--to-stdout" "-xf" tarfile file))
      (call-process "cat" file t nil))
    (mupad-help-mode)
  )

(defun mupad-help-mode ()
  "Major mode version `mupad-help-mode-version' for searching in Mupad help.
\\<mupad-help-mode-map>
The main work is to read mupad help files.

Available special keys:
\\{mupad-help-mode-map}"
  (interactive)
  (kill-all-local-variables)
; interpr�tation des codes de contr�les ANSI
  (let (br br1 bra brp br3 brm)
      (goto-char 1)
      (setq brp 0)
      (setq br 1)
      (while (setq br1 (re-search-forward "\033\\[.m" nil t))
       (setq bra (buffer-substring (- br1 2)(1- br1)))
       (delete-region (- br1 4) br1)
       (setq br1 (- br1 4))
       (cond 
         ((string= bra "0")(setq brm 0)) 
         ((string= bra "1")(setq brm (logior brp 1)))
         ((string= bra "4")(setq brm (logior brp 2))))
       (cond 
        ((eq brp 0) 
          (put-text-property br br1 'face 'mupad-help-face-normal))
        ((eq brp 1) 
          (put-text-property br br1 'face 'mupad-help-face-gras))
        ((eq brp 2) 
          (put-text-property br br1 'face 'mupad-help-face-soul))
        ((eq brp 3) 
          (put-text-property br br1 'face 'mupad-help-face-gras-soul)
	  (put-text-property br br1 'mouse-face 'highlight)))
      (setq br br1)
      (setq brp brm))
    (goto-char 1)
    (setq buffer-read-only t)
    (use-local-map mupad-help-mode-map)
    (mupad-help-info-mode))
; initialisation du tampon et du clavier
  (use-local-map mupad-help-mode-map)
; la barre de menu
;   (mupad-help-init-menu-bar)  
; configuration du mode majeur et �valuation du hook
  (setq major-mode 'mupad-help-mode) 
  (setq mode-name "MuPAD-help")
  (run-hooks 'mupad-help-mode-hook))

(defun mupad-help-emacs-search () 
  "Directly search the help file"
  (interactive)
  (let ((br1 (point)) bra brb)
; recherche le premier caract�re qui n'est pas dans un nom d'aide en ligne
    (setq bra (or (posix-search-backward "[^a-zA-Z0-9_:.]" nil t) 0))
    (goto-char br1)
; recherche la fin du mot dans l'aide en ligne, des lettres comprenant 
; �ventuellement :: ou ... ; 
; cette m�thode ne marche pas pour la recherche arri�re.
    (setq brb 
      (posix-search-forward 
        "\\([a-zA-Z0-9_]\\|\\(::\\)\\|\\(\\.\\.+\\)\\)*" 
        nil t))
    (condition-case err 
      (mupad-help-from-string-to-buffer (buffer-substring (1+ bra) brb))
      (error 
        (progn (message "%s" (error-message-string err)) (goto-char br1))))))

(defun mupad-help-mouse-2 (click) 
  "Directly search the help file"
  (interactive "e")
  (mouse-set-point click)
  (mupad-help-emacs-search))

(defun mupad-help-from-string-to-buffer (str)
  (let (br)
    (setq br (assoc str mupad-help-item-to-file))
    (unless br 
       (error (concat "Emacs can't find help-file for '" str "'")))
    (setq mupad-run-save-buffer (current-buffer))
    (mupad-help-from-file-to-buffer (cdr br))))

(defun mupad-help-from-toc-to-buffer (str)
  (let (br)
    (setq br (assoc str mupad-help-toc-to-item))
    (unless br 
       (error (concat "Emacs can't find help-file for '" str "'")))
    (setq mupad-run-save-buffer (current-buffer))
    (mupad-help-from-string-to-buffer (cdr br))))
;
; Pour l'aide en ligne (les fonctions sont d�finies � la suite)
;
(defun mupad-help-quit () 
  (interactive)
  (mupad-bus-window-manager "*MuPAD*Help*" 'mupad-remove-help-now))

(defun mupad-help-next-example ()
  (interactive)
  (if (< (point) (point-max)) (forward-char 1))
  (if (re-search-forward "^ *>> " nil t) 
    (forward-char -3)
    (goto-char (point-max))))

(defun mupad-help-previous-example ()
  (interactive)
  (if (re-search-backward "^ *>> " nil t)
    (progn ; This ensures that the cursor is under the first >
      (re-search-forward "^ *>> " nil t)
      (forward-char -3))
    (goto-char (point-min))))

(defun mupad-help-return ()
  (interactive)
  ; To be coherent with the previous functions, it might be better to
  ; test whether the current line starts by "^ *>> ", and if so, whether
  ; the cursor is in this portion of the line.
  (cond 
    ((string= (buffer-substring (point) (min (+ (point) 3)(point-max))) ">> ")
       (mupad-help-select-example))
    ((string= 
       (buffer-substring (max (- (point) 1) 0) (min (+ (point) 2)(point-max)))
       ">> ")
       (backward-char 1)
       (mupad-help-select-example))
    ((string= 
       (buffer-substring (max (- (point) 2) 0) (min (+ (point) 1)(point-max)))
       ">> ")
       (backward-char 2)
       (mupad-help-select-example))
    (t (mupad-help-emacs-search))))

(defun mupad-help-select-example ()
  (interactive)
  (save-excursion
    (let ((brp (point)) brs brp2 br br2)
      ; Get the text of the example from the *MuPAD*Help* buffer
      (beginning-of-line)
      (goto-char brp)
      (re-search-forward "\012\\s *\012\\s *\012")
      (setq br (match-beginning 0))
      (setq brs (buffer-substring-no-properties (+ brp 3) br))
      ; Go to the end of the MuPAD buffer, and make sure it ends by a newline
      (set-buffer "*MuPAD*")
      (goto-char (point-max))
      (unless (string= (buffer-substring (1-(point-max)) (point-max)) "\n")
	(insert "\n"))
      ; Insert the text of the example
      (insert mupad-help-examples-comment)
      (setq brp2 (point))
      (insert brs)
      ; Remove the extra indentation
      (goto-char brp2)
      (while (re-search-forward "^    \\( *\\)" nil t) (replace-match "\\1"))
      ; Execute the example (optional)
      (when mupad-help-examples-execute
	(mupad-run-return))
      (when (setq br2 (get-buffer-window "*MuPAD*"))
	(set-window-point br2 (point-max)))
      )))

;-------------------------------------------------------------------------
(defun mupad-help-completion (question)
  (save-excursion
    (let* 
      ((posfuncname
         (buffer-substring-no-properties 
           (or (mupad-bus-backward-extended-mupadword) (point))
	   (mupad-bus-forward-extended-mupadword)))
         (funcname (completing-read
           (if (string-equal posfuncname "") 
             (concat question ": ")
             (concat question " [" posfuncname "]: "))
           mupad-help-completion-array)))
      (and (string-equal funcname "") (setq funcname posfuncname)) funcname)))

(defun mupad-help-emacs-ask ()
  (interactive)
  (mupad-help-from-string-to-buffer (mupad-help-completion "Help about")))

(defun mupad-help-info-mode ()
   (message 
     "q/Q pour sortir, </> d�but/fin, voir aussi C-left/C-right/RET/C-cC-h"))

(defun mupad-help-reset ()
  (interactive)
  (unless mupad-run-less-questions
    (let 
      ((brd (read-from-minibuffer "Directory of the mupad help database: "
                                  mupad-help-tree)))
      (cond
        ((and 
           (file-readable-p (concat brd "ascii.tar"))
           (file-readable-p (concat brd "ascii.toc")))
          (setq mupad-help-file-name-tar (concat brd "ascii.tar"))
          (setq mupad-help-file-name-toc (concat brd "ascii.toc"))
          (setq mupad-help-tree brd)
          (setq mupad-help-init-item-to-file nil)
          (mupad-help-init))
        (t 
           (error 
 "Ascii.tar or ascii.toc files are missing or unreadable in the directory."
                 ))))))
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;
;;  Initialisation effectu�e au chargement.
;;
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;

(defface mupad-help-face-gras-soul
  `((t (:foreground "red")))
  "Bold and underlined face in mupad-help buffer"
  :group 'mupad-help-faces)

(defface mupad-help-face-gras
  `((((background dark)) (:foreground "lightblue"))
    (t                   (:foreground "blue")))
  "Bold face in mupad-help buffer"
  :group 'mupad-help-faces)

(defface mupad-help-face-soul 
  `((((background dark)) (:foreground "green"))
    (t                   (:foreground "forestgreen")))
  "Underlined face in mupad-help buffer"
  :group 'mupad-help-faces)

(defface mupad-help-face-normal 
  `((((background dark)) (:foreground "white"))
    (t                   (:foreground "black")))
  "Normal face in mupad-help buffer"
  :group 'mupad-help-faces)


(defun mupad-help-init nil
; construction des bases de donn�es de l'aide en ligne 
;    relations entre le nom du fichier dans l'aide en ligne, 
;    la position de ce fichier dans l'archive .tar, 
;    et le nom de cette m�me aide dans l'interface multi-fen�tre
  (unless mupad-help-item-to-file
    (mupad-help-init-item-to-file)
    (mupad-help-init-file-to-offset)
    (mupad-help-init-toc-to-item)))

;; On utilise custom-add-option plut�t que add-hook pour une meilleure
;; compatibilit� avec custom.
;; Bug corrig�: la customisation de mupad-run-mode-hook de l'utilisateur
;; dans le .emacs �tait �cras�e.
;; (add-hook 'mupad-mode-hook 'mupad-help-init)
(custom-add-option 'mupad-run-mode-hook 'mupad-help-init)
(custom-add-option 'mupad-mode-hook 'mupad-help-init)

