;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;
;; copyright 2002 par François Maltey : Francois.Maltey@enst-bretagne.fr
;;
;; Ce programme est un logiciel libre en cours de développement distribué 
;; sans aucun support ni garantie de la part de l'auteur. 
;; L'utilisateur est donc conscient qu'il le teste à ses risques et périls.
;;
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

(provide 'mupad-help)
(require 'mupad-bus)
(defconst mupad-help-mode-version "2.00" "Version of `mupad-help.el'.")

;; This variable should be set by a CONFIGURE if it ever exists...
(defvar mupad-directory "/usr/local/mupad/"
"Used for initializing some variables below.")

(defcustom mupad-help-tree (concat mupad-directory "/doc/")
"Location of the help files.
Used to set `mupad-help-file-name-tar' and `mupad-help-file-name-toc'."
:type 'string :group 'mupad-run)

(defvar mupad-help-file-name-tar      (concat mupad-help-tree "ascii.tar"))
(defvar mupad-help-file-name-indextty "ascii/.mupadhelpindextty")
(defvar mupad-help-file-name-toc      (concat mupad-help-tree "ascii.toc"))
(defvar mupad-help-file-name-index    "ascii/.mupadhelpindex")

(eval-and-compile
  (mapcar (lambda (sym) (eval (list 'defvar sym nil)))
  '(mupad-help-item-to-file mupad-help-toc-to-item 
    mupad-help-file-to-offset)))

(defvar mupad-help-mode-map 
  nil 
  "Touches définies dans l'aide en ligne de mupad.")
(unless mupad-help-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" (function mupad-help-quit))
    (define-key map "Q" (function mupad-help-quit))
    (define-key map [C-left] (function mupad-help-previous-exemple))
    (define-key map [C-right] (function mupad-help-next-exemple))
    (define-key map "\r" (function mupad-help-return))
    (define-key map ">" (function end-of-buffer))
    (define-key map "<" (function beginning-of-buffer))
    (define-key map [f5] (function mupad-help-emacs-search))
    (define-key map "\C-c\C-h" (function mupad-help-emacs-search))
    (define-key map [f6] (function mupad-help-emacs-ask))
    (define-key map "\C-c\C-i" (function mupad-help-emacs-ask))
    (define-key map [mouse-2] (function mupad-help-mouse-2))
    (define-key map "\M-o" (function mupad-restore-wind-conf))
    (setq mupad-help-mode-map map)))

(defconst mupad-help-face
  (if (or (eq frame-background-mode 'light) (not frame-background-mode))
    '((mupad-help-face-gras-soul    "red"         )
      (mupad-help-face-gras         "blue"        ) 
      (mupad-help-face-soul         "forestgreen" )
      (mupad-help-face-normal       "black"       ))
    '((mupad-help-face-gras-soul    "red"         )
      (mupad-help-face-gras         "lightblue"   ) 
      (mupad-help-face-soul         "lightgreen" )
      (mupad-help-face-normal       "white"       ))))

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
;;    (y compris en cours de saisie, et recopie d'exemples)
;;
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;
;; mupad-help-init-item-to-file 
;;   construit la liste (sous-titre de l'aide . nom du fichier de l'aide) ;
;;   elle est affectée à la variable mupad-help-item-to-file
;;
;; mupad-help-init-toc-to-item 
;;   construit la liste (nom dans l'interface graphique . sous-titre) ;
;;   elle est affectée à la variable mupad-help-toc-to-file
;;
;; mupad-help-init-file-to-offset
;;   construit la liste (fichier de l'aide . position dans l'archive) ;
;;   elle est affectée à la variable mupad-help-file-to-offset.
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
; br contient la paire pointée indiquant les positions de début et fin
    (and 
      (setq br (assoc (concat "ascii/" ind ".help") mupad-help-file-to-offset))
      (setq br (cdr br)))
    (unless br (error "Help file is missing in ascii format"))
; création d'un tampon d'aide et remise à zéro s'il existe déjà.
    (mupad-bus-window-manager "*MuPAD Help*" 'mupad-show-help)
    (set-buffer "*MuPAD Help*")
    (mupad-help-mode)
    (setq buffer-read-only nil)
    (erase-buffer)
; insertion de l'aide en ligne
      (call-process "sh" nil t nil "-c" 
        (concat 
         "head -c" (cdr br) " " mupad-help-file-name-tar
         "| tail +" (number-to-string (+ (string-to-number (car br)) 1)) "c"))
      (goto-char 1)
      (setq brp 0)
      (setq br 1)
; interprétation des codes de contrôles ansi
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
    (mupad-help-info-mode)))

(defun mupad-help-mode ()
  "Major mode version `mupad-help-mode-version' for searching in Mupad help.
\\<mupad-help-mode-map>
The main work is to read mupad help files.

Available special keys:
\\{mupad-help-mode-map}"
  (interactive)
  (kill-all-local-variables)
; initialisation du tampon et du clavier
  (use-local-map mupad-help-mode-map)
; la barre de menu
;   (mupad-help-init-menu-bar)  
; configuration du mode majeur et évaluation du hook
  (setq major-mode 'mupad-help-mode) 
  (setq mode-name "MuPAD-help")
  (run-hooks 'mupad-help-mode-hook))

(defun mupad-help-emacs-search () 
  "Directly search the help file"
  (interactive)
  (let ((br1 (point)) bra brb)
; recherche le premier caractère qui n'est pas dans un nom d'aide en ligne
    (setq bra (or (posix-search-backward "[^a-zA-Z0-9_:.]" nil t) 0))
    (goto-char br1)
; recherche la fin du mot dans l'aide en ligne, des lettres comprenant 
; éventuellement :: ou ... ; 
; cette méthode ne marche pas pour la recherche arrière.
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
; Pour l'aide en ligne (les fonctions sont définies à la suite)
;
(defun mupad-help-quit () 
  (interactive)
  (mupad-bus-window-manager "*MuPAD Help*" 'mupad-remove-help-now))

(defun mupad-help-next-exemple ()
  (interactive)
  (if (< (point) (point-max)) (forward-char 1))
  (if (search-forward ">> " nil t) 
    (forward-char -3)
    (goto-char (point-max))))

(defun mupad-help-previous-exemple ()
  (interactive)
  (unless (search-backward ">> " nil t) (goto-char (point-min))))

(defun mupad-help-return ()
  (interactive)
  (cond 
    ((string= (buffer-substring (point) (min (+ (point) 3)(point-max))) ">> ")
       (mupad-help-select-exemple))
    ((string= 
       (buffer-substring (max (1-(point)) 0) (min (+ (point) 2)(point-max)))
       ">> ")
       (backward-char 1)
       (mupad-help-select-exemple))
    ((string= 
       (buffer-substring (max (- (point) 2) 0) (min (+ (point) 1)(point-max)))
       ">> ")
       (backward-char 2)
       (mupad-help-select-exemple))
    (t (mupad-help-emacs-search))))

(defun mupad-help-select-exemple ()
  (let ((brp (point)) brs brp2 br br2)
      (beginning-of-line)
      (goto-char brp)
      (setq br (re-search-forward "\012 *\012"))
      (setq brs (buffer-substring-no-properties (+ brp 3) br))
      (set-buffer "*MuPAD*") 
      (goto-char (point-max))
      (unless (string= (buffer-substring (1-(point-max)) (point-max)) "\n")
        (insert "\n")) 
      (insert "///------ [extrait de l'aide en ligne]\n")
      (setq brp2 (point))
      (insert brs)
      (backward-char 1) 
      (goto-char brp2)
      (next-line 1)
      (beginning-of-line) 
      (while (< (point) (- (point-max) 4))
        (delete-char 4) (setq br (- br 4)) (next-line 1))
      (when (< (point) (point-max)) (delete-char 1))
      (setq br2 (point))
      (while (memq (char-before br2) '(?\n ?\t ?\ )) (setq br2 (1- br2)))
      (when (not (memq (char-before br2) '(?\: ?\;)))
        (goto-char br2) (insert " ;"))
      (goto-char (point-max))
      (when (setq br2 (get-buffer-window "*MuPAD*"))
        (set-window-point br2 (point-max)))))

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
     "q/Q pour sortir, </> début/fin, voir aussi C-left/C-right/RET/C-cC-h"))
;;  
;;
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;
;;  Initialisation effectuée au chargement.
;;
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;

(defun mupad-help-init nil
  ;; initialisation des couleurs
  ;;
  (mapcar 
   (lambda (a-face) (make-face (car a-face)) 
     (set-face-foreground (car a-face) (cadr a-face)))
   mupad-help-face)
  ;;
  ;; construction des bases de données de l'aide en ligne 
  ;;    relations entre le nom du fichier dans l'aide en ligne, 
  ;;    la position de ce fichier dans l'archive .tar, 
  ;;    et le nom de cette même aide dans l'interface multi-fenêtre
  ;;
  (unless mupad-help-item-to-file
    (mupad-help-init-item-to-file)
    (mupad-help-init-file-to-offset)
    (mupad-help-init-toc-to-item)))

(add-hook 'mupad-run-mode-hook 'mupad-help-init)
(add-hook 'mupad-mode-hook 'mupad-help-init)
