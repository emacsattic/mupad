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
;; Présentation dans le fichier mupad-run.el-info.
;; 
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;
;;  1/ Configuration, variables et constantes
;;  2/ Appel, lecture et sauvegarde des fichiers, impression 
;;  3/ Les sorties de mupad (y compris interruption et complétion)
;;  4/ Construction de la commande suivante de mupad et des séparateurs
;;  5/ Suppression et ajout de commentaires séparateurs
;;  7/ Traitement des appels systèmes et affichage graphique
;;  8/ Historique des commandes et recherche
;;  9/ Commandes de déplacement spéciales
;; 10/ Suppression (temporaire) d'affichage
;;   11/ Gestion du couper/coller
;;   12/ Ajout de hook avant et après les commandes envoyées à mupad
;;   13/ Affichage du temps de calcul (calculé par emacs)
;;   14/ Modification de la ligne d'état
;;   15/ Gestion des messages d'erreur d'emacs pour les adapter à mupad
;; 
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;
;; 1/ Configuration, variables et constantes
;;
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;
;; autres fichiers nécessaires 
;;
(require 'mupad-bus)
(require 'mupad-help)
(require 'gud)
;;
;;
;; défini l'extension mupad-run, et sa version
;;
(provide 'mupad-run)
(defconst mupad-run-mode-version "2.00" "Version of `mupad-run.el'.")
;;
;; Variables de configuration
;; 
;; mupad-run-pgm est le programme appelé, "mupad" en général
;;
;; mupad-run-pgm-opt est la liste des paramètres de la ligne de commande
;;   -R ou -E (-E est uniquement valable pour les versions >= 3.0) ;
;;   avec l'option -E les appels à l'aide en ligne de la forme ? sin 
;;   sont mieux gérés.
;;   Les options "-U" et "EMACS" permettent de déterminer dans mupad que
;;   le système a été lancé par emacs, pour configurer correctement
;;   vcam lors de l'initialisation de mupad, dans ~/.mupad/userinit.mu.
;;
;; La valeur de mupad-help-method concerne l'aide en ligne ; 
;; elle est 'mupad-help-from-toc-to-buffer avec l'option -R, 
;; et est 'mupad-help-from-file-to-buffer avec l'option -E.
;;
;; La variable mupad-run correspond indique le chemin d'accès au
;; fichier de présentation de mupad-run.
;;
;; La valeur de mupad-run-hisptory-max indique le nombre maximal de commandes
;; mémorisées dans l'historique de mupad.
;; 
(defgroup mupad-run nil
  "MuPAD customization subgroup the MuPAD shell"
  :group 'mupad :prefix "mupad-")

(defcustom mupad-run-pgm "mupad"
  "Command to run mupad"
  :type 'string :group 'mupad-run)

; modifié par la configuration automatique
(defcustom mupad-run-info 
  "/home/ramare/lisp/first-look/MuPAD/mupad-run.el-info"
  "Indique où est le fichier de présentation de mupad-run"
  :type 'string :group 'mupad-run)

(defvar mupad-help-method 'mupad-help-from-toc-to-buffer)
;mupad-help-from-toc-to-buffer  --> valable pour l'option -R
;mupad-help-from-file-to-buffer --> valable pour l'option -E

(defvar mupad-run-less-questions nil
  "Set it to t if you want a more automated behaviour.
In which case the options for starting mupad won't be asked for.")

(defun mupad-run-set-options (sym val)
  (set sym val)
  (cond ((member "-E" val)
          (setq mupad-help-method 'mupad-help-from-file-to-buffer))
        ((member "-R" val)
          (setq mupad-help-method 'mupad-help-from-toc-to-buffer))))

(defcustom mupad-run-pgm-opt
  '("-R" "-U" "EMACS")
  "Options given to the mupad process.
In fact other options can be given but one of these two blocks
should be present."
  :type '(choice (const ("-R" "-U" "EMACS")) (const ("-E" "-U" "EMACS")))
  :initialize 'custom-initialize-default
  :set 'mupad-run-set-options
  :group 'mupad-run)

(defcustom mupad-run-history-max 100
  "Nombre de commandes dans l'historique"
  :type 'integer :group 'mupad-run)

(defvar mupad-run-system-trace 3) 
;; 0 - n'affiche ni les commandes envoyées au système ni les résultats
;; 1 - affiche les commandes sans les résultats 
;; 2 - affiche les résultats sans les commandes et bloque la saisie
;; 3 - affiche les commandes et les résultats, et bloque la saisie
;;
(defvar mupad-run-system-exception '("vcam"))
;; les programmes appelés par mupad dont le nom est dans la liste 
;; mupad-run-system-exception sont exécutés en tâche de fond si la 
;; saisie de mupad est bloquée, et le contraire sinon.
;;
(defvar mupad-run-buffer-name 0)
;; 0 - mupad-run-mode possible uniquement pour un tampon *MuPAD*
;; 1 - mupad-run-mode possible uniquement pour un tampon *MuPAD* et *MuPAD*<2>
;; 2 - mupad-run-mode possible uniquement pour un tampon *MuPAD* et *MuPAD*<n>
;; 3 - mupad-run-mode possible pour tout tampon 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(eval-and-compile
  (mapcar (lambda (sym) (eval (list 'defvar sym nil)))
  '(mupad-run-edit mupad-run-todo mupad-run-comp-edit mupad-run-last-prompt
    mupad-run-itema mupad-run-itemb mupad-run-last-type
    mupad-run-output mupad-run-state mupad-run-time-start 
    mupad-run-hist-commands mupad-run-save-except mupad-run-debug 
    mupad-run-save-buffer mupad-run-comp-begin mupad-run-prompt
    mupad-run-rawcommand mupad-run-debugger-file mupad-run-debugger-line
    mupad-run-last-session mupad-run-buffer-face)))
;;
(defvar mupad-run-process nil)

;; test with (list-colors-display)
;; mupad-run-face : type -> nom-de-face-general -> nom-de-face-buffer 
;;      'mupad-run-face-result 
;;   -> 'mupad-run-face-result ou 'mupad-run-face-result-bg
;;   et 'mupad-run-face-result-fg -> grey95 ou grey50 ou ... selon le tampon
(defvar mupad-run-face 
  (if (or (eq frame-background-mode 'light) (not frame-background-mode))
   '((mupad-run-face-result            "darkblue"    "grey95"   ) 
     (mupad-run-face-prompt            "red"         "grey95"   ) 
     (mupad-run-face-local-prompt      "pink"        "grey95"   ) 
     (mupad-run-face-last-input        "black"       "grey95"   ) 
     (mupad-run-face-for-emacs         "grey50"      "grey95"   ) 
     (mupad-run-face-separator         "black"       "white"    ) 
     (mupad-run-face-call-system       "firebrick"   "grey95"   ) 
     (mupad-run-face-system            "saddlebrown" "grey95"   )
     (mupad-run-face-completion        "darkgreen"   "grey95"   )
     (mupad-run-face-error             "black"       "lightpink")  
     (mupad-run-face-waiting-commands  "black"       "cyan"     ) 
     (mupad-run-face-beginning-rem     "red"         "white"    ) 
     (mupad-run-face-beginning-waiting "red"         "cyan"     )
     (mupad-run-face-result-flag       "darkblue"    "lightblue") 
     (mupad-run-face-prompt-flag       "red"         "lightblue") 
     (mupad-run-face-local-prompt-flag "pink"        "darkblue") 
     (mupad-run-face-last-input-flag   "black"       "lightblue") 
     (mupad-run-face-for-emacs-flag    "grey50"      "lightblue") 
     (mupad-run-face-separator-flag    "black"       "lightblue") 
     (mupad-run-face-call-system-flag  "firebrick"   "lightblue") 
     (mupad-run-face-system-flag       "saddlebrown" "lightblue")
     (mupad-run-face-completion-flag   "darkgreen"   "lightblue")
     (mupad-run-face-error-flag        "black"       "lightblue"))
   '((mupad-run-face-result            "lightblue"   "grey25"  ) 
     (mupad-run-face-prompt            "red"         "grey25"  ) 
     (mupad-run-face-local-prompt      "pink"        "grey25"  ) 
     (mupad-run-face-last-input        "white"       "grey25"  ) 
     (mupad-run-face-for-emacs         "grey50"      "grey25"  ) 
     (mupad-run-face-separator         "white"       "black"   ) 
     (mupad-run-face-call-system       "firebrick"   "grey25"  ) 
     (mupad-run-face-system            "saddlebrown" "grey25"  )
     (mupad-run-face-completion        "lightgreen"  "grey25"  )
     (mupad-run-face-error             "white"       "maroon"  )  
     (mupad-run-face-waiting-commands  "white"       "darkcyan") 
     (mupad-run-face-beginning-rem     "red"         "black"   ) 
     (mupad-run-face-beginning-waiting "red"         "darkcyan")
     (mupad-run-face-result-flag       "lightblue"   "blue") 
     (mupad-run-face-prompt-flag       "red"         "blue") 
     (mupad-run-face-local-prompt-flag "pink"        "blue") 
     (mupad-run-face-last-input-flag   "white"       "blue") 
     (mupad-run-face-for-emacs-flag    "grey50"      "blue") 
     (mupad-run-face-separator-flag    "white"       "blue") 
     (mupad-run-face-call-system-flag  "firebrick"   "blue") 
     (mupad-run-face-system-flag       "brown"       "blue")
     (mupad-run-face-completion-flag   "lightgreen"  "blue")
     (mupad-run-face-error-flag        "white"       "blue"))))

(setq mupad-run-color-change 
  '((("grey95" . "A") "grey85") 
    (("grey25" . "A") "grey40")))

(defvar mupad-run-mode-map nil "Touches définies par mupad-run-mode.")
(unless mupad-run-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [C-return] (function mupad-run-creturn))
    (define-key map [C-up] (function mupad-run-previous-history))
    (define-key map [C-down] (function mupad-run-next-history))
    (define-key map [C-prior] (function mupad-run-previous-history-search))
    (define-key map [C-next] (function mupad-run-next-history-search))
    (define-key map [C-left] (function mupad-run-left))
    (define-key map [C-right] (function mupad-run-right))
    (define-key map [C-delete] (function mupad-run-hide))
    (define-key map [C-insert] (function mupad-run-show))
    (define-key map "\r" (function mupad-run-return))
    (define-key map "\C-d" (function mupad-run-suppression))
    (define-key map [delete] (function mupad-run-suppression))
    (define-key map [backspace] (function mupad-run-backspace))
    (define-key map "\C-i" (function mupad-run-tab))
    (define-key map "\C-c\C-c" (function mupad-run-break))
    (define-key map "\C-c\C-s" (function mupad-run-save))
    (define-key map "\C-c\C-w" (function mupad-run-save))
    (define-key map "\C-ck" (function mupad-run-end))
    (define-key map "\C-c0" (function mupad-run-reset))
    (define-key map "\C-c1" (function mupad-run-insert-last-session))
    (define-key map [f5] (function mupad-help-emacs-search))
    (define-key map "\C-c\C-h" (function mupad-help-emacs-search))
    (define-key map [f6] (function mupad-help-emacs-ask))
    (define-key map "\C-c\C-i" (function mupad-help-emacs-ask))
    (define-key map "\C-y" (function mupad-run-yank))
    (setq mupad-run-mode-map map)))

(defun mupad-run-set-arrow-behaviour (symbol val)
  "See `mupad-run-arrow-behaviour'"
  (setq mupad-run-arrow-behaviour val)
  (cond 
    ((string= val "Usual")
      (define-key mupad-run-mode-map 
        [C-up] (function mupad-run-previous-history))
      (define-key mupad-run-mode-map 
        [C-down] (function mupad-run-next-history))
      (define-key mupad-run-mode-map [up] (function previous-line))
      (define-key mupad-run-mode-map [down] (function next-line)))
    (t 
      (define-key mupad-run-mode-map 
        [up] (function mupad-run-previous-history))
      (define-key mupad-run-mode-map [down] (function mupad-run-next-history))
      (define-key mupad-run-mode-map [C-up] (function previous-line))
      (define-key mupad-run-mode-map [C-down] (function next-line)))))

(defcustom mupad-run-arrow-behaviour
  "Usual"
  "Selects the behaviour of the arrow up and down :
  the usual behaviour corresponds to up and down
  while C-up and C-down correspond to history.
  When in Bash-Style, this behaviour in exchanbed."
  :type '(choice (const "Usual") (const "Bash-Style"))
  :initialize 'custom-initialize-default
;do not use mupad-run-set-arrow-behaviour
;initially since the map is not yet defined !
  :set 'mupad-run-set-arrow-behaviour
  :group 'mupad-run)

(defconst mupad-run-automate-exception
  '((( 0 . ?\n) .  1) (( 0 . ?\\) .  2) (( 0 . ?\") . 10) (( 0 . ?\/) .  3) 
    (( 0 . ?\*) .  5)
    (( 2 . ?\n) .  0)
    (( 1 . ?\") . 10) (( 1 . ?\\) .  2) (( 1 . ?\n) .  1) (( 1 . ?\/) .  4)
    (( 1 . ?\*) .  5)
    ((11 . ?n ) . 10) ((11 . ?r ) . 10) ((11 . ?t ) . 10) ((11 . ?\\) . 10)
    ((11 . ?\") . 10)
    ((10 . ?\\) . 11) ((10 . ?\") .  0)
    (( 3 . ?\n) .  1) (( 3 . ?\\) .  2) (( 3 . ?\") . 10) (( 3 . ?\/) . 20)
    (( 3 . ?\*) . 30)
    (( 5 . ?\n) .  1) (( 5 . ?\/) . 70)
    ((20 . ?\n) .  1)
    (( 4 . ?\n) .  1) (( 4 . ?\\) .  2) (( 4 . ?\") . 10) (( 4 . ?\/) . 55)
    (( 4 . ?\*) . 80)
    ((55 . ?\n) .  1) ((55 . ?\/) . 22)
    ((22 . ?\n) .  1) ((22 . ?\-) . 23)
    ((23 . ?\n) .  1) ((23 . ?\-) . 24)
    ((24 . ?\n) .  1) ((24 . ?\-) . 25)))

(defconst mupad-run-automate-general 
    '(( 0 .  0) (10 . 10) (11 . 60) ( 2 . 50) ( 1 .  0) ( 3 .  0) ( 5 .  0)
      (20 . 20) ( 4 .  0) (55 . 20) (22 . 20) (23 . 20) (24 . 20)))

(defadvice save-buffers-kill-emacs (before mupad-run-quit-emacs)
  "Propose to save the *MuPAD* buffers."
  (mapcar 
    (lambda (buf)
      (set-buffer buf)
      (when (eq major-mode 'mupad-run-mode)
        (let ((inhibit-read-only t))(switch-to-buffer buf) (kill-buffer buf))))
    (buffer-list)))

(ad-activate 'save-buffers-kill-emacs)

;;
;;
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;
;; 2/ Gestion de l'appel et des sorties de mupad 
;;
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; lancement et fin d'une session de mupad, 
;; affichage des sorties de mupad
;;
;; le mode mupad-run est défini par ces variables :
;;   un tampon de nom *MuPAD*                               
;;   un processus pointé                                 [ mupad-run-process ]
;;   l'état du programme mupad (attente ou calcul)         [ mupad-run-state ]
;;   valeurs possibles: 'beginning 'wait-input 'wait-debugger-input 'running
;;   la chaîne de caractères résultat                     [ mupad-run-output ]
;;   l'instant de lancement de la dernière commande   [ mupad-run-time-start ]
;;   les commandes associées au clavier                 [ mupad-run-mode-map ]
;;   un marqueur sur le début des calculs à faire           [ mupad-run-todo ]
;;   un marqueur sur le début de la zone éditable           [ mupad-run-edit ]
;;   un marqueur au début du prompt précédent        [ mupad-run-last-prompt ]
;;   un marqueur où insérer la complétion              [ mupad-run-comp-edit ]
;;   début du nom dont la complétion est recherchée   [ mupad-run-comp-begin ]
;;   chaîne du prompt                                     [ mupad-run-prompt ]
;;   un compteur pour séparer les commandes en attente     [ mupad-run-itema ]
;;   un compteur pour séparer les sorties de mupad         [ mupad-run-itemb ]
;;   les attributs des polices de caractères                [ mupad-run-face ]
;;   l'historique des commandes                        [ mupad-hist-commands ]
;;   la dernière commande d'entrée envoyée à mupad    [ mupad-run-rawcommand ]
;;   affichage de messages de debug                          [mupad-run-debug]
;;        mupad-run-last-session mupad-run-save-except
;;
;; fonctions principales : 
;;   lancement du mode mupad-run dans un nouveau tampon          [ mupad-run ]
;;   sortie de  mupad-run après confirmation de sauvegarde   [ mupad-run-end ]
;;   réinitialisation du mode mupad-run                    [ mupad-run-reset ]
;; Fonctions : 
;;   mupad-run, mupad-run-end, mupad-run-reset
;;   mupad-run-filter, mupad-run-output-complete-data, mupad-run-print,
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-buffer-name-p (str) 
  (cond 
    ((= mupad-run-buffer-name 0) (string= str "*MuPAD*")) 
    ((= mupad-run-buffer-name 1) 
      (or (string= str "*MuPAD*") (string= str "*MuPAD*<2>")))
    ((= mupad-run-buffer-name 2) 
      (or (string= str "*MuPAD*")
          (and (string= (substring str 0 (min 8 (length str))) "*MuPAD*<")
               (string= (substring str -1) ">"))))
    ((= mupad-run-buffer-name 3) t)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-ask-pgm-opt nil
  (unless mupad-run-less-questions
    (let ((cmd-to-start
	   (read-from-minibuffer
	    "Command to start mupad: "
	    (concat mupad-run-pgm " "
		    (mapconcat (lambda (wd) wd) mupad-run-pgm-opt " ")))))
      (mupad-run-set-options
       'mupad-run-pgm-opt (cdr (split-string cmd-to-start " "))))))

(defalias 'run-mupad 'mupad-run)
(defun mupad-run nil
  (interactive)
  (mupad-run-ask-pgm-opt)
  (switch-to-buffer "*MuPAD*")
  (mupad-run-mode))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-end ()
   (interactive)
  (let ((br))
   (when (setq br (get-buffer "*MuPAD*")) 
     (switch-to-buffer br) (mupad-run-end))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-reset ()
   (interactive) (mupad-end) (mupad-run))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-mode ()
  "Major mode version `mupad-run-mode-version' for running Mupad code.
\\<mupad-run-mode-map>
The main work is to run mupad in an emacs buffer.

Available special keys:
\\{mupad-run-mode-map}"
  (interactive)
  (if 
    (and 
      (eq major-mode 'mupad-run-mode) 
      mupad-run-process
      (processp mupad-run-process))
    (message "Buffer already in mupad-run mode")
    (cond 
      ((eq major-mode 'mupad-run-mode)
        (error "Mupad doesn't run in this mupad-run buffer"))
      ((and (not (eq major-mode 'mupad-run-mode)) mupad-run-process)
        (error "Mupad runs inside a buffer in an other mode"))
      ((not (mupad-run-buffer-name-p (buffer-name (current-buffer))))
        (error "Buffer name isn't allowed for mupad-run mode")))
;    (setq delete-exited-processes t)
    (kill-all-local-variables)
; initialisation du tampon et du clavier
    (use-local-map mupad-run-mode-map)
    (mupad-run-set-arrow-behaviour nil mupad-run-arrow-behaviour)
    (add-hook 'kill-buffer-query-functions 'mupad-run-before-kill)
    (goto-char 1)
    (set-text-properties (point-min) (point-max) nil)
    (mapcar     
      (lambda (var) (make-local-variable var))
      '(mupad-run-history-max mupad-run-system-trace 
        mupad-run-system-exception mupad-run-process
        mupad-run-edit mupad-run-todo mupad-run-comp-edit 
        mupad-run-last-prompt mupad-run-hist-commands 
        mupad-run-output mupad-run-state 
        mupad-run-itema mupad-run-itemb mupad-run-last-type 
        mupad-run-time-start mupad-run-comp-begin mupad-run-prompt
        mupad-run-save-except mupad-run-debug
        mupad-run-last-session mupad-run-buffer-face
; NT 04/11/2002 added for the debugger
        mupad-run-rawcommand
	mupad-run-debugger-file	mupad-run-debugger-line
	gud-comint-buffer gud-find-file))
    (when (string= (buffer-name) "*MuPAD*<2>")
      (setq mupad-run-buffer-face "A"))
    (run-hooks 'mupad-run-mode-hook-before)
    (setq mupad-run-edit (make-marker))
    (set-marker mupad-run-edit (point))
    (setq mupad-run-todo (make-marker))
    (set-marker mupad-run-todo (point))
; autres markeurs pour la complétion et les messages d'erreur
    (setq mupad-run-comp-edit (make-marker))
    (set-marker mupad-run-comp-edit nil)
    (setq mupad-run-last-prompt (make-marker))
    (set-marker mupad-run-last-prompt nil)
    (setq mupad-run-itema 1) 
    (setq mupad-run-itemb 1) 
    (setq mupad-run-last-type -1)
; gestion de l'historique 
    (setq mupad-run-hist-commands (head-tail-void))
    (set-ptr-head mupad-run-hist-commands)
; lancement du programme 
    (setq mupad-run-output "")
    (setq mupad-run-state 'beginning)
    (setq mupad-run-process 
      (apply (function start-process) 
         "mupad" (current-buffer) mupad-run-pgm mupad-run-pgm-opt))
    (set-process-filter mupad-run-process (function mupad-run-filter))
    (setq mupad-run-time-start (current-time))
; la barre de menu
    (mupad-run-init-menu-bar)  
    (add-hook 'menu-bar-update-hook
              '(lambda nil
                 (when (eq major-mode 'mupad-mode)
                   (easy-menu-add-item MuPAD-menu-map nil
                                       ["quit"   mupad-end]
                                       "Send file to MuPAD..."))))
; configuration du mode majeur et évaluation du hook
    (setq major-mode 'mupad-run-mode) 
    (setq mode-name "MuPAD-run")
; affichage de messages de debug
    (setq mupad-run-debug ())
; the last raw command sent to MuPAD:
;   nil or a list (message-type message) like (1 "x+1")
    (setq mupad-run-rawcommand nil)
; the file position in the debugger: nil or a string
    (setq mupad-run-debugger-file nil)
; the file position in the debugger: nil or an int
    (setq mupad-run-debugger-line nil)
; two variables to trick gud into thinking that this buffer is a
; normal gud-buffer so that we can use gud-display-line
    (setq gud-comint-buffer (current-buffer))
    (setq gud-find-file 'gud-gdb-find-file)
    (run-hooks 'mupad-run-mode-hook)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-end () 
  "Kill the first buffer in mupad-run-mode"
  (interactive)
  (let ((bfl (buffer-list)) (inhibit-read-only t))
    (save-excursion
      (while bfl
      (set-buffer (car bfl))
      (when (eq major-mode 'mupad-run-mode)
; kill-process est superflu car il attaché au tampon
        (when (not mupad-run-save-except)
          (setq mupad-run-save-except 'end))
        (kill-buffer (current-buffer))
        (setq mupad-run-process nil)
        (setq bfl nil))))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-reset () 
  (interactive) 
  (let 
    ( (br (buffer-name (current-buffer))) (brc mupad-run-hist-commands) 
      br1 (mupad-run-save-except 'reset) (inhibit-read-only t))
    (mupad-run-store-line (buffer-substring mupad-run-edit (point-max)))
    (mupad-run-save)
    (setq br1 mupad-run-last-session)
    (when (processp mupad-run-process)
;      (setq br1 (mupad-run-save-br))
;      (setq mupad-run-process nil)
;      (setq br1 
;      (let (mupad-run-last-session) (kill-buffer br) mupad-run-last-session))
      (setq mupad-run-save-except 'kill)
      (kill-buffer br)
      (switch-to-buffer (set-buffer (get-buffer-create br)))
      (mupad-run-mode)
      (setq mupad-run-last-session br1)
      (setq mupad-run-hist-commands brc)
;      (insert-buffer br1)
;      (kill-buffer br1)
      (goto-char (point-max)))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-insert-last-session ()
  "Insert the last MuPAD session"
  (interactive)
  (when mupad-run-last-session  
    (goto-char (point-max))
    (unless (eq (char-before (point)) ?\n) (insert "\n"))
    (insert mupad-run-last-session)))
    
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-save-br ()
  (let (brn br br1 br2 (brb (current-buffer)) brp1 brp2 brp3 
    (inhibit-read-only t))
    (setq brp1 (marker-position mupad-run-todo))
    (setq brp2 (marker-position mupad-run-edit))
    (if (marker-position mupad-run-last-prompt)
      (setq brp3 
        (+ 
          (marker-position mupad-run-last-prompt) 
          (length mupad-run-prompt) 2)))
    (setq brn (generate-new-buffer-name "tmp_mupad_file"))
    (setq brn (get-buffer-create brn))
    (set-buffer brn)
    (insert-buffer-substring brb)
    (when (< brp2 (point-max))
      (goto-char brp2) (insert "///--- commandes en cours de saisie\n"))
    (when (< brp1 brp2) 
      (goto-char brp1) (insert "///--- commandes non encore évaluées\n"))
    (when (and brp3 (<= brp3 brp1))
      (goto-char (- brp3 2))
      (insert "///--- commandes en cours d'évaluation\n"))
    (goto-char (point-min))
    (while (not (eobp))
      (setq br1 (get-text-property (point) 'mupad-run-char-prop))
      (setq br2 (or (next-property-change (point)) (point-max)))
      (setq br (get-text-property (point) 'to-insert))
           (when br (insert br) (setq br2 (+ br2 (length br))))
      (cond 
        ((and (eq br1 'mupad-run-face-prompt) (eq (char-after) ?\n))
          (forward-char 1))
        ((memq br1 
           '(mupad-run-face-result      mupad-run-face-local-prompt 
             mupad-run-face-for-emacs   mupad-run-face-prompt
             mupad-run-face-call-system mupad-run-face-system 
             mupad-run-face-completion  mupad-run-face-error
             mupad-run-face-result-flag    
             mupad-run-face-local-prompt-flag 
             mupad-run-face-for-emacs-flag   
             mupad-run-face-prompt-flag
             mupad-run-face-call-system-flag 
             mupad-run-face-system-flag
             mupad-run-face-completion-flag  
             mupad-run-face-error-flag))
           (delete-region (point) br2))
         ((get-text-property (point) 'not-save)
           (delete-region (point) (1+ br2)))
;        ((eq br1 'mupad-run-face-last-input) (goto-char br2))
        (t (goto-char br2))))
  (set-text-properties (point-min) (point-max) nil)
  (when (and (> (point) 1)(not (eq (char-before (point)) ?\n))) (insert "\n"))
  brn))

(defun mupad-run-save ()
  "Saves the MuPAD commands"
  (interactive)
  (let ((brb (current-buffer)) brn)
; mupad-run-save-except 'reset 'end ou nil (lors des sauvegardes 'save)
; 'kill pour ne rien faire 
    (cond 
      ((not mupad-run-save-except)
        (setq brn (mupad-run-save-br))
        (unwind-protect (save-buffer) (kill-buffer brn))
        (kill-buffer brn)
        (set-buffer brb))
      ((eq mupad-run-save-except 'reset)
         (setq brn (mupad-run-save-br))
         (set-buffer brb)
         (setq mupad-run-last-session 
           (prog2 (set-buffer brn) (buffer-string) (set-buffer brb)))
         (kill-buffer brn))
      ((eq  mupad-run-save-except 'end)
         (when 
           (not 
             (yes-or-no-p 
               "Mupad-run buffer not saved.  Quit without save ? "))
          (setq brn (mupad-run-save-br))
          (unwind-protect (save-buffer) 
            (progn (kill-buffer brn) (set-buffer brb)))
          (kill-buffer brn))
          (set-buffer brb)))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-before-kill () 
  (when (eq major-mode 'mupad-run-mode) 
    (switch-to-buffer (current-buffer))
    (when (not mupad-run-save-except)
      (setq mupad-run-save-except 'end))
    (mupad-run-save)
    (let ((inhibit-read-only t))
      (when (processp mupad-run-process) (delete-process mupad-run-process))
      (setq mupad-run-process nil)
      (set-text-properties (point-min) (point-max) nil)))
  t)
;; 
;; 
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;
;; 3/ Les sorties de mupad (y compris complétion)
;;
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; affichage des données envoyées par MuPAD
;; 
;; mupad-run-filter est la fonction principale.
;;   elle traite les données reçues au fur et à mesure (mupad-run-output),
;;   gère la possibilité d'écriture dans le tampon *MuPAD* (mupad-run-state),
;;   gère la position du curseur de mupad (mupad-run-todo)
;;   var locales : save-buffer, output-type, output-index, output-str
;;
;; mupad renvoie ^F n données ^G avec les valeurs de n : 
;; 2 : résultat de MuPAD                  9 : erreur
;; 3 : appel système,                    32 : énumération des complétions
;; 6 : largeur de l'affichage (en ascii) 33 : fin de l'énumération 
;; 7 : type d'affichage (prettyprint)   255 : perte de synchro. (par emacs)
;; 8 : aide en ligne 
;;
(defun mupad-run-filter (proc str) 
  (let 
    ((output-index 0) output-type output-str brt (inhibit-read-only t)
     (brc (current-buffer)) (brb (process-buffer proc)) brp)
     (set-buffer brb)
     (setq mupad-run-output (concat mupad-run-output str))
; tant-qu'il y a des données complètes à traiter, le faire
    (while (setq output-type (mupad-run-output-complete-data output-index))
      (setq output-str     
        (substring 
          mupad-run-output (+ 2 (car output-type)) (cadr output-type)))
      (setq brt (car (cddr output-type)))
      (mupad-run-message-debug 'mupad-run-filter 
        (concat "MuPAD output: [" (number-to-string brt) "] " output-str))
; traiter les différents types de données renvoyées par mupad
      (cond
        ((or (eq brt 2)   ; output
	     (eq brt 61)) ; NT: debugger output MPRCmdb_output
          (mupad-run-print output-str 
            'mupad-run-face-result 
            (marker-position mupad-run-todo) brt nil))
        ((eq brt 13) ; prompt 
           (setq mupad-run-prompt output-str)
           (set-marker mupad-run-last-prompt (1- mupad-run-todo))
           (mupad-run-print (concat output-str "\n")
             'mupad-run-face-prompt 
             (marker-position mupad-run-todo) brt nil)
           (set-marker mupad-run-last-prompt (1+ mupad-run-last-prompt))
           (setq mupad-run-state 'wait-input))
        ((or (eq brt 9)   ; error message
	     (eq brt 63)) ; NT: debugger error message
          (put-text-property mupad-run-last-prompt (1+ mupad-run-last-prompt) 
              'to-insert "///--- Erreur dans ce bloc\n")
          (mupad-run-print output-str 
              'mupad-run-face-error 
              (marker-position mupad-run-todo) brt nil)
          (put-text-property (1- mupad-run-todo) mupad-run-todo  
              'to-insert "///--- Fin du bloc avec une erreur\n"))
        ((eq brt 3) (mupad-run-call-system output-str)) ; system-call
        ((eq brt 6)) ; change to TEXTWIDTH
        ((eq brt 7)) ; change to PRETTYPRINT
        ((eq brt 8)  ; online documentation
          (condition-case err 
           (apply mupad-help-method (list output-str))
           (error (message "%s" (error-message-string err))))
          (set-buffer brb))
        ((eq brt 32) (mupad-run-output-completion output-str))
        ((eq brt 33) (mupad-run-output-end-comp output-str))
; Début des modifications NT 04/11/2002 
        ((or (eq brt 34)            ; MPRCmdb_file_pos
;	     (eq brt 41)
	     (eq brt 66))           ; MPRCmdb_where
; debugger -> frontend: display file at position line no.
	 (string-match "^\\(\\S-+\\)\n\\([0-9]+\\)" output-str)
	 (let
	     ((file (match-string 1 output-str))
	      (line (string-to-number (match-string 2 output-str))))
	   (setq gud-comint-buffer (current-buffer))
	   (gud-display-line file line)
	   (setq mupad-run-debugger-file file)
	   (setq mupad-run-debugger-line line)
	   ))
        ((eq brt 62)
	 ; kernel has stopped and waits for the next debugger command
	 (setq mupad-run-prompt output-str)
	 (set-marker mupad-run-last-prompt (1- mupad-run-todo))
	 (mupad-run-print (concat output-str "\n")
	  'mupad-run-face-prompt (marker-position mupad-run-todo) brt nil)
	 (set-marker mupad-run-last-prompt (1+ mupad-run-last-prompt))
	 (setq mupad-run-state 'wait-debugger-input))
; We ignore all begin and end tags, and a few others
	((memq brt '(36	             ; MPRCmdb_disp_list_begin
		     37		     ; MPRCmdb_disp_list_end
		     42		     ; MPRCmdb_proc_list_begin
		     43		     ; MPRCmdb_proc_list_end
	             47 	     ; MPRCmdb_bkpt_list
	             48 	     ; MPRCmdb_bkpt_list_begin
	             49  	     ; MPRCmdb_bkpt_list_end
	             57 	     ; MPRCmdb_quit: Quit debugger
	             64 	     ; MPRCmdb_start: Initialize debugger
	     )))
; Fin des modification NT 04/11/2002
        (t  
          (mupad-run-print 
            (concat "\n [" (number-to-string (car (cddr output-type))) 
                    "] : " output-str "\n")
            'mupad-run-face-for-emacs (marker-position mupad-run-todo) 
            brt nil)))
    (setq output-index (1+ (cadr output-type))))
  (when 
    (and 
      (/= (marker-position mupad-run-edit) (marker-position mupad-run-todo))
      (memq mupad-run-state '(wait-input wait-debugger-input)))
    (mupad-run-from-todo-to-output))
; raccourcissement de la chaîne à traiter à la fin de la boucle 
  (setq mupad-run-output (substring mupad-run-output output-index))
  (when (equal brb brc) (recenter -4))
  (set-buffer brc)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; mupad-run-output-complete-data 
;;   renvoie  
;;   (deb, fin, type) s'il y a une série de données complète à afficher 
;;     entre les indices deb (compris) et fin (compris)
;;     l'index suivant commence à fin+1
;;   nil sinon
;;   type est la valeur renvoyée par MuPAD
;;      ou 255 si une erreur est détectée dans le flux des données.
;; 
(defun mupad-run-output-complete-data (ind)
  (let (br1 br2 (br3 t) res)
    (cond 
      ((<= (length mupad-run-output) ind) nil)
      ((and 
        (setq br1 (string-match "\006" mupad-run-output ind))
        (> (length mupad-run-output) (+ br1 2)))
        (setq br2 (+ br1 2))
        (while br3
          (cond ; 7=^G - 8=^H 
            ((>= br2 (length mupad-run-output)) (setq br3 nil))
            ((= (aref mupad-run-output br2) 7) 
              (setq res (list br1 br2 (aref mupad-run-output (1+ br1))))
              (setq br3 nil))
            ((= (aref mupad-run-output br2) 8)
              (setq br2 (+ br2 2)))
            (t (setq br2 (1+ br2)))))
        res)
      (t nil)))) 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-print (str aspect pos type ajout)
    (save-excursion 
      (unless (eq type mupad-run-last-type)
        (setq mupad-run-itemb (1+ mupad-run-itemb))
        (setq mupad-run-last-type type))
      (goto-char pos) 
      (insert-before-markers str)
      (put-text-property pos (point) 'item mupad-run-itemb)
      (put-text-property pos (point) 'mupad-run-char-prop aspect)
      (mupad-run-put-face pos (point) aspect)
      (put-text-property pos (point) 'rear-nonsticky t)
      (put-text-property pos (point) 'front-sticky t)
      (put-text-property pos (point) 'read-only t)
      (add-text-properties pos (point) ajout)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-put-face (pt1 pt2 aspect)
  (cond
    (mupad-run-buffer-face 
      (put-text-property pt1 pt2 'face 
        (mupad-run-to-local-symb aspect mupad-run-buffer-face)))
    (t (put-text-property pt1 pt2 'face aspect))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-break ()
  (interactive)
  (process-send-string mupad-run-process "\006\003\n\007")
  (process-send-string mupad-run-process "\006\003\n\007")
  (process-send-string mupad-run-process "\006\003\n\007")
  (process-send-string mupad-run-process "\006\002\n\007")
  (process-send-string mupad-run-process "\006\002\n\007"))
;  (process-send-string mupad-run-process "\006\001\n\007"))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-tab () 
  "Search last word completion"
  (interactive)
  (when (not (eq mupad-run-state 'wait-input))
    (error "MuPAD computes, completion impossible."))
  ;; NT: TODO: modify to allow for completion in debugger commands
  ;; see also mupad-run-output-end-comp for this
  (when (< (point) mupad-run-edit)
    (error "Completion only in the edit zone."))
  (set-marker mupad-run-comp-edit (point))
  (let 
    ((br (posix-search-backward "[^a-zA-Z0-9_:\\.]" (1- mupad-run-edit) t)))
    (when br
      (process-send-string 
        mupad-run-process 
        (concat 
          "\006\037"   
          (setq mupad-run-comp-begin 
            (buffer-substring (1+ br) mupad-run-comp-edit))
          "\007\n"))
        (goto-char mupad-run-comp-edit)
        (setq mupad-run-state 'running))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-output-completion (str) 
  (let (br br1 br2 br3 brs)
    (cond 
      ((string= "" str) 
        (message "Complete identifier"))
      ((string= "\010\007" str) 
        (message 
          (concat 
            "Sorry, no completion available for `" 
            mupad-run-comp-begin "' !")))
       (t 
         (setq br1 0) ; sur str
         (setq br2 0) ; sur sortie
         (setq br3 0) ; sur sortie
         (while (< br1 (length str))
           (cond 
             ((/= (aref str br1) ?,) (setq br1 (1+ br1)))
             (t 
               (setq brs (substring str br3 br1))
               (setq brs 
                 (concat brs (make-string (- 15 (mod (length brs) 15)) 32)))
               (cond 
                 ((<= (+ (length brs) br2) 72)
                   (setq br2 (+ br2 (length brs))))
                 (t (setq br2 (length brs)) (setq brs (concat "\n" brs))))
               (setq br (cons brs br))
               (setq br3 (+ 2 br1)) 
               (setq br1 br3))))
         (setq br 
           (apply 'concat (reverse (cons "\n" (cons (substring str br3) br)))))
         (setq br1 (marker-position mupad-run-last-prompt))
         (mupad-run-print br 
           'mupad-run-face-completion 
           (marker-position mupad-run-last-prompt) 'comp nil)
         (mupad-run-move-flag-up (marker-position mupad-run-last-prompt))
         (setq mupad-run-itemb (1+ mupad-run-itemb))
         (put-text-property br1 mupad-run-last-prompt 'item mupad-run-itemb)
         (setq mupad-run-last-type 'end-comp)))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-output-end-comp (str)  
  (setq mupad-run-state 'wait-input)
  ; NT: TODO: modify to allow for completion in debugger commands
  (if (= (point) (marker-position mupad-run-comp-edit))
    (insert str)
    (save-excursion (goto-char mupad-run-comp-edit) (insert str))))
;; 
;; 
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;
;;  4/ Construction de la commande suivante de mupad et des séparateurs
;;
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;
;; Après une validation, les commandes tapées sont stockées dans la zone 
;; todo et associée à un attribut différent. Elles sont éventuellement
;; complètées par : ou ; 
;; À chaque fois que mupad termine un calcul il prend la commande suivante
;; dans cette zone (ou le commentaire séparateur) 
;; 
;; 
;; s'arrête au premier ^///--- rencontré ou à la fin
;; 
;; état 0 [général]
;;   (NL) -> 1 [début de ligne]     \ -> 2 [échapement]     " -> 10 [chaîne]
;;      / -> 3 [division]           * -> 5 [multiplié]  sinon -> 0 [général]
;;
;; état 10 [chaine]
;;      \ -> 11 [échap]            " -> 0 [général]    sinon -> 10
;;  
;; état 11 [echap chaine]                               
;;      n -> 10                    r -> 10             t -> 10
;;      \ -> 10                    " -> 10         sinon -> 60 -> 10 (err.)
;;
;; état 2 [échap] 
;;   (NL) -> 0 [suite de ligne]                    sinon -> 50 -> 0 (err.)
;;
;; état 1 [début de ligne] 
;;    " -> 10 [chaîne]             \ -> 2 [échapement]  
;; (NL) -> 1 [début de ligne]      / -> 4 [div. déb. ligne]  
;;    * -> 5 [multiplié]       sinon -> 0 [général]
;;
;; état 3 [division]
;;   (NL) -> 1 [début de ligne]    \ -> 2 [échapement]  
;;      " -> 10 [chaîne]           / -> 20 [commentaire]     
;;      * -> 80 -> 0           sinon ->  0 [général]
;;
;; état 5 [multiplié]
;;   (NL) -> 1 [début de ligne]    / -> 70 -> 0 (niveau -1)
;;  sinon -> 0
;;
;; état 20 [commentaire]
;;   (NL) ->  1 [début ligne]  sinon -> 20 [commentaire]
;;
;; état 4 [division début ligne]
;;   (NL) ->  1 [début de ligne]   \ ->  2 [échapement]
;;      " -> 10 [chaîne]           / -> 55 [commentaire début ligne]
;;      * -> 80 -> 0 [commentaire imbriqué]             sinon ->  0 [général]
;;
;; état 55 [commentaire début ligne] 
;;   (NL) ->  1 [début de ligne]   / -> 22 [presque separateur /// et niveau 0]
;;  sinon -> 20 [général]
;;
;; état 22 [presque separateur a]
;;   (NL) ->  1 [début de ligne]   - -> 23 [presque separateur b]
;;  sinon -> 20 [commentaire]
;;
;; état 23 [presque separateur b]
;;   (NL) ->  1 [début de ligne]   - -> 24 [presque separateur c]
;;  sinon -> 20 [commentaire]
;;
;; état 24 [presque separateur c]
;;   (NL) ->  1 [début de ligne]    - -> 25 [séparateur] 
;;  sinon -> 20 [commentaire]
;;
;; recherche la première commande entre pos et max
;; 
(defun mupad-run-automate (pos max)
  (let ((etat 1) (niveau 0) (ordre 0) (test (< pos max)) (err 0) br br1)
    (while test
      (setq br (string-to-char (buffer-substring-no-properties pos (1+ pos))))
      (setq br1 (cdr (assoc (cons etat br) mupad-run-automate-exception)))
      (unless br1 
        (setq br1 (cdr (assoc etat mupad-run-automate-general))))
      (setq etat br1) 
      (setq pos (1+ pos))
      (when (> etat 49)
        (cond 
          ((= etat 55) (if (/= niveau 0) (setq etat 20)))
          ((= etat 50) 
            (message "Erreur après \\") 
            (setq err 1)
            (setq etat 0))
          ((= etat 60) 
            (message "Erreur après \\ dans une chaîne") 
            (setq err 1)
            (setq etat 10))
          ((= etat 70) 
            (setq niveau (1- niveau))
            (setq etat 0))
          ((= etat 80) 
            (setq niveau (1+ niveau))
            (setq etat 0))))
      (setq test (and (< pos max) (or (/= etat 25) (/= niveau 0)))))
    (if (not (memq etat '(0 1 20 21 22 23 24 25))) (setq err 1))
    (list etat pos err)))
;;
;; commentaire ///--- : mupad-run-automate renvoie état 25 et le pointeur + 6 
;;                    : sinon zone pos -> pointeur et etat 0/1 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-from-edit-to-todo () 
  (let (br br1 br2 (inhibit-read-only t))
    (mupad-run-message-debug 'edit-to-todo "DEB : edit-to-todo")
    (goto-char (point-max))
    (when 
      (or 
        (= (point-max) (marker-position mupad-run-edit))
        (/= (char-before (point)) ?\n))
      (insert "\n"))
    (mupad-run-store-current-command 
      (buffer-substring mupad-run-edit (1- (point))))
    (while (< (marker-position mupad-run-edit) (point-max))
      (setq br 
        (mupad-run-automate (marker-position mupad-run-edit) (point-max)))
      (cond  
       ((and (= (car br) 25) 
             (= (cadr br) (+ (marker-position mupad-run-edit) 6)))
          (put-text-property (- (cadr br) 6) (cadr br) 'rear-nonsticky t)
          (put-text-property (- (cadr br) 6) (cadr br) 'front-sticky t)
          (put-text-property (- (cadr br) 6) (cadr br) 'read-only t)
          (put-text-property (- (cadr br) 6) (- (cadr br) 5) 
            'mupad-run-char-prop 'mupad-run-face-beginning-rem)
          (mupad-run-put-face (- (cadr br) 6) (- (cadr br) 5) 
            'mupad-run-face-beginning-rem)
          (goto-char (cadr br)) 
          (end-of-line)
          (setq mupad-run-itema (1+ mupad-run-itema))
          (put-text-property (- (cadr br) 6) (1+ (point))
            'item (cons 'rem mupad-run-itema))
          (put-text-property (point) (1+ (point)) 'rear-nonsticky t)
          (put-text-property (point) (1+ (point)) 'front-sticky nil)
          (put-text-property (point) (1+ (point)) 'read-only t)
          (set-marker mupad-run-edit (1+ (point))))
       (t   
          (if (= (car br) 25) (setq br1 (- (cadr br) 6)) (setq br1 (cadr br)))
          (when (<= (1+ (marker-position mupad-run-edit)) br1)
            (put-text-property mupad-run-edit (1+ mupad-run-edit) 
              'mupad-run-char-prop 'mupad-run-face-beginning-waiting)
            (mupad-run-put-face mupad-run-edit (1+ mupad-run-edit) 
              'mupad-run-face-beginning-waiting))
         (when (<= (+ 2 (marker-position mupad-run-edit)) br1)
           (put-text-property (1+ mupad-run-edit) br1
             'mupad-run-char-prop 'mupad-run-face-waiting-commands)
           (mupad-run-put-face (1+ mupad-run-edit) br1
             'mupad-run-face-waiting-commands))
         (put-text-property mupad-run-edit br1 'rear-nonsticky t)
         (put-text-property mupad-run-edit br1 'front-sticky t)
         (put-text-property mupad-run-edit br1 'read-only t)
         (setq mupad-run-itema (1+ mupad-run-itema))
         (put-text-property mupad-run-edit br1 
           'item (cons 'cmd mupad-run-itema))
         (set-marker mupad-run-edit br1)))
      (goto-char (point-max))
      (mupad-run-message-debug 'edit-to-todo  "FIN : edit-to-todo"))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-from-todo-to-output ()
  (save-excursion
    (let (br br1 br2 br3 (brp (point)) (inhibit-read-only t))
      (mupad-run-message-debug 'todo-to-output "DEB : todo-to-output")
      (setq br (get-text-property mupad-run-todo 'item))
      (when (eq (car br) 'rem) ; commentaire : en premier effacer le prompt
        (goto-char mupad-run-todo)
        (delete-region (- (point) (length mupad-run-prompt) 1) (point))
        (while (eq (car br) 'rem) ; tant que c'est un commentaire
          (setq br1 (point))
          (end-of-line)
          (setq br2 (1+ (point)))
          (setq mupad-run-itemb (1+ mupad-run-itemb))
          (put-text-property br1 br2 'item mupad-run-itemb)
          (put-text-property br1 br2 
            'mupad-run-char-prop 'mupad-run-face-separator)
          (mupad-run-put-face br1 br2 'mupad-run-face-separator)
          (forward-char 1)
          (set-marker mupad-run-todo (point))
          (if 
            (/= (marker-position mupad-run-todo) 
                (marker-position mupad-run-edit))
            (setq br (get-text-property mupad-run-todo 'item))
            (setq br nil)))
; la première commande de la zone todo
        (mupad-run-print (concat mupad-run-prompt "\n") 
          'mupad-run-face-prompt (point) 'cmd nil)
        (set-marker mupad-run-last-prompt 
          (- (point) (length mupad-run-prompt) 1)))
; mettre à jour l'affichage si envoi d'une commande 
      (when (and br (eq (car br) 'cmd))
        (setq br1 
          (or (next-single-property-change mupad-run-todo 'item) (point-max)))
        (mupad-run-message-debug 'todo-to-output "MIL : todo-to-output")
; NT 04/11/2002: modifications for the debugger 
; en fonction de mupad-run-state
	 (cond
	  ((eq mupad-run-state 'wait-input)	     ; Normal input
; éventuellement ajouter un " ;" à la fin
            (setq br2 (1- br1))
            (while (memq (char-before br2) '(?\n ?\t ?\ )) (setq br2 (1- br2)))
            (if (not mupad-run-edit) (message "nil"))
            (when 
               (and (> br2 (marker-position mupad-run-todo))
                    (not (memq (char-before br2) '(?\: ?\;))))
               (goto-char br2) 
               (insert " ;")
               (setq br1 (+ br1 2)))
            (setq br2 (buffer-substring mupad-run-todo (1- br1)))
            (setq mupad-run-rawcommand (list 1 br2)))
          ((eq mupad-run-state 'wait-debugger-input) ; Debugger input
            (setq br2 (buffer-substring mupad-run-todo (1- br1)))
            (setq mupad-run-rawcommand 
              (mupad-run-from-todo-to-output-debug br2))))
	(when mupad-run-rawcommand
	  (mupad-run-message-debug 'todo-to-output
	     (concat "MuPAD input: ["
               (number-to-string (car mupad-run-rawcommand)) "] "
	       (nth 1 mupad-run-rawcommand)))
	  (process-send-string
	   mupad-run-process
	   (concat "\006"
		   (string (car mupad-run-rawcommand))
		   (nth 1 mupad-run-rawcommand)
		   "\007\n"))
          (setq br mupad-run-state)
	  (setq mupad-run-state 'running))
        (delete-region mupad-run-todo br1)
        (setq br3 (1- (marker-position mupad-run-todo)))
        (setq brp (- br3 (length mupad-run-prompt)))
        (mupad-run-print br2
          'mupad-run-face-last-input (1- mupad-run-todo) 'cmd 
          (and (eq br 'wait-debugger-input) '(not-save debug-command)))
        (goto-char br3)
        (forward-line)
        (while (< (point) mupad-run-todo)
          (setq br3 (point))
          (insert mupad-run-prompt)
          (put-text-property br3 (point) 
            'mupad-run-char-prop 'mupad-run-face-local-prompt)
          (mupad-run-put-face br3 (point) 'mupad-run-face-local-prompt)
          (put-text-property br3 (point) 'rear-nonsticky t)
          (put-text-property br3 (point) 'front-sticky t)
          (put-text-property br3 (point) 'read-only t)
          (forward-line)
          (beginning-of-line))
        (put-text-property brp (point) 'item mupad-run-itemb)
        (setq mupad-run-last-type 'end-cmd))))
        (mupad-run-message-debug 'todo-to-output "MIL : todo-to-output"))

(defun mupad-run-from-todo-to-output-debug (br2)
  (cond 
    ((string= br2 "") mupad-run-rawcommand) ; Reuse previous command
    ((string-match 
       "^\\s-*\\([a-zA-Z?]\\)\\(\\s-+\\(.*[^;:       ]\\)\\)?\\s-*[;:]?$" br2)
      (let ((command (match-string 1 br2)) (args (or (match-string 3 br2) "")))
; assertion: there is no space or semicolon at the end of args
        (cond
; MPRCmdb_disp_list         35 // f -> k : request display list
; MPRCmdb_disp_list_begin   36 // begin tag
; MPRCmdb_disp_list_end     37 // end tag
; set/unset display variables:
          ((string= command "D") (list 38 args)) ; MPRCmdb_disp_set
	  ((string= command "U") (list 39 args)) ; MPRCmdb_disp_clear
; A: non standard shortcut in the mupad text debugger
          ((string= command "A") (list 40))      ; MPRCmdb_disp_clearall
;         ((string= command "l") (list 41))      ; MPRCmdb_proc_list
; MPRCmdb_proc_list_begin 42
; MPRCmdb_proc_list_end   43
         ((string= command "u") (list 44))      ; MPRCmdb_proc_up
         ((string= command "d") (list 45))      ; MPRCmdb_proc_down
; MPRCmdb_proc_goto 46 // obsolete?  use MPRCmdb_proc_level
         ((string= command "b") (list 47))      ; MPRCmdb_bkpt_list
; MPRCmdb_bkpt_list_begin 48
; MPRCmdb_bkpt_list_end   49
; S file line [cond] : Set a breakpoint
; S [cond]	    : Set a breakpoint at the current position
; This second form is non standard in the mupad text debugger
; Question: should we send TRUE when there is no condition?
         ((string= command "S")                 ; MPRCmdb_bkpt_set 50
         (if (string-match
       "^\\(\\(\\S-+\\)\\s-+\\([0-9]+\\)\\)\\(\\s-+\\(.+\\)\\)?\\|\\(.+\\)?$"
          args)
         (let 
           ((file  (or (match-string 2 args) mupad-run-debugger-file))
            (line  (or (match-string 3 args)
	    (number-to-string mupad-run-debugger-line)))
            (condi (or (match-string 5 args) (match-string 6 args) "TRUE")))
         (list 50 (concat "\n" file "\n" line "\n" condi)))
         (message "Usage: S filename line [condition] or S [condition]")
         nil))
; C file line : Clears a breakpoint
; S           : Clears a breakpoint at the current position
; This second form is non standard in the mupad text debugger
       ((string= command "C")		      ; MPRCmdb_clear
         (message args)
	 (if (string-match "^\\(\\(\\S-+\\)\\s-+\\([0-9]+\\)\\)?$" args)
         (let ((file  (or (match-string 2 args) mupad-run-debugger-file))
               (line  (or (match-string 3 args)
               (number-to-string mupad-run-debugger-line))))
           (list 51 (concat file " " line)))
	   (message "Usage: C filename line or C")
           nil))
	((string= command "a") (list 52))      ; MPRCmdb_clearall
	((string= command "n") (list 53))      ; MPRCmdb_next
	((string= command "s") (list 54))      ; MPRCmdb_step
	((string= command "c") (list 55))      ; MPRCmdb_cont
	((string= command "e") (list 56 args)) ; MPRCmdb_execute
	((string= command "q") (list 57))      ; MPRCmdb_quit
	((string= command "p") (list 58 args)) ; MPRCmdb_print
	((or (string= command "h") (string= command "?"))   ;MPRCmdb_help
          (list 59))
; MPRCmdb_status 60 ???
        ((string= command "f") (list 65))      ; MPRCmdb_finish
	((string= command "w") (list 66))      ; MPRCmdb_where
	((string= command "P") (list 67 args)) ; MPRCmdb_pprint
; MPRCmdb_stop_at 68 (deprecated?)
	((string= command "g") (list 69 args)) ; MPRCmdb_goto_proc
; non standard shortcut in the mupad text debugger:
	((string= command "L") (list 70 args)) ; MPRCmdb_proc_level
	(t (message 
            (concat "Undefined debugger command: \"" command "\". Try \"h\"."))
	nil))))
   (t (message (concat "Incorrect debugger command: \"" br2 "\". Try \"h\"."))
     nil)))
;;
;; 
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;
;;  5/ Commandes [RET], [C-RET], [SUP] et [BS] (commentaires séparateurs)
;;
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;
; [RET] et [C-RET] : 
;   zone édition                     -> to-todo ou passage à la ligne
;   zone todo                        -> 
;     si dans commentaire : couper le commentaire à cet endroit
;     si dans commandes   : aller à la fin ou au début, et inserer commentaire
;   zone résultats                   -> 
;     si dans commentaire : couper le commentaire à cet endroit
;     si prompt ou commande          -> recopier la commande à la fin
;     sinon insérer à la fin de la commande après le résultat
;
; mupad-run-return-position renvoie modifie le tampon en conséquence.
;
; [C-SUP] dans zone résultat sur résultat, et complétion 
;                 -> cache le résultat ou la complétion
; [C-INS] réinsere la partie cachée.
;                
(defun mupad-run-creturn ()
  (interactive)
  (cond 
    ((>= (point) (marker-position mupad-run-edit)) (insert "\n"))
    ((or 
        (memq (get-text-property (point) 'mupad-run-char-prop)
          '(mupad-run-face-local-prompt mupad-run-face-prompt-flag
            mupad-run-face-last-input   
            mupad-run-face-result-last-input))
        (and 
          (eq (get-text-property (point) 'mupad-run-char-prop) 
              'mupad-run-face-prompt)
          (eq (char-after) ?\n)))
      (mupad-run-copy-cmd))
    (t (mupad-run-insert-comment))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-return ()
  (interactive)
  (cond 
    ((>= (point) (marker-position mupad-run-edit))
      (mupad-run-from-edit-to-todo)
; NT 04/11/2002
      (if (memq mupad-run-state '(wait-input wait-debugger-input)) 
        (mupad-run-from-todo-to-output)))
    ((or 
      (memq (get-text-property (point) 'mupad-run-char-prop)
        '(mupad-run-face-local-prompt 
          mupad-run-face-local-prompt-flag
          mupad-run-face-last-input   
          mupad-run-face-result-last-input))
        (and 
          (eq (get-text-property (point) 'mupad-run-char-prop) 
              'mupad-run-face-prompt)
          (eq (char-after) ?\n)))
      (mupad-run-copy-cmd))
    (t (mupad-run-insert-comment))))

;
; [SUP]                                      
;   dans zone éditable                          -> zone éditable, effacer
;   dans ///---( )*(RET)                        -> supprimer
;   dans ///--- et ligne précédente commentaire -> recoller
;   sur (RET) et ligne suivante commentaire     -> recoller
;
(defun mupad-run-suppression () 
  ""
  (interactive)
  (let ((br (point)) (inhibit-read-only t))
    (cond 
; caractère modifiable 
      ((not (get-char-property (point) 'read-only)) 
        (delete-char 1) (setq br nil))
; dans la zone ///--- d'un commentaire : si commentaire avant -> recoller
     ((and
         (memq (get-char-property (point) 'mupad-run-char-prop)
            '(mupad-run-face-separator-flag mupad-run-face-separator))
         (not (beginning-of-line))
         (<= (- br (point)) 5)
         (> (point) 1)
         (not (backward-char))
         (memq (get-char-property (point) 'mupad-run-char-prop)
            '(mupad-run-face-separator-flag mupad-run-face-separator)))
        (mupad-run-move-flag-down (1+ (point)))
        (delete-region (point) (+ 7 (point)))
        (setq br nil))
; sur (RET) et ligne suivante commentaire     -> recoller
     ((and 
         (goto-char br)
         (eolp)
         (memq (get-char-property (point) 'mupad-run-char-prop)
            '(mupad-run-face-separator-flag 
              mupad-run-face-separator))
         (< (point) (point-max))
         (not (forward-char))
         (memq (get-char-property (point) 'mupad-run-char-prop)
            '(mupad-run-face-separator-flag 
              mupad-run-face-separator)))
       (mupad-run-move-flag-down (point))
       (delete-region (1- (point)) (+ 6 (point)))
       (setq br nil))
; une ligne vide de commentaires
      ((and
           (goto-char br)
           (memq (get-char-property (point) 'mupad-run-char-prop)
             '(mupad-run-face-separator-flag 
               mupad-run-face-separator))
           (not (beginning-of-line))
           (looking-at "///---[ \t]*$"))
        (mupad-run-move-flag-down (point))
        (delete-region (point) (1+ (match-end 0)))
        (setq br nil)))
    (if br (goto-char br))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-backspace () 
  "" 
  (interactive) 
  (when (and (> (point) 1) (/= (point) (marker-position mupad-run-edit)))
    (backward-char) (mupad-run-suppression)))
;;
;; 
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;
;; 7/ Traitement des appels systèmes et affichage graphique
;;
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;
(defun mupad-run-call-system (str)
  (let (br brp brs)
    (when (eq (mod mupad-run-system-trace 2) 1)
        (mupad-run-print (concat str "\n")
          'mupad-run-face-call-system (marker-position mupad-run-todo) 
          'sys nil))
    (setq br 0)  
    (while (and (< br (length str)) (/= (aref str br) ? )) (setq br (1+ br)))
    (setq brs (member (substring str 0 br) mupad-run-system-exception))
    (if (or (and (< mupad-run-system-trace 2) (not brs)) brs)
      (call-process "sh" nil 0 nil "-c" str)
      (setq brp (point))
      (save-excursion 
        (goto-char mupad-run-todo) 
        (setq br (point))
        (call-process "sh" nil t nil "-c" str)
        (put-text-property br (point) 'mupad-run-char-prop 
          'mupad-run-face-system)
        (mupad-run-put-face br (point) 'mupad-run-face-system)
        (put-text-property br (point) 'rear-nonsticky t)
        (put-text-property br (point) 'front-sticky t)
        (put-text-property br (point) 'read-only t)
        (if 
          (= (marker-position mupad-run-todo) 
             (marker-position mupad-run-edit))
          (set-marker mupad-run-edit (point)))
        (setq brp (= (marker-position mupad-run-todo) brp))
        (set-marker mupad-run-todo (point)))
      (if brp (goto-char mupad-run-todo)))))
;;
;; 
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;
;;  8/ Historique des commandes et recherche
;;
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;
;; structure des données doublement chainée avec un point d'accès 
;; intermédiaire, sous forme d'un tableau :
;; coordonnées générale
;;   0 = liste directe - 1 = liste inverse - 2 = particulier - 3 = longueur.
;; L'accès aux éléments de la liste est aussi un tableau dont la valeur des 
;;   coordonnées est 
;; 0 = valeur - 1 = terme suivant - 2 = terme précédent
;;
(defun head-tail-void ()
  (let ((res (make-vector 4 nil)))
    (aset res 3 0)
    res))

(defun add-head (a struct) 
  (let ((br (make-vector 3 nil)) (tete (aref struct 0)))
    (aset br 0 a) 
    (aset br 1 tete)
    (if tete (aset tete 2 br) (aset struct 1 br))
    (aset struct 0 br)
    (aset struct 3 (1+ (aref struct 3)))
    struct))

(defun add-tail (a struct) 
  (let ((br (make-vector 3 nil)) (queue (aref struct 1)))
    (aset br 0 a) 
    (aset br 2 queue)
    (if queue (aset queue 1 br) (aset struct 0 br))
    (aset struct 1 br)
    (aset struct 3 (1+ (aref struct 3)))
    struct))

(defun remove-head (struct) 
  (unless (aref struct 0) (error "structure vide"))
  (when (eq (aref struct 2) (aref struct 0)) (aset struct 2 'head))
  (aset struct 0 (aref (aref struct 0) 1))
  (if (aref struct 0) (aset (aref struct 0) 2 nil) (aset struct 1 nil))
  (aset struct 3 (1- (aref struct 3)))
  struct)

(defun remove-tail (struct) 
  (unless (aref struct 0) (error "structure vide"))
  (when (eq (aref struct 2) (aref struct 1)) (aset struct 2 'tail))
  (aset struct 0 (aref (aref struct 0) 1))
  (if (aref struct 0) (aset (aref struct 0) 2 nil) (aset struct 1 nil))
  (aset struct 3 (1- (aref struct 3)))
  struct)

(defun list-tail (A) 
  (let ((tmp (aref A 0)) res)
    (while tmp (setq res (cons (aref tmp 0) res)) (setq tmp (aref tmp 1)))
    res))

(defun list-head (A) 
  (let ((tmp (aref A 1)) res)
    (while tmp (setq res (cons (aref tmp 0) res)) (setq tmp (aref tmp 2)))
    res))

(defun set-ptr-head (A) 
  (if (aref A 1) (aset A 2 (aref A 0)) (aset A 2 'head)))

(defun set-ptr-tail (A) 
  (if (aref A 1) (aset A 2 (aref A 1)) (aset A 2 'tail)))

(defun ptr-to-tail (A)
  (cond 
    ((not (aref A 2)) (error "pointeur vide"))
    ((eq (aref A 2) 'tail))
    ((eq (aref A 2) 'head) (or (aset A 2 (aref A 0)) (aset A 2 'tail)))
    ((not (aref (aref A 2) 1)) (aset A 2 'tail))
    (t (aset A 2 (aref (aref A 2) 1))))
  A)

(defun ptr-to-head (A)
  (cond 
    ((not (aref A 2)) (error "pointeur vide"))
    ((eq (aref A 2) 'head))
    ((eq (aref A 2) 'tail)  (or (aset A 2 (aref A 1)) (aset A 2 'head)))
    ((not (aref (aref A 2) 2)) (aset A 2 'head))
    (t (aset A 2 (aref (aref A 2) 2))))
  A)
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-get-previous-command (str)
  (let (br brt brs) 
    (setq br (aref mupad-run-hist-commands 2))
    (ptr-to-tail mupad-run-hist-commands)
    (setq brt t)
    (while 
      (and 
         brt 
         (not (symbolp (aref mupad-run-hist-commands 2)))
         (setq brs (aref (aref mupad-run-hist-commands 2) 0))
; Si les débuts de chaines sont égaux alors brt vaut nil
         (setq brt 
           (not 
             (string= str (substring brs 0 (min (length str) (length brs)))))))
      (ptr-to-tail mupad-run-hist-commands))
;    (when brt (aset mupad-run-hist-commands 2 br))
; renvoie nil si le début de la chaîne n'est pas trouvé, la chaine sinon
    (and (not brt) brs)))
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-get-next-command (str)
  (let (br brt brs) 
    (setq br (aref mupad-run-hist-commands 2))
;    (when (eq br 'tail) (ptr-to-head mupad-run-hist-commands))
    (ptr-to-head mupad-run-hist-commands)
    (setq brt t)
    (while 
      (and 
         brt 
         (not (symbolp (aref mupad-run-hist-commands 2)))
         (setq brs (aref (aref mupad-run-hist-commands 2) 0))
         (setq brt 
           (not 
             (string= str (substring brs 0 (min (length str) (length brs)))))))
      (ptr-to-head mupad-run-hist-commands))
;    (when brt (aset mupad-run-hist-commands 2 br))
    (and (not brt) brs)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-store-line (str)
  (when (>= (length str) 3)
    (cond 
      ((and 
        (aref mupad-run-hist-commands 0) 
        (string= (aref (aref mupad-run-hist-commands 0) 0) str)))
      ((and 
        (eq (aref mupad-run-hist-commands 2) 'tail)
        (aref mupad-run-hist-commands 1) 
        (string= (aref (aref mupad-run-hist-commands 1) 0) str)))
      ((and 
        (not (symbolp (aref mupad-run-hist-commands 2)))
        (string= (aref (aref mupad-run-hist-commands 2) 0) str)))
      (t (add-head str mupad-run-hist-commands)))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-store-current-command (str)
  (when (>= (length str) 3)
    (add-head str mupad-run-hist-commands)
    (aset mupad-run-hist-commands 2 'head)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-previous-history-search ()
  (interactive)
  (when (>= (point) (marker-position mupad-run-edit))
    (let 
      ( (br (buffer-substring mupad-run-edit (point-max))) 
        (brs (buffer-substring mupad-run-edit (point))) (brn (point))
        br1 br2 br3)
      (setq br2 (aref mupad-run-hist-commands 2))
      (setq br1 (mupad-run-get-previous-command brs))
      (setq br3 (aref mupad-run-hist-commands 2))
      (aset mupad-run-hist-commands 2 br2)
      (unless (string= brs br) (mupad-run-store-line br))
      (aset mupad-run-hist-commands 2 br3)
      (delete-region mupad-run-edit (point-max))
      (goto-char mupad-run-edit)
      (cond 
        ((not br1) 
          (insert brs)
          (error "end of history list"))
        (t 
          (insert br1) 
          (goto-char brn))))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-previous-history ()
  (interactive)
  (when (>= (point) (marker-position mupad-run-edit))
    (let 
      ( (br (buffer-substring mupad-run-edit (point-max))) br1 br2 br3
        (brn (point)))
      (setq br2 (aref mupad-run-hist-commands 2))
      (setq br1 (mupad-run-get-previous-command ""))
      (setq br3 (aref mupad-run-hist-commands 2))
      (aset mupad-run-hist-commands 2 br2)
      (unless (string= "" br) (mupad-run-store-line br))
      (aset mupad-run-hist-commands 2 br3)
      (delete-region mupad-run-edit (point-max))
      (goto-char mupad-run-edit)
      (cond 
        ((not br1) 
          (error "end of history list"))
        (t 
          (insert br1) 
          (goto-char (min brn (point-max))))))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-next-history-search ()
  (interactive)
  (when (>= (point) (marker-position mupad-run-edit))
    (let 
      ( (br (buffer-substring mupad-run-edit (point-max))) 
        (brs (buffer-substring mupad-run-edit (point))) (brn (point))
        br1 br2 br3)
      (setq br2 (aref mupad-run-hist-commands 2))
      (setq br1 (mupad-run-get-next-command brs))
      (setq br3 (aref mupad-run-hist-commands 2))
      (aset mupad-run-hist-commands 2 br2)
      (unless (string= brs br) (mupad-run-store-line br))
      (aset mupad-run-hist-commands 2 br3)
      (delete-region mupad-run-edit (point-max))
      (goto-char mupad-run-edit)
      (cond 
        ((not br1) 
          (insert brs)
          (error "end of history list"))
        (t 
          (insert br1) 
          (goto-char brn))))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-next-history ()
  (interactive)
  (when (>= (point) (marker-position mupad-run-edit))
    (let 
      ( (br (buffer-substring mupad-run-edit (point-max))) br1 br2 br3 
        (brn (point)))
      (setq br2 (aref mupad-run-hist-commands 2))
      (setq br1 (mupad-run-get-next-command ""))
      (setq br3 (aref mupad-run-hist-commands 2))
      (aset mupad-run-hist-commands 2 br2)
      (unless (string= "" br) (mupad-run-store-line br))
      (aset mupad-run-hist-commands 2 br3)
      (delete-region mupad-run-edit (point-max))
      (goto-char mupad-run-edit)
      (cond 
        ((not br1) 
          (error "end of history list"))
        (t 
          (insert br1) 
          (goto-char (min brn (point-max))))))))
;; 
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;
;;  9/ Commandes de déplacement et d'insertion / supression de résultats
;;
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;
(defun mupad-run-left () 
  (interactive)
  (goto-char (or (previous-single-property-change (point) 'item) (point-min)))
  (beginning-of-line))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-right () 
  (interactive)
  (end-of-line)
  (goto-char (or (next-single-property-change (point) 'item) (point-max))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-hide () 
  (interactive)
  (let (br bra brb (inhibit-read-only t))
    (cond 
      ((and mupad-run-last-prompt (>= (point) mupad-run-last-prompt)) 
        (error "Hiding impossible at this point"))
      ((memq (get-text-property (point) 'mupad-run-char-prop)
        '(mupad-run-face-result mupad-run-face-call-system 
          mupad-run-face-system mupad-run-face-completion
          mupad-run-face-result-flag mupad-run-face-call-system-flag 
          mupad-run-face-system-flag mupad-run-face-completion-flag))
        (setq bra (or (previous-single-property-change (1+(point)) 'item) 1))
        (setq brb 
          (or (next-single-property-change (point) 'item)
              (marker-position mupad-run-last-prompt)))
        (put-text-property brb (1+ brb) 'hide 
          (cons (buffer-substring bra brb) (get-text-property brb 'hide)))
        (setq br (symbol-name (get-text-property brb 'mupad-run-char-prop)))
        (when (not (string= (substring br -5) "-flag"))
          (put-text-property brb (1+ brb) 'mupad-run-char-prop 
            (intern (concat br "-flag")))
          (mupad-run-put-face brb (1+ brb) (intern (concat br "-flag"))))
        (delete-region bra brb)
        (recenter)))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-show () 
  (interactive)
  (let (br1 br2 (br (point)) (inhibit-read-only t))
    (when (not (eobp)) (forward-char 1) (mupad-run-left))
    (setq br1 (get-text-property (point) 'hide))
    (when br1 
      (put-text-property (point) (1+ (point)) 'hide (cdr br1))
      (insert-before-markers (car br1))
      (setq br2 (symbol-name (get-text-property (point) 'mupad-run-char-prop)))
      (when (not (cdr br1))
        (put-text-property (point) (1+ (point))  
          'mupad-run-char-prop 
          (intern (substring br2 0 (- (length br2) 5))))
        (mupad-run-put-face (point) (1+ (point))
          (intern (substring br2 0 (- (length br2) 5)))))
      (recenter))))

(defun mupad-run-move-flag-up (pt)
  (let ( (inhibit-read-only t) 
         (br (symbol-name (get-text-property pt 'mupad-run-char-prop))))
    (save-excursion 
      (when (string= (substring br -5) "-flag")
        (put-text-property pt (1+ pt)
          'mupad-run-char-prop (intern (substring br 0 (- (length br) 5))))
        (goto-char pt)
        (mupad-run-left)
        (put-text-property (point) (1+ (point))
          'hide (get-text-property pt 'hide))
        (put-text-property pt (1+ pt) 'hide ())
        (setq br 
          (symbol-name (get-text-property (point) 'mupad-run-char-prop)))
        (put-text-property (point) (1+ (point)) 
          'mupad-run-char-prop (intern (concat br "-flag")))
        (mupad-run-put-face (point) (1+ (point)) 
          (intern (concat br "-flag")))))))

(defun mupad-run-move-flag-down (pt)
  (let 
    ( br1 (inhibit-read-only t) 
      (br (symbol-name (get-text-property pt 'mupad-run-char-prop))))
    (save-excursion 
      (when (string= (substring br -5) "-flag")
        (put-text-property pt (1+ pt)
          'mupad-run-char-prop (intern (substring br 0 (- (length br) 5))))
        (mupad-run-right)
        (setq br1 (get-text-property (point) 'hide)))
        (put-text-property (point) (1+ (point)) 
          'hide (append (get-text-property pt 'hide) br1))
        (unless br1
           (setq br 
              (symbol-name (get-text-property (point) 'mupad-run-char-prop)))
           (put-text-property (point) (1+ (point)) 
             'mupad-run-char-prop (intern (concat br "-flag")))
           (mupad-run-put-face (point) (1+ (point)) 
             (intern (concat br "-flag")))))))

(defun mupad-run-copy-cmd ()
  (interactive) 
  (set-mark (point))
  (let ((br (point)) bra brb (inhibit-read-only t))
    (mupad-run-left) 
    (setq bra (point))
    (mupad-run-right)
    (setq brb (point))
    (when (= brb br) 
      (setq bra brb) (mupad-run-right) (setq brb (point)))
    (goto-char (point-max))
    (setq br (point-max))
    (while (memq (char-before br) '(?\n ?\t ?\ )) (setq br (1- br)))
    (when 
      (and (> br (marker-position mupad-run-edit))
           (not (memq (char-before br) '(?\: ?\;))))
      (goto-char br)
      (insert " ;"))
    (goto-char (point-max))
    (if (not (bolp)) (insert "\n"))
    (insert (buffer-substring bra brb))
    (goto-char mupad-run-edit)
    (while (not (eobp))
      (setq bra (get-text-property (point) 'mupad-run-char-prop))
      (setq brb (or (next-property-change (point)) (point-max)))
      (cond 
        ((and (eq bra 'mupad-run-face-prompt) (eq (char-after) ?\n))
          (forward-char 1))
        ((memq bra
           '(mupad-run-face-local-prompt      mupad-run-face-prompt
             mupad-run-face-local-prompt-flag mupad-run-face-prompt-flag))
           (delete-region (point) brb))
        ((eq bra 'mupad-run-face-last-input) 
            (goto-char brb))
        (t (goto-char brb))))
   (set-text-properties mupad-run-edit (point-max) nil)))

; selectionner le bloc de commandes et le recopier à la fin de la 
; zone d'édition
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; si zone résultat             : à la fin de la zone résultat 
;; si zone commande             : à la fin du résultat éventuel,
;;                                avant le prompt suivant
;; si zone completion           : à la fin de la complétion
;; si zone prompt               : avant ce prompt
;; si zone commentaire          : avant ce commentaire
;; si dans commentaire          : coupure de commentaire
;; si trop bas                  : en haut de la zone edit
;; si dans la zone todo         : à la commande suivante
;;
(defun mupad-run-insert-comment ()
  (let 
    ((br (point)) (br1 (get-text-property (point) 'mupad-run-char-prop))
     (inhibit-read-only t) brs br2)
    (cond 
; dans la zone edit
      ((>= (point) (marker-position mupad-run-edit))
        (goto-char mupad-run-edit) (insert "///---\n") (backward-char 1))
; dans un prompt (sauf fin de ligne) : aller au début de la ligne
      ((and 
          (memq br1 
            '(mupad-run-face-prompt mupad-run-face-prompt-flag))
          (not (eolp)))
        (beginning-of-line) (mupad-run-insert-comment-br (point) ""))
; au debut d'une zone de completion, d'un prompt ou d'un commentaire
      ((and 
        (not (eq (point) 1))
        (bolp)
        (not 
          (eq 
            (get-text-property (point) 'item)  
            (get-text-property (1- (point)) 'item)))
        (memq (get-text-property (point) 'mupad-run-char-prop)
          '(mupad-run-face-prompt mupad-run-face-prompt-flag 
            mupad-run-face-separator mupad-run-face-separator-flag 
            mupad-run-face-completion 
            mupad-run-face-completion-flag)))
         (mupad-run-insert-comment-br (point) ""))
; à la fin d'une ligne de saisie (face = prompt)
      ((memq br1 '(mupad-run-face-prompt mupad-run-face-prompt-flag))
        (mupad-run-right) (mupad-run-insert-comment))
; dans la zone todo sur le caractère de tête
      ((memq br1 
           '(mupad-run-face-beginning-waiting 
             mupad-run-face-beginning-rem))
        (mupad-run-insert-comment-br (point) ""))
; dans un commentaire, sur l'un des caractères de ///---
      ((and 
         (memq br1 
           '(mupad-run-face-separator mupad-run-face-separator-flag))
           (< (- (point) (progn (beginning-of-line) (point))) 6))
       (forward-char 6) (mupad-run-insert-comment))
; dans un commentaire, au milieu
      ((and 
         (goto-char br)
         (memq br1 
           '(nil mupad-run-face-separator 
             mupad-run-face-separator-flag)))
       (end-of-line)
       (setq br2 (buffer-substring br (point)))
       (delete-region br (point))
       (forward-char 1)
       (mupad-run-insert-comment-br (point) br2))
; dans la zone todo (sauf commentaires traités ci-dessus)
      (( >= (point) (marker-position mupad-run-todo))
        (mupad-run-right) (mupad-run-insert-comment))
; au milieu d'une zone...
      ((and 
          (not (eq (point) 1))
          (eq 
            (get-text-property (point) 'item)  
            (get-text-property (1- (point)) 'item)))
         (mupad-run-right) (mupad-run-insert-comment))
; sinon (dans la zone de mupad) avancer jursqu'à trouver le bon endroit
      (t
        (goto-char br) 
        (while 
          (memq (get-text-property (point) 'mupad-run-char-prop)
            '(mupad-run-face-last-input   
              mupad-run-face-last-input-flag
              mupad-run-face-result mupad-run-face-result-flag
              mupad-run-face-local-prompt 
              mupad-run-face-local-prompt-flag
              mupad-run-face-for-emacs mupad-run-face-for-emacs-flag
              mupad-run-face-call-system  
              mupad-run-face-call-system-flag  
              mupad-run-face-system mupad-run-face-system-flag  
              mupad-run-face-error mupad-run-face-error-flag))
       (mupad-run-right))
       (mupad-run-insert-comment)))))

(defun mupad-run-insert-comment-br (pt str)  
  (goto-char pt)
  (cond 
    ((= pt (marker-position mupad-run-last-prompt))
     (set-marker mupad-run-last-prompt (1+ pt))
     (insert "///---\n")
     (set-marker mupad-run-last-prompt (1- mupad-run-last-prompt)))
    (t (insert "///---\n")))
  (backward-char)
  (put-text-property (- (point) 6) (1+ (point)) 'rear-nonsticky t)
  (put-text-property (- (point) 6) (point) 'front-sticky t)
  (put-text-property (point) (1+ (point)) 'front-sticky nil)
  (put-text-property (- (point) 6) (1+ (point)) 'read-only t)
  (put-text-property (- (point) 6) (1+ (point))
    'mupad-run-char-prop 'mupad-run-face-separator)
  (mupad-run-put-face (- (point) 6) (1+ (point)) 'mupad-run-face-separator)
  (cond 
    ((< (point) mupad-run-todo)
      (setq mupad-run-itemb (1+ mupad-run-itemb))
      (put-text-property (- (point) 6) (1+ (point)) 'item mupad-run-itemb))
    ((< (point) mupad-run-edit)
      (setq mupad-run-itema (1+ mupad-run-itema))
      (put-text-property (- (point) 6) (1+ (point))
        'item (cons 'rem mupad-run-itema))
      (put-text-property (- (point) 6) (- (point) 5)
        'mupad-run-char-prop 'mupad-run-face-beginning-rem)
      (mupad-run-put-face (- (point) 6) (- (point) 5)
        'mupad-run-face-beginning-rem)))
  (save-excursion 
    (insert str) (mupad-run-right) (mupad-run-move-flag-up (point))))
;; 
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;
;; 11/ Gestion du couper/coller
;;
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;
(defun mupad-run-yank (&optional arg)
  (interactive "*P")
  ""
  (let (br br1 br2 br3) 
; curseur dans la zone d'édition 
    (cond ((>= (point) (marker-position mupad-run-edit))
        (yank arg)
        (setq br1 (min (point) (mark)))
        (setq br br1)
        (setq br2 (max (point) (mark)))
        (save-excursion 
          (goto-char br1)
          (put-text-property br1 br2 'read-only nil)
          (while (< br1 br2) 
            (cond
              ((and
                  (memq 
                    (get-text-property br1 'mupad-run-char-prop)
                    '(mupad-run-face-prompt mupad-run-face-local-prompt))
                  (not (eq (char-after) ?\n)))
                (delete-char 1)
                (setq br2 (1- br2)))
              (t (setq br1 (1+ br1)) (forward-char)))))
          (set-text-properties br br2 nil))
; curseur dans une zone de commentaire
      ((and 
          (not (get-text-property (point) 'mupad-run-char-prop))
          (or (not (get-text-property (point) 'read-only)) (eolp)))
        (yank arg)
        (setq br1 (min (point) (mark)))
        (setq br br1)
        (setq br2 (max (point) (mark)))
        (save-excursion 
          (goto-char br1)
          (set-text-properties br br2 nil)
          (end-of-line)
          (put-text-property (point) (1+ (point)) 'rear-nonsticky t)
          (put-text-property (point) (1+ (point)) 'front-sticky nil)
          (put-text-property (point) (1+ (point)) 'read-only t)
          (forward-char)
          (while (<= (point) br2) 
            (insert "///---")
            (setq br2 (+ br2 6))
            (put-text-property (- (point) 6) (point) 'rear-nonsticky t)
            (put-text-property (- (point) 6) (point) 'front-sticky t)
            (setq mupad-run-itema (1+ mupad-run-itema))
            (put-text-property (- (point) 6) (point) 
              'item (cons 'rem mupad-run-itema))
            (put-text-property (- (point) 6) (point) 'read-only t)
            (end-of-line)
            (put-text-property (point) (1+ (point)) 'rear-nonsticky t)
            (put-text-property (point) (1+ (point)) 'front-sticky nil)
            (put-text-property (point) (1+ (point)) 'read-only t)
            (forward-char)))))))
;;
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;
(defun mupad-run-show-mupad-info nil
  "Show mupad-run.el-info on another window."
  (interactive)
  (condition-case err
    (let 
        ((wind (selected-window)) (where-it-is "") (inhibit-read-only t)
        (to-be-tested 
          (list 
            "/usr/local/lib/MuPAD/emacs/" "/usr/local/share/lib/MuPAD/emacs/" 
            "/usr/local/share/emacs/site-lisp/" "/usr/share/lib/MuPAD/"
            "/usr/share/lib/MuPAD/emacs/" "/usr/local/lib/MuPAD/"       
            "/usr/local/share/lib/MuPAD/" "/usr/share/emacs/site-lisp/")))
;; Locate mupad.el-info:
      (mapcar 
        (lambda (afile) (if (file-exists-p afile) (setq where-it-is afile)))
        (mapcar 
          (lambda (apath) 
            (expand-file-name (concat apath "/mupad-run.el-info")))
          (append to-be-tested load-path)))
      (if (and mupad-run-info (file-exists-p mupad-run-info))
        (setq where-it-is mupad-run-info))
      (cond 
        ((not (string-equal where-it-is ""))
;; We switch to the buffer *MuPAD Help* and erase its content:
          (set-buffer (get-buffer-create "*MuPAD Help*"))
          (erase-buffer)
          (message where-it-is)  ;; tell *Messages* which version is used.
          (insert-file where-it-is)
;; Show the help buffer and tell user how to remove help window:
          (mupad-bus-window-manager "*MuPAD Help*" 'mupad-show-help)
          (setq buffer-read-only t)
          (mupad-info-wind-conf)
          (select-window wind))
;; Tell the user the file was not found:
        (t 
          (mupad-bus-window-manager "*MuPAD Help*" 'mupad-beginning-temp)
          (insert 
             (concat 
               "The file mupad-run.el-info was not found."
               "You should discover where it is, say in the directory" 
               "/usr/local/share/emacs/site-lisp and add the line\n"
               "(setq load-path "
               "(concat load-path \"/usr/local/share/emacs/sitelisp\"))\n"
            "to your .emacs file (create it if it doesn't already exist)."))
          (setq fill-column 
            (1- (window-width (get-buffer-window "*MuPAD Help*"))))
          (fill-individual-paragraphs (point-min) (point-max) 'left)
;; Remove help window :
          (setq buffer-read-only t)
          (mupad-bus-window-manager 
            "*MuPAD Help*" 'mupad-remove-help-old-config)
          (mupad-restore-wind-conf))))
    (error (princ "An error occured in mupad-info: ") (princ err) nil)))

(defun mupad-run-customize-group nil
  (interactive)
  (customize-group "mupad-run"))

(defmacro mupad-run-menu-bar ()
  "Menu-bar item MuPAD"
  (` (append
    (list
      "MuPAD"
      ["break" mupad-run-break :active (processp mupad-run-process)]
      ["save"   mupad-run-save :active t]
      ["quit"   mupad-run-end :active (processp mupad-run-process)]
      ["reset"  mupad-run-reset :active t]
      ["other mupad buffer" mupad-bus :active nil]
      (list
        "Send file to MuPAD..."
        ["Silently"  mupad-bus-file t :active (featurep 'mupad)
         :help "Send a file to the mupad-process by `read(...):'"]
        ["Openly"    mupad-bus-execute-file t :active (featurep 'mupad)
         :help "Send a file to the mupad-process by `read(...);'"])
      "---------------------"
      ["Manual" mupad-start-manual :active t :key-sequence nil
        :help "Open the hytex manual"]
      ["Info on this mode" mupad-run-show-mupad-info :active t 
        :key-sequence nil]
      "---------------------"
      ["help around cursor" mupad-help-emacs-search :active t]
      ["help on ..."        mupad-help-emacs-ask :active t 
        :help "Text help on a mupad object"]
      "---------------------"
      ["Restore windows" mupad-restore-wind-conf
        :active (not (null mupad-registers-list))
        :help "Go to previous window configuration"]
      "----------------------------"
      (list 
        "Environment"
        ["Set DIGITS..." mupad-bus-set-digits :active 
          (processp mupad-run-process)]
        ["Adapt TEXTWIDTH" mupad-bus-adapt-textwidth :active 
          (processp mupad-run-process)
          :help 
"Set the textwidth of the mupad process to the actual width of your window"]
        ["PrettyPrint switch" mupad-bus-prettyprint-switch :active 
          (processp mupad-run-process) :help "Toggle the value of PRETTYPRINT"]
        "--------------------"
        ["Customize" mupad-run-customize-group :active t 
          :key-sequence nil])))))

(defvar MuPAD-run-menu-map nil)

(defun mupad-run-init-menu-bar ()
  "Add menu-bar item MuPAD in mupad-run-mode"
  (require 'easymenu)
  (when (and (featurep 'easymenu) (eq MuPAD-run-menu-map nil))
    (easy-menu-define MuPAD-run-menu-map 
      mupad-run-mode-map
      "Menu-bar item used under mupad-run-mode"
      (mupad-run-menu-bar))))
;;
;;
;;
(defun mupad-run-message-debug (item str) 
  (if (memq item mupad-run-debug) (message str)))
;;
;;
;; initialisation des couleurs
;;
(mapcar
  (lambda (a-face)
    (make-face (car a-face))
    (set-face-foreground (car a-face) (cadr a-face))
    (set-face-background (car a-face) (car (cddr a-face))))
  mupad-run-face)

(defun mupad-run-to-local-symb (symb arg)
  (intern (concat "local-" arg "-" (symbol-name symb))))

(mapcar
  (lambda (a-face)
    (let ((br (mupad-run-to-local-symb (car a-face) "A")) br1)
      (make-face br)
      (setq br1 (cadr a-face))
      (setq br1 (or (cadr (assoc (cons br1 "A") mupad-run-color-change)) br1))
      (set-face-foreground br br1)
      (setq br1 (car (cddr a-face)))
      (setq br1 (or (cadr (assoc (cons br1 "A") mupad-run-color-change)) br1))
      (set-face-background br br1)))
  mupad-run-face)
;;
;;
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;=-= FIN du fichier mupad-run.el =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
