;; $Id$
;;; mupad-run.el -- to use emacs as an editor for a session of mupad.

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
;; Pr�sentation dans le fichier mupad-run.el-info.
;; 
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-==
;;
;;  1/ Configuration, variables et constantes
;;  2/ Appel, lecture et sauvegarde des fichiers, impression 
;;  3/ Les sorties de mupad (y compris interruption et compl�tion)
;;  4/ Construction de la commande suivante de mupad et des s�parateurs
;;  5/ Suppression et ajout de commentaires s�parateurs
;;  7/ Traitement des appels syst�mes et affichage graphique
;;  8/ Historique des commandes et recherche
;;  9/ Commandes de d�placement sp�ciales
;; 10/ Suppression (temporaire) d'affichage
;;   11/ Gestion du couper/coller
;;   12/ Ajout de hook avant et apr�s les commandes envoy�es � mupad
;;   13/ Affichage du temps de calcul (calcul� par emacs)
;;   14/ Modification de la ligne d'�tat
;;   15/ Gestion des messages d'erreur d'emacs pour les adapter � mupad
;; 
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;
;; 1/ Configuration, variables et constantes
;;
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;
;; autres fichiers n�cessaires 
;;
(require 'mupad-bus)
(require 'mupad-help)
(require 'mupad) ; for mupad-start-manual
(require 'advice)
;(require 'mupad-xemacs)
;; Another stuff for xemacs:
(unless (fboundp 'char-int) (defun char-int (x) x))
;;
;;
;; d�finit l'extension mupad-run, et sa version.
;;
(provide 'mupad-run)
(defconst mupad-run-mode-version "3.00" "Version of `mupad-run.el'.")
;;
;;
;; Variables de configuration
;; 
;; mupad-run-command est le programme appel� ("mupad" en g�n�ral), suivi
;; de la liste des param�tres de la ligne de commande
;;   -R ou -E (-E est uniquement valable pour les versions >= 3.0) ;
;;   avec l'option -E les appels � l'aide en ligne de la forme ? sin 
;;   sont mieux g�r�s.
;;   Les options "-U" et "EMACS=TRUE" permettent de d�terminer dans mupad que
;;   le syst�me a �t� lanc� par emacs, pour configurer correctement
;;   vcam lors de l'initialisation de mupad, dans ~/.mupad/userinit.mu.
;;
;; La valeur de mupad-help-method concerne l'aide en ligne ; 
;; elle est 'mupad-help-from-toc-to-buffer avec l'option -R, 
;; et est 'mupad-help-from-file-to-buffer avec l'option -E.
;;
;; La variable mupad-run correspond indique le chemin d'acc�s au
;; fichier de pr�sentation de mupad-run.
;;
;; La valeur de mupad-run-hisptory-max indique le nombre maximal de commandes
;; m�moris�es dans l'historique de mupad.
;; 
(defgroup mupad-run nil
  "MuPAD customization subgroup the MuPAD shell"
  :group 'mupad :prefix "mupad-")
;
;(defcustom mupad-run-pgm "mupad"
;  "Command to run mupad"
;  :type 'string :group 'mupad-run)
;
; modifi� par la configuration automatique
(defcustom mupad-run-info 
  "/usr/local/share/emacs/site-lisp/mupad-run.el-info"
  "Location of the mupad-run.el-info file describing mupad-run.el"
  :type 'file :group 'mupad-run)

(defvar mupad-help-method 'mupad-help-from-toc-to-buffer)
;mupad-help-from-toc-to-buffer  --> valable pour l'option -R
;mupad-help-from-file-to-buffer --> valable pour l'option -E

(defvar mupad-run-less-questions nil
  "Set it to t if you want a more automated behaviour.
In which case the options for starting mupad won't be asked for.")

(defun mupad-run-help-method (val)
  (let ((l (split-string val " ")))
    (cond 
      ((member "-E" l) 'mupad-help-from-file-to-buffer)
      (t 'mupad-help-from-toc-to-buffer))))

(defun mupad-run-set-options (sym val) (set sym val))

(defcustom mupad-run-commandline
  "mupad -R -U EMACS=TRUE"
  "The command-line for the mupad process.

One of the '-R' or '-E' options is mandatory. Both tell MuPAD to use
its raw communication protocol. The -E options allows for some extra
features (e.g. P in the debugger), but requires MuPAD >= 2.5.1.

The '-U EMACS=TRUE' option is not mandatory, but this is the standard
and recommended way for telling MuPAD programms (typically in the user
initialization file) that MuPAD is currently running under emacs."
  :type '(choice (const "mupad -R -U EMACS=TRUE")
		 (const "mupad -E -U EMACS=TRUE")
		 (string :tag "Manual input"))
  :initialize 'custom-initialize-default
  :set 'mupad-run-set-options
  :group 'mupad-run)

(defvar mupad-run-commandline-history
  (list mupad-run-commandline)
  ;initialize 'custom-initialize-default
  "The history of the commands used to start mupad")

(defcustom mupad-run-history-max 100
  "Number of commands in the history"
  :type 'integer :group 'mupad-run)

(defcustom mupad-run-mode-hook-before nil
  "Hook for mupad-mode. Executed early by mupad-run."
  :type 'hook :group 'mupad-run)

(defcustom mupad-run-mode-hook '(mupad-help-init mupad-bus-adapt-textwidth)
  "Hook for mupad-mode. Executed last by mupad-run."
  :options '(mupad-help-init mupad-bus-adapt-textwidth)
  :type 'hook :group 'mupad-run)

; affichage de messages de debug
(defcustom mupad-run-debug-level '()
  "Controls the level of debug messages output by mupad-run."
  :type '(set (const filter)
	      (const input)
	      (const edit-to-todo)
	      (const todo-to-output))
  :group 'mupad-run)

(defun mupad-run-debug-message (item str)
  (if (memq item mupad-run-debug-level) (message "%s" str)))

(defun mupad-run-error (str) (ding) (message str))

(defun mupad-run-recenter ()
  ;; If the MuPAD buffer is visible in a window, recenter it to avoid
  ;; showing too many empty lines at the end of the buffer.

  ;; mupad-run-recenter looks at the position of the point to keep it
  ;; visible; so any movement of the point should be done before
  ;; calling it.

  ;; The real work is done by mupad-run-recenter-br
  (when (get-buffer-window (current-buffer)) ; the buffer is in a window
    (let ((brx (window-buffer)) (bry (current-buffer)))
      (cond
        ((string= (buffer-name bry) (buffer-name brx))
          (mupad-run-recenter-br))
        (t
          (pop-to-buffer bry t)
          (mupad-run-recenter-br)
          (pop-to-buffer brx t))))))

(defun mupad-run-recenter-br ()
  (cond ((eq mupad-run-recenter-behaviour 'agressive)
	 ;; Agressive recentering
	 (recenter (max 1
			(- (window-height)
			   (+ (max 1 (count-screen-lines (point) (point-max)))
			      mupad-run-recenter-bottom-margin
			      1)))))
	(t
	 ;; Conservative recentering. Francois: check this part!
	 (if (> (count-lines (window-start) (point)) ; "the cursor-line on the screen"
		(- (1- (window-height)) mupad-run-recenter-bottom-margin))
	     (recenter (- mupad-run-recenter-bottom-margin))))))

(defun mupad-run-set-system-trace (sym val)
  (set sym val)
  (save-excursion
    (mapcar
     (lambda (buf)
       (set-buffer buf)
       (when (eq major-mode 'mupad-run-mode)
	 (set sym val)))
     (buffer-list))))

(defcustom mupad-run-system-trace 0
  "Mainly intended for debugging purposes.
0 : Do not print commands send to system nor the answers
1 : Print commands send to system but not the answers
2 : Do not print commands send to system but the answers
3 : Print commands send to system and the answers"
  :type '(choice (const 0) (const 1) (const 2) (const 3))
  :initialize 'custom-initialize-default
  :set 'mupad-run-set-system-trace
  :group 'mupad-run)

(defcustom mupad-run-system-exception '("vcam")
  "Programs listed here will be executed in background when called
by system. Otherwise emacs will be blocked until the program has finished.
This list typically includes the external MuPAD graphics viewer vcam."
  :type '(repeat string)
  :group 'mupad-run)
;; les programmes appel�s par mupad dont le nom est dans la liste 
;; mupad-run-system-exception sont ex�cut�s en t�che de fond si la 
;; saisie de mupad est bloqu�e, et le contraire sinon.
;;
(defcustom mupad-run-buffer-name 0
  "0 - mupad-run-mode possible only on buffer with name *MuPAD*
1 - mupad-run-mode possible only on a buffer with name *MuPAD* or *MuPAD*<2>
2 - mupad-run-mode possible only on a buffer with name *MuPAD* or *MuPAD*<n>
3 - mupad-run-mode possible only on a buffer with name *MuPAD*xxxxx
4 - mupad-run-mode possible on all buffers"
  :type 'integer
  :group 'mupad-run)

(defcustom mupad-run-buffer-distinct-colorisation t
  "If t, the mupad buffers *MuPAD*, *MuPAD*<2>, and *MuPAD*<3> use
distinct faces (and in particular text colors) to tell them apart."
  :type 'boolean
  :group 'mupad-run)

;; 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defgroup mupad-run-faces nil
  "MuPAD customization subgroup for faces of the MuPAD shell"
  :group 'mupad-run :prefix "mupad-run")

(defface mupad-run-face-default
  `((t (:foreground ,(face-foreground 'default)
	:background ,(face-background 'default))))
  "Default face in mupad-run."
  :group 'mupad-run-faces)

(defface mupad-run-face-default-2
  `((((background dark)) (:foreground "white" :background "grey40"))
    (t                   (:foreground "black" :background "grey85")))
  "Default face in mupad-run."
  :group 'mupad-run-faces)

(defface mupad-run-face-default-3
  `((((background dark)) (:foreground "white" :background "yellow"))
    (t                   (:foreground "black" :background "DarkGreen")))
  "Default face in mupad-run."
  :group 'mupad-run-faces)

(defface mupad-run-face-result
  `((((background dark)) 
       (:foreground "lightblue" 
        :background ,(face-background 'mupad-run-face-default)))
    (t (:foreground "darkblue" 
        :background ,(face-background 'mupad-run-face-default))))
  "Face in mupad-run for results of MuPAD."
  :group 'mupad-run-faces)

(defface mupad-run-face-prompt
  `((((background dark)) 
       (:foreground "red" 
        :background ,(face-background 'mupad-run-face-default)))
    (t (:foreground "red" 
        :background ,(face-background 'mupad-run-face-default))))
  "Face in mupad-run for prompts."
  :group 'mupad-run-faces)

(defface mupad-run-face-local-prompt
  `((((background dark)) 
       (:foreground "pink" 
        :background ,(face-background 'mupad-run-face-default)))
    (t (:foreground "pink" 
        :background ,(face-background 'mupad-run-face-default))))
  "Face in mupad-run for all prompt but the first."
  :group 'mupad-run-faces)

(defface mupad-run-face-last-input
  `((((background dark)) 
       (:foreground "white" 
        :background ,(face-background 'mupad-run-face-default)))
    (t (:foreground "black" 
        :background ,(face-background 'mupad-run-face-default))))
  "Face in mupad-run for the last input."
  :group 'mupad-run-faces)

(defface mupad-run-face-for-emacs
  `((((background dark)) 
       (:foreground "grey50" 
        :background ,(face-background 'mupad-run-face-default)))
    (t (:foreground "grey50" 
        :background ,(face-background 'mupad-run-face-default))))
  "Face in mupad-run for emacs output."
  :group 'mupad-run-faces)

(defface mupad-run-face-separator
  `((((background dark)) (:foreground "white" :background "black"))
    (t                   (:foreground "black" :background "white")))
  "Face in mupad-run for comments inside emacs output."
  :group 'mupad-run-faces)

(defface mupad-run-face-call-system
  `((((background dark)) 
       (:foreground "firebrick" 
        :background ,(face-background 'mupad-run-face-default)))
    (t (:foreground "firebrick" 
        :background ,(face-background 'mupad-run-face-default))))
  "Face in mupad-run for call system commands."
  :group 'mupad-run-faces)

(defface mupad-run-face-system
  `((((background dark)) 
       (:foreground "saddlebrown" 
        :background ,(face-background 'mupad-run-face-default)))
    (t (:foreground "saddlebrown" 
        :background ,(face-background 'mupad-run-face-default))))
  "Face in mupad-run for results of system calls."
  :group 'mupad-run-faces)

(defface mupad-run-face-completion
  `((((background dark)) 
       (:foreground "lightgreen" 
        :background ,(face-background 'mupad-run-face-default)))
    (t (:foreground "darkgreen"  
        :background ,(face-background 'mupad-run-face-default))))
  "Face in mupad-run for lists of completions."
  :group 'mupad-run-faces)

(defface mupad-run-face-error
  `((((background dark)) (:foreground "white" :background "maroon"))
    (t                   (:foreground "black" :background "lightpink")))
  "Face in mupad-run for errors of MuPAD."
  :group 'mupad-run-faces)

(defface mupad-run-face-waiting-commands
  `((((background dark)) (:foreground "white" :background "darkcyan"))
    (t                   (:foreground "black" :background "cyan")))
  "Face in mupad-run for waiting commands."
  :group 'mupad-run-faces)

(defface mupad-run-face-beginning-waiting
  `((((background dark)) (:foreground "red" :background "darkcyan"))
    (t                   (:foreground "red" :background "cyan")))
  "Face in mupad-run in the waiting area for first character of a command."
  :group 'mupad-run-faces)

(defface mupad-run-face-beginning-rem
  `((((background dark)) (:foreground "white" :background "darkcyan"))
    (t                   (:foreground "black" :background "cyan")))
  "Face in mupad-run in the waiting area for first character of a comment."
  :group 'mupad-run-faces)

(defface mupad-run-face-beginning-waiting
  `((((background dark)) (:foreground "white" :background "darkcyan"))
    (t                   (:foreground "black" :background "cyan")))
  "Face in mupad-run in the waiting area for first character of a command."
  :group 'mupad-run-faces)

(defface mupad-run-face-default-flag
  `((((background dark)) (:background "blue"))
    (t                   (:background "lightblue")))
  "Default face in mupad-run for hiden data."
  :group 'mupad-run-faces)

(defun mupad-run-create-face-flag (x)
  (let ((br (intern (concat (symbol-name x) "-flag"))))
    (make-face br)
    (set-face-foreground br (face-foreground x))
    (set-face-background br (face-background 'mupad-run-face-default-flag))))

(mapcar 'mupad-run-create-face-flag 
  '(mupad-run-face-result     mupad-run-face-prompt mupad-run-face-for-emacs 
    mupad-run-face-separator  mupad-run-face-error  mupad-run-face-completion
    mupad-run-face-system     mupad-run-face-call-system))

(defun mupad-run-other-faces (postfix base facex)
  (let ((br (intern (concat (symbol-name facex) postfix))))
    (make-face br)
    (set-face-foreground br (face-foreground facex))
    (cond
      ((equal 
          (face-background facex) 
          (face-background 'mupad-run-face-default))
        (set-face-background br (face-background base)))
      (t (set-face-background br (face-background facex))))))

(mapcar 
   (lambda (x) (mupad-run-other-faces "-2" 'mupad-run-face-default-2 x))
   '(mupad-run-face-local-prompt        mupad-run-face-result    
     mupad-run-face-prompt              mupad-run-face-last-input   
     mupad-run-face-for-emacs           mupad-run-face-system
     mupad-run-face-completion          mupad-run-face-call-system))

(mapcar 
   (lambda (x) (mupad-run-other-faces "-3" 'mupad-run-face-default-3 x))
   '(mupad-run-face-local-prompt        mupad-run-face-result    
     mupad-run-face-prompt              mupad-run-face-last-input   
     mupad-run-face-for-emacs           mupad-run-face-system
     mupad-run-face-completion          mupad-run-face-call-system))

(defvar mupad-run-mode-map nil "Touches d�finies par mupad-run-mode.")

(when (not mupad-run-mode-map) 
  (let ((map (make-sparse-keymap)))
    (define-key map [(control return)] (function mupad-run-creturn))
    (define-key map [(control up)] (function mupad-run-previous-history))
    (define-key map [(control down)] (function mupad-run-next-history))

    ;; Dans eshell et erc (Emacs Shell et Emacs IRC Client) les cl�s
    ;; M-p et M-n sont pour l'historique
    (define-key map [(meta p)] (function mupad-run-previous-history))
    (define-key map [(meta n)] (function mupad-run-next-history))
    
    (define-key map [(control prior)] 
      (function mupad-run-previous-history-search))
    (define-key map [(control next)] (function mupad-run-next-history-search))
    (define-key map [(control left)] (function mupad-run-left))
    (define-key map [(control right)] (function mupad-run-right))
    (define-key map [(control delete)] (function mupad-run-hide))
    (define-key map [(control insert)] (function mupad-run-show))
    (define-key map "\r" (function mupad-run-return))
    (define-key map [(control ?d)] (function mupad-run-suppression))
    (define-key map [delete]       (function mupad-run-suppression))
    (define-key map [backspace] (function mupad-run-backspace))
    (define-key map [(control ?i)] (function mupad-run-tab))
    (define-key map [(control ?c) (control ?c)] (function mupad-run-break))
    (define-key map [(control ?c) (control ?s)] (function mupad-run-save))
    (define-key map [(control ?c) (control ?w)] (function mupad-run-save))
    (define-key map [(control ?c) ?k]  (function mupad-run-end))
    (define-key map [(control ?c) ?0]  (function mupad-run-reset))
    (define-key map [(control ?c) ?1]  
      (function mupad-run-insert-last-session))
    
; Les cl�s F5 - F9 sont reserv� � l'utilisateur
    
;    (define-key map [f5] (function mupad-help-emacs-search))
    (define-key map [(control ?c) (control ?h)] 
      (function mupad-help-emacs-search))
;    (define-key map [f6] (function mupad-help-emacs-ask))
    (define-key map [(control ?c) (control ?i)] 
      (function mupad-help-emacs-ask))
    (define-key map [(control ?y)] (function mupad-run-yank))
    (define-key map [mouse-2] (function mupad-run-yank-at-click))
    (setq mupad-run-mode-map map)))

(eval-and-compile
  (mapcar (lambda (sym) (eval (list 'defvar sym nil)))
  '(mupad-run-edit mupad-run-todo mupad-run-last-prompt
    mupad-run-itema mupad-run-itemb mupad-run-last-type
    mupad-run-output mupad-run-state mupad-run-time-start 
    mupad-run-hist-commands mupad-run-save-except mupad-run-debug-level 
    mupad-run-save-buffer mupad-run-prompt
    mupad-run-completion-prefix mupad-run-completion-completions
    mupad-run-rawcommand mupad-run-debugger-file mupad-run-debugger-line
    mupad-run-last-session mupad-run-buffer-face)))

(defvar mupad-run-process nil)

(defun mupad-run-set-arrow-behaviour (symbol val)
  "See `mupad-run-arrow-behaviour'"
  (setq mupad-run-arrow-behaviour val)
  (cond
    ((string= val "Usual")
      (define-key mupad-run-mode-map [(control up)]
        (function mupad-run-previous-history))
      (define-key mupad-run-mode-map [(control down)]
        (function mupad-run-next-history))
      (define-key mupad-run-mode-map [(up)] (function previous-line))
      (define-key mupad-run-mode-map [(down)] (function next-line)))
    ((string= val "Usual-Search")
      (define-key mupad-run-mode-map [(control up)]
        (function mupad-run-previous-history-search))
      (define-key mupad-run-mode-map [(control down)]
        (function mupad-run-next-history-search))
      (define-key mupad-run-mode-map [(up)] (function previous-line))
      (define-key mupad-run-mode-map [(down)] (function next-line)))
    ((string= val "Bash-Style")
      (define-key mupad-run-mode-map [(up)] 
        (function mupad-run-previous-history))
      (define-key mupad-run-mode-map [(down)] 
        (function mupad-run-next-history))
      (define-key mupad-run-mode-map [(control up)] (function previous-line))
      (define-key mupad-run-mode-map [(control down)] (function next-line)))
    (t ; for MuPAD-Style
      (define-key mupad-run-mode-map [(up)]
        (function mupad-run-previous-history-search))
      (define-key mupad-run-mode-map [(down)]
        (function mupad-run-next-history-search))
      (define-key mupad-run-mode-map [(control up)] (function previous-line))
      (define-key mupad-run-mode-map [(control down)] (function next-line)))))

(defcustom mupad-run-arrow-behaviour
  "Usual-Search"
  "Selects the behaviour of the up, down, C-up, C-down arrows:
  Usual:
   - up and down: move in the buffer
   - C-up and C-down: move through the history
  Usual-Search:
   - up and down: move in the buffer
   - C-up and C-down: search through the history
  Bash-Style:
   - up and down: move through the history
   - C-up and C-down: move in the buffer
  MuPAD-Style:
   - up and down: search through the history;
   - C-up and C-down: move in the buffer"
  :type '(choice (const "Usual-Search")
		 (const "Usual")
		 (const "Bash-Style")
		 (const "MuPAD-Style"))
  :initialize 'custom-initialize-default
;do not use mupad-run-set-arrow-behaviour
;initially since the map is not yet defined !
  :set 'mupad-run-set-arrow-behaviour
  :group 'mupad-run)

(defcustom mupad-run-recenter-behaviour nil
  "Selects how the buffer is recentered after history lookups, MuPAD
output, and so on. nil means no recentering. agressive means recenter
the window to show as many lines of the command/output as possible,
without showing empty lines after it."
  :type '(radio (const nil) (const agressive))
  :group 'mupad-run)

(defcustom mupad-run-recenter-bottom-margin 1
  "Maximum number of empty lines left at the bottom of the buffer on
recentering."
  :type 'integer :group 'mupad-run)

(defcustom mupad-run-completion-style 'temporary-buffer
  "Selects where completion lists are displayed:
* temporary-buffer means in a temporary buffer
* inline means inside the MuPAD buffer"
  :type '(radio (const inline) (const temporary-buffer))
  :group 'mupad-run)

(defcustom mupad-run-completion-strip-prefix t
  "Selects where completion lists are displayed:
* temporary-buffer means in a temporary buffer
* inline means inside the MuPAD buffer"
  :type '(boolean)
  :group 'mupad-run)

(defcustom mupad-run-input-terminator " ;"
  "The string that is appended to those input that do not readilly end
by a ';' or ';'. The purpose of this feature is to make the session
rereadable by MuPAD."
  :type '(string)
  :group 'mupad-run)

(defconst mupad-run-automate-exception
  '((( 0 . ?\n) .  1) (( 0 . ?\\) .  2) (( 0 . ?\") . 10) (( 0 . ?\/) .  3) 
    (( 0 . ?\*) .  5)
; a modifier ? en 
    (( 3 . ?\*) . 80)
; ancienne version 
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
;; le mode mupad-run utilise les variables suivantes:
;;   un tampon de nom *MuPAD*                               
;;   un processus point�                                 [ mupad-run-process ]
;;   l'�tat du programme mupad (attente ou calcul)         [ mupad-run-state ]
;;   valeurs : 'beginning 'wait-input 'wait-debugger-input 'running 'run-debug
;;   la cha�ne de caract�res r�sultat                     [ mupad-run-output ]
;;   l'instant de lancement de la derni�re commande   [ mupad-run-time-start ]
;;   les commandes associ�es au clavier                 [ mupad-run-mode-map ]
;;   un marqueur sur le d�but des calculs � faire           [ mupad-run-todo ]
;;   un marqueur sur le d�but de la zone �ditable           [ mupad-run-edit ]
;;   un marqueur au d�but du prompt pr�c�dent        [ mupad-run-last-prompt ]
;;   d�but du nom dont la compl�tion est recherch�e   [ mupad-run-completion-prefix ]
;;   list of MuPAD completions: nil or a comma separated string [ mupad-run-completion-completions ]
;;   cha�ne du prompt                                     [ mupad-run-prompt ]
;;   un compteur pour s�parer les commandes en attente     [ mupad-run-itema ]
;;   un compteur pour s�parer les sorties de mupad         [ mupad-run-itemb ]
;;   les attributs des polices de caract�res                [ mupad-run-face ]
;;   l'historique des commandes                        [ mupad-hist-commands ]
;;   la derni�re commande d'entr�e envoy�e � mupad    [ mupad-run-rawcommand ]
;;   affichage de messages de debug                  [ mupad-run-debug-level ]
;;     liste de mots clefs: ('filter 'input 'edit-to-todo 'todo-to-output)
;;        mupad-run-last-session mupad-run-save-except
;;
;; fonctions principales : 
;;   lancement du mode mupad-run dans un nouveau tampon          [ mupad-run ]
;;   sortie de  mupad-run apr�s confirmation de sauvegarde   [ mupad-run-end ]
;;   r�initialisation du mode mupad-run                    [ mupad-run-reset ]
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
    ((= mupad-run-buffer-name 3) 
      (string= (substring str 0 (min (length str) 7)) "*MuPAD*"))
    ((= mupad-run-buffer-name 4) t)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-commandline-ask nil
  ;; Let the user edit the mupad command (mupad-run-commandline) in the
  ;; minibuffer. The edition is repeated until the command is non
  ;; trivial.
  (while (<= (length (split-string
		      (setq mupad-run-commandline
			    (read-from-minibuffer
			     "Command to start mupad: "
			     mupad-run-commandline nil nil
			     'mupad-run-commandline-history))
		      " "))
	     1)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defalias 'run-mupad 'mupad-run)
(defun mupad-run nil
  (interactive)
  (switch-to-buffer "*MuPAD*")
  (mupad-run-mode))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-new ()
  (interactive)
  (let ((brs (generate-new-buffer-name "*MuPAD*")))
    (cond 
      ((mupad-run-buffer-name-p brs)
        (switch-to-buffer brs) (mupad-run-mode))
      (t (error "A new buffer for mupad is impossible")))))
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
  "Major mode for running MuPAD inside an emacs buffer.

For customization, see M-x customize-group mupad-run .

See also http://mupacs.sourceforge.net/ for extra documentation

Some help from MuPAD is needed for viewing graphics. Please insert the
following lines in your MuPAD initialization file ~/.mupad/userinit.mu :

///////////////////////////////////////////////////////////////////////////
// Customization when running inside an emacs buffer with mupacs
if has({text2expr(Pref::userOptions())}, hold(EMACS)) then
    if package(\"javaview\", Quiet) = FAIL then
    // With MuPAD < 3.1.1, patch stdlib::plot to call the external 
    // MuPAD graphics viewer vcam
    proc()
	local oldProtectState ;
    begin
	oldProtectState := protect(stdlib, ProtectLevelNone) ;
	stdlib::oldPlot := stdlib::plot:
	stdlib::plot :=
	proc()
	    local tmpfile;
	begin
	    // Comment this out to get rid of the message
	    tmpfile := \"/tmp/muplot\" . getpid();
	    fprint(Unquoted, 0, \"Calling vcam \" . tmpfile);
	    stdlib::oldPlot(PlotDevice=[tmpfile, Binary], args());
	    // You may need to edit this to suit your MuPAD installation
	    // See also the emacs variable mupad-run-system-exception
	    system(\"vcam \".tmpfile);
	end_proc;
	protect(stdlib, oldProtectState);
    end_proc();
    end_if:
end_if:
///////////////////////////////////////////////////////////////////////////

Available special keys:
\\{mupad-run-mode-map}"
  (interactive)
  (when (mupad-run-mode-control)
    (unless mupad-run-less-questions (mupad-run-commandline-ask))
    (mupad-run-mode-intern)))

(defun mupad-run-mode-control ()
  (cond 
    ((and 
        (eq major-mode 'mupad-run-mode) 
        mupad-run-process 
        (processp mupad-run-process)) 
      (message "Buffer already in mupad-run mode")
      nil)
    ((eq major-mode 'mupad-run-mode)
      (error "Mupad doesn't run in this mupad-run buffer"))
    ((and (not (eq major-mode 'mupad-run-mode)) mupad-run-process)
      (error "Mupad runs inside a buffer in an other mode"))
    ((not (mupad-run-buffer-name-p (buffer-name (current-buffer))))
      (error "Buffer name isn't allowed for mupad-run mode"))
    (t t)))

(defun mupad-run-process-fct (str)
  (let ((l (split-string str)))
    (apply (function start-process) 
      "mupad" (current-buffer) (car l) (cdr l))))

(defun mupad-run-mode-intern ()
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
       mupad-run-edit mupad-run-todo
       mupad-run-last-prompt mupad-run-hist-commands 
       mupad-run-output mupad-run-state 
       mupad-run-itema mupad-run-itemb mupad-run-last-type 
       mupad-run-time-start mupad-run-prompt
       mupad-run-save-except mupad-run-debug-level
       mupad-run-last-session mupad-run-buffer-face
       ;; Automatic completion
       mupad-run-completion-prefix mupad-run-completion-completions
; NT 04/11/2002 added for the debugger
       mupad-run-rawcommand
       mupad-run-debugger-file	mupad-run-debugger-line))
; construction de la couleur de fond
    (when mupad-run-buffer-distinct-colorisation
      (when (string= (buffer-name) "*MuPAD*<2>")
	(setq mupad-run-buffer-face "-2"))
      (when (string= (buffer-name) "*MuPAD*<3>")
	(setq mupad-run-buffer-face "-3")))
  (run-hooks 'mupad-run-mode-hook-before)
  (setq mupad-run-edit (make-marker))
  (set-marker mupad-run-edit (point))
  (setq mupad-run-todo (make-marker))
  (set-marker mupad-run-todo (point))
; autres markeurs pour la compl�tion et les messages d'erreur
  (setq mupad-run-last-prompt (make-marker))
  (set-marker mupad-run-last-prompt nil)
  (setq mupad-run-itema 1) 
  (setq mupad-run-itemb 1) 
  (setq mupad-run-last-type -1)
; gestion de l'historique 
  (setq mupad-run-hist-commands (head-tail-void))
  (ptr-to-head mupad-run-hist-commands)
; gestion du curseur
  (when mupad-run-recenter-behaviour (setq scroll-conservatively 1))
; lancement du programme 
  (setq mupad-run-output "")
  (setq mupad-run-state 'beginning)
  (setq mupad-run-process (mupad-run-process-fct mupad-run-commandline))
; methode d'acc�s � l'aide en ligne
  (setq mupad-help-method (mupad-run-help-method mupad-run-commandline))
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
; configuration du mode majeur et �valuation du hook
  (setq major-mode 'mupad-run-mode) 
  (setq mode-name "MuPAD-run")
; the last raw command sent to MuPAD:
;   nil or a list (message-type message) like (1 "x+1")
  (setq mupad-run-rawcommand nil)
; the file position in the debugger: nil or a string
; used for setting breakpoints
  (setq mupad-run-debugger-file nil)
; the file position in the debugger: nil or an int
  (setq mupad-run-debugger-line nil)

  (run-hooks 'mupad-run-mode-hook))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-end () 
  "Kill the first buffer in mupad-run-mode"
  (interactive)
  (mupad-run-delete-windows "*MuPAD*Completions*")
  (let ((bfl (buffer-list)) (inhibit-read-only t))
    (save-excursion
      (while bfl
      (set-buffer (car bfl))
      (when (eq major-mode 'mupad-run-mode)
; kill-process est superflu car il attach� au tampon
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
    ( br1 (br (buffer-name (current-buffer))) (brc mupad-run-hist-commands) 
      (mupad-run-save-except 'reset) (brd default-directory)
      (inhibit-read-only t))
    (mupad-run-store-line (buffer-substring mupad-run-edit (point-max)))
    (mupad-run-save)
    (setq br1 mupad-run-last-session)
    (when (processp mupad-run-process)
      (setq mupad-run-save-except 'kill)
      (kill-buffer br)
      (switch-to-buffer (set-buffer (get-buffer-create br)))
      (setq default-directory brd)
      (mupad-run-mode)
      (setq mupad-run-last-session br1)
      (setq mupad-run-hist-commands brc)
      (goto-char (point-max)))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-insert-last-session ()
  "Insert the last MuPAD session"
  (interactive)
  (mupad-run-delete-windows "*MuPAD*Completions*")
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
      (goto-char brp1) (insert "///--- commandes non encore �valu�es\n"))
    (when (and brp3 (<= brp3 brp1))
      (goto-char (- brp3 2))
      (insert "///--- commandes en cours d'�valuation\n"))
    (goto-char (point-min))
    (while (not (eobp))
      (setq br1 (mupad-run-get-face (point)))
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;
;; 3/ Les sorties de mupad (y compris compl�tion)
;;
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; affichage des donn�es envoy�es par MuPAD
;; 
;; mupad-run-filter est la fonction principale.
;;   elle traite les donn�es re�ues au fur et � mesure (mupad-run-output),
;;   g�re la possibilit� d'�criture dans le tampon *MuPAD* (mupad-run-state),
;;   g�re la position du curseur de mupad (mupad-run-todo)
;;   var locales : save-buffer, output-type, output-index, output-str
;;
;; mupad renvoie ^F n donn�es ^G avec les valeurs de n : 
;; 2 : r�sultat de MuPAD                  9 : erreur
;; 3 : appel syst�me,                    32 : �num�ration des compl�tions
;; 6 : largeur de l'affichage (en ascii) 33 : fin de l'�num�ration 
;; 7 : type d'affichage (prettyprint)   255 : perte de synchro. (par emacs)
;; 8 : aide en ligne 
;;
(defun mupad-run-filter (proc str)
  (mupad-run-debug-message 'filter
			   (concat "MuPAD raw output: " str))
  (let
    ((output-index 0) output-type output-str brt (inhibit-read-only t)
     (brc (current-buffer)) (brb (process-buffer proc)))
     (set-buffer brb)
     (setq mupad-run-output (concat mupad-run-output str))
; (message mupad-run-output)
; tant-qu'il y a des donn�es compl�tes � traiter, le faire
    (while (setq output-type (mupad-run-output-complete-data output-index))
      (setq output-str     
        (substring 
          mupad-run-output (+ 2 (car output-type)) (cadr output-type)))
      (setq brt (char-int (car (cddr output-type))))
      (mupad-run-debug-message 'filter 
        (concat "MuPAD output: [" (number-to-string brt) "] " output-str))
; traiter les diff�rents types de donn�es renvoy�es par mupad
      (cond
        ((eq brt 2)   ; output
          (mupad-run-print-result output-str brt))
        ((eq brt 61) ; NT: debugger output MPRCmdb_output
	 ;; Workaround: with MuPAD < 3.1.1 and the -E option, the file
	 ;; line information is output via MPRCmdb_output (61) instead
	 ;; of MPRCmdb_file_pos (34). The following code detects this,
	 ;; strips away the information from output-str, and calls
	 ;; mupad-run-debugger-display-line appropriately.  Hopefully,
	 ;; there won't be false detections.
	 (if (string-match "^\\(#[0-9]+	in.*\n\\)*\\(\\([0-9]+\\)[ 	]in \\(\\S-+\\)\n\\)$" output-str)
	   (let ((file (match-string 4 output-str))
		 (line (string-to-number (match-string 3 output-str))))
	     (setq output-str (replace-match "" t t output-str 2))
	     (mupad-run-debugger-display-line file line)))
	 ;; Same treatment as for normal output (2) above
          (mupad-run-print-result output-str brt))
        ((eq brt 13) ; prompt 
	   (mupad-run-print-prompt output-str brt)
           (setq mupad-run-state 'wait-input)
	   )
	((eq brt 35)      ; display variable MPRCmdb_disp_list
	 (mupad-run-print-result (concat output-str "\n") brt))
        ((or (eq brt 9)   ; error message
	     (eq brt 63)) ; NT: debugger error message
	 (mupad-run-print-error output-str brt))
        ((eq brt 3) (mupad-run-call-system output-str)) ; system-call
        ((eq brt 6)) ; change to TEXTWIDTH
        ((eq brt 7)) ; change to PRETTYPRINT
        ((eq brt 8)  ; online documentation
          (condition-case err
           (apply mupad-help-method (list output-str))
           (error (message "%s" (error-message-string err))))
          (set-buffer brb))

	;; Completion lists returned by MuPAD
	;; receive the completion list (possibly in several pieces)
	;; output-str contains:
	;; - Multiple completions: the completions, in full, separated by ", "
	;; - No completion: the string "^H^G"
	;; - A single completion: empty string
	((eq brt 32) (mupad-run-completion-callback-partial output-str))
	;; end of completion list
	;; output-str is the common prefix of all completion items
	((eq brt 33) (mupad-run-completion-callback-end output-str))

; D�but des modifications NT 04/11/2002
        ((or (eq brt 34)            ; MPRCmdb_file_pos
;	     (eq brt 41)
	     (eq brt 66))           ; MPRCmdb_where
; debugger -> frontend: display file at position line no.
	 (string-match "^\\(\\S-+\\)\n\\([0-9]+\\)" output-str)
	 (mupad-run-debugger-display-line
	  (match-string 1 output-str)
	  (string-to-number (match-string 2 output-str)))
	 ;; Ask the kernel to display the variables MPRCmdb_disp_list
	 ;; Maybe that should only be done with 34
	 (process-send-string mupad-run-process
			      (concat "\006" (string 35) "\007\n"))
	 (setq mupad-run-state 'wait-display-list)
	 )
	((eq brt 62) ; debugger prompt
	 ; kernel has stopped and waits for the next debugger command
	 (if (equal mupad-run-state 'wait-display-list)
	     (setq mupad-run-state 'wait-prompt)
	   (mupad-run-print-prompt output-str brt)
	   (setq mupad-run-state 'wait-debugger-input)))
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
; raccourcissement de la cha�ne � traiter � la fin de la boucle 
  (setq mupad-run-output (substring mupad-run-output output-index))
; maj de l'affichage dans une session quand le curseur est � la fin
  (when (eq (point) (point-max)) (mupad-run-recenter))
  (set-buffer brc)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; mupad-run-output-complete-data 
;;   renvoie  
;;   (deb, fin, type) s'il y a une s�rie de donn�es compl�te � afficher 
;;     entre les indices deb (compris) et fin (compris)
;;     l'index suivant commence � fin+1
;;   nil sinon
;;   type est la valeur renvoy�e par MuPAD
;;      ou 255 si une erreur est d�tect�e dans le flux des donn�es.
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
(defun mupad-run-to-local-symb (symb arg)
  (let (br)
    (cond 
      (arg 
        (setq br (intern (concat (symbol-name symb) arg)))
        (if (facep br) br symb))
      (t symb))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-put-face (pt1 pt2 aspect)
  (put-text-property pt1 pt2 'face 
    (mupad-run-to-local-symb aspect mupad-run-buffer-face)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-get-face (pt)
  (let ((br (symbol-name (get-text-property pt 'face))))
    (if (string= (substring br -2 -1) "-")
      (intern (substring br 0 -2))
      (intern br))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-print (str aspect pos type ajout)
    (save-excursion 
      (unless (eq type mupad-run-last-type)
        (setq mupad-run-itemb (1+ mupad-run-itemb))
        (setq mupad-run-last-type type))
      (goto-char pos) 
      (insert-before-markers str)
      (put-text-property pos (point) 'item mupad-run-itemb)
      (mupad-run-put-face pos (point) aspect)
      (put-text-property pos (point) 'rear-nonsticky t)
      (put-text-property pos (point) 'front-sticky t)
      (put-text-property pos (point) 'read-only t)
      (add-text-properties pos (point) ajout)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Printing callbacks for mupad-run-filter
;; FIXME: Can we get rid of the 'type' argument?
;; FIXME: rename as mupad-run-print-callback-... ?

(defun mupad-run-print-result (string type)
  (if (string-match "^DISPLAY HELP FILE: \\(\\S-+\\)$" string)
      (let ((file (match-string 1 string)))
	(save-excursion (mupad-help-display-help-file file))
	(setq string "")))
  (mupad-run-print string
		   'mupad-run-face-result
		   (marker-position mupad-run-todo) type nil))

(defun mupad-run-print-prompt (string type)
  (setq mupad-run-prompt output-str)
  (set-marker mupad-run-last-prompt (1- mupad-run-todo))
  (mupad-run-print (concat output-str "\n")
		   'mupad-run-face-prompt
		   (marker-position mupad-run-todo) type nil)
  (set-marker mupad-run-last-prompt (1+ mupad-run-last-prompt))
  ;; Conservatively reset the undo list to make sure that the
  ;; user will not revert inadvertently the insertion of the
  ;; prompt, as this would invalidate the marker positions
  ;;
  ;; Warning: the user will loose undo information if he is
  ;; editing a command when the prompt is inserted; I don't
  ;; know if/how one can disable selectively the undoing of
  ;; the insertion of the prompt without disabling all the
  ;; previous undo information.
  (setq buffer-undo-list nil)
  )

(defun mupad-run-print-error (string type)
  (mupad-run-print string
		   'mupad-run-face-error
		   (marker-position mupad-run-todo) type nil)
  (when (marker-position mupad-run-last-prompt)
    (put-text-property mupad-run-last-prompt 
		       (1+ mupad-run-last-prompt) 
		       'to-insert "///--- Erreur dans ce bloc\n")
    (put-text-property (1- mupad-run-todo) mupad-run-todo  
		       'to-insert "///--- Fin du bloc avec une erreur\n"))
  ;; When the error message contains a file/line/col information, open
  ;; the file at this position in a separate buffer
  ;; First get rid of linesplits by the MuPAD pretty printer
  (setq string (mapconcat 'identity (split-string string "\\\\\n") ""))
  (if (string-match
       "^Error:.*\\[file \\(\\S-+\\), line \\([0-9]+\\), col \\([0-9]+\\)\\];$"
       string)
      (let ((file (match-string 1 string))
	    (line (string-to-number (match-string 2 string)))
	    (col  (string-to-number (match-string 3 string))))
	(mupad-run-display-line file line col)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatic completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mupad-run-tab ()
  "Search last word completion"
  (interactive)
  (mupad-run-debug-message 'input "Tab")
  ;; NT: TODO: modify to allow for completion in debugger commands
  ;; see also mupad-run-completion-*
  (when (< (point) mupad-run-edit)
    (error "Completion only in the edit zone"))
  ;; Filename completion inside a read("....
  (if (and (save-excursion
	     (re-search-backward "\\bread(\"\\([^\"]*\\)" (1- mupad-run-edit) t))
	   (= (match-end 0) (point)))
      (let ((prefix (match-string 1)))
	(let ((directory        (or (file-name-directory prefix) ""))
	      (partial-file-name (file-name-nondirectory prefix)))
	  (let ((completions
		 (file-name-all-completions partial-file-name directory))
		(common-prefix
		 (file-name-completion partial-file-name directory)))
	    (mupad-run-completion-insert-or-display-completions
	     prefix
	     completions
	     (if common-prefix
		(progn
		  (if (eq common-prefix t)
		      ;; the file name is already complete
		      ;; (cf. file-name-completion)
		      (setq common-prefix partial-file-name))
		  (if (and (eq (length completions) 1)
			   (not (string-match "/$" (car completions))))
		      ;; a single completion which is not a directory;
		      ;; prepend ") to close the read("
		      (setq common-prefix (concat common-prefix "\")")))
		  ;; Strips away the prefix
		  (substring (concat directory common-prefix)
			     (length prefix)))
	       nil)))))
    ;; Identifier/keyword/... completion by MuPAD
    (when (not (memq mupad-run-state '(wait-input)))
      (error "MuPAD computes - completion impossible"))
    (when (save-excursion
	    (re-search-backward "[^a-zA-Z0-9_:\\.]" (1- mupad-run-edit) t))
      (let ((prefix (buffer-substring (match-end 0) (point))))
	(mupad-run-debug-message
	 'edit-to-todo
	 (concat "MuPAD input: ["
		 (number-to-string 37) "] "
		 prefix))
	(process-send-string
	 mupad-run-process
	 (concat
          "\006" (string 31)
	  prefix
          "\007\n"))
	(setq mupad-run-state 'wait-for-completion)
	(setq mupad-run-completion-prefix prefix) ;; Check if this is required
	(mupad-run-debug-message 'input "Waiting for mupad output")
	;; Assertion: the following callbacks will be triggered:
	;;  - mupad-run-completion-callback-partial
	;;  - mupad-run-completion-callback-end
	(while (not (eq mupad-run-state 'wait-input))
	  (accept-process-output mupad-run-process))
	(mupad-run-debug-message 'input "Done")
	))))

;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Handle the insertion or display of the completion at (point)
;; FIXME: find a better name
;;
;; tab ouvre une completion
;;    dans la fen�tre compl�tion, la fen�tre mupad ou le minibuffer
;; fen�tre de compl�tion ferm�e : 
;;    apr�s que mupad r�pond / seule compl�tion
;;    apr�s 5 secondes de non utilisation
;; dans la fen�tre : le premier tab compl�te et maj la compl�tion
;;    le suivant fait tourner la fen�tre de compl�tion RAZ 
;;    de la dur�e de fermeture de la fen�tre de compl�tion
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
(defun mupad-run-completion-insert-or-display-completions
  (prefix            ; The string being completed
   completions       ; The list of all completions, nil if there is none
                     ; This is intended for display only, so the entries may
                     ; be abbreviated/decorated in any suitable way
   common-completion ; The common part of all completions that
                     ; can be inserted right away in the buffer
                     ; Unused if there is no completions
   )

  (if (not completions)
      (progn
	;; No completions
	(message (concat
		  "Sorry, no completion available for `"
		  prefix "' !"))
	(when (eq mupad-run-completion-style 'temporary-buffer)
	  (mupad-run-delete-windows "*MuPAD*Completions*")))

    (insert common-completion)

    (if (> (length completions) 1)
	;; Multiple completions: display the completion list
	(cond
	 ((eq mupad-run-completion-style 'temporary-buffer)
	  (with-output-to-temp-buffer "*MuPAD*Completions*"
	    (display-completion-list completions)))
	 ((eq mupad-run-completion-style 'inline)
	  (mupad-run-momentary-string-display
	   (concat "\n" (mapconcat 'identity completions " "))
	   (line-end-position) "" "Several completions available; type any key to erase the completion list")))
      ;; A single completion
      (message "Complete identifier")
      (when (eq mupad-run-completion-style 'temporary-buffer)
	(mupad-run-delete-windows "*MuPAD*Completions*")))))

;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Process filter call back: accumulate completions at the end of the
;; current list of MuPAD completions mupad-run-completion-completions.
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
(defun mupad-run-completion-callback-partial (completions)
  (if (not mupad-run-completion-completions)
    (setq mupad-run-completion-completions completions)
    (setq mupad-run-completion-completions
	  (concat mupad-run-completion-completions completions))))

;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;; Process filter call back: the list of MuPAD completions
;; mupad-run-completion-completions is now complete. Treat it.
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
(defun mupad-run-completion-callback-end (common-completion)
    (setq mupad-run-state 'wait-input)
    (let ((completions mupad-run-completion-completions))
      (setq mupad-run-completion-completions nil)
      ;; Transform completions into a clean list of completions
      ;; or nil if there is none
      (if (string= "\010\007" completions)
	  (setq completions nil)
	(setq completions
	      (or (split-string completions ", ")
		  (list (concat mupad-run-completion-prefix
				common-completion))))
	(if mupad-run-completion-strip-prefix
	    ;; strips away the bla::ble:: prefixes for a more compact output
	    (setq completions
		  (mapcar
		   (lambda (s)
		     (if (string-match "\\(.*::\\)." s)
			 (replace-match "" nil nil s 1)
		       s))
		   completions))))
      (mupad-run-completion-insert-or-display-completions
       mupad-run-completion-prefix
       completions
       common-completion)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debugger
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mupad-run-debugger-display-line (file line)
  "Displays the file/line/col the debugger is running on in a separate buffer"
  (cond
   ((string-match "^\\(.*\\.tar\\)#\\(.*\\)$" file)
    ;; Special treatment for tar files:
    (let
	((tarfile (match-string 1 file))
	 (subfile (match-string 2 file)))
      (setq file tarfile)))
   ((string-match "^/tmp/debug[0-9]\\.[0-9]+$" file)
					;[:digit:]\\.[:digit:]+
    ;; Special treatment for debug files /tmp/debug*:
    ;;  - Opened read-only in MuPAD-mode
    ;;  - Reverted at each iteration
    (save-excursion
      ;; (message (concat "Check for debug buffer" file))
      (if (get-file-buffer file)
	  (set-buffer (get-file-buffer file))
	;; (message "Open buffer")
	(set-buffer (find-file-noselect file t))
	;; (message "Switch to mupad-mode")
	(mupad-mode))
      ;; (message "Revert buffer")
      (revert-buffer t t t)
      ;; (message "Set buffer read only")
      (setq buffer-read-only t)
      )))
  ;; (message "Displaying line")
  (mupad-run-display-line file line)
  ;; (message "Setting file")
  (setq mupad-run-debugger-file file)
  (setq mupad-run-debugger-line line)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Low level routines to display a file/line/col in another buffer
;; This is essentially duplicated from gud, with just a few small modifications.
;; Yeah, that's bad, but:
;;  - This makes us fully independent on gud; no more tweaking to let
;;    gud think that this is a normal gud buffer; no more version
;;    incompatibilities
;;  - Allow for minor adaptations (displaying the column)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Taken from gud-gdb-find-file of emacs 21.3
;; It seems not to be defined in emacs from CVS (NT, CCR, 2004/10/04)
(defun mupad-run-find-file (f) (find-file-noselect f 'nowarn))

;; Adapted from gud-diplay-line of emacs 21.3
(defun mupad-run-display-line (true-file line &optional col)
  (let* ((last-nonmenu-event t)  ; Prevent use of dialog box for questions.
         (buffer
          (save-excursion
            ;(or (eq (current-buffer) gud-comint-buffer)
            ;    (set-buffer gud-comint-buffer))
            (mupad-run-find-file true-file)))
         (window (and buffer (or (get-buffer-window buffer)
                                 (display-buffer buffer))))
         (pos))
    (if buffer
        (progn
          (save-excursion
            (set-buffer buffer)
            (save-restriction
              (widen)
              (goto-line line)
	      (save-excursion
		(when col (move-to-column col))
		(setq pos (point)))
              (setq overlay-arrow-string "=>")
              (or overlay-arrow-position
                  (setq overlay-arrow-position (make-marker)))
              (set-marker overlay-arrow-position (point) (current-buffer)))
            (cond ((or (< pos (point-min)) (> pos (point-max)))
                   (widen)
                   (goto-char pos))))
          (set-window-point window pos)))))


;; 
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;
;;  4/ Construction de la commande suivante de mupad et des s�parateurs
;;
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;
;; Apr�s une validation, les commandes tap�es sont stock�es dans la zone 
;; todo et associ�e � un attribut diff�rent. Elles sont �ventuellement
;; compl�t�es par : ou ; 
;; � chaque fois que mupad termine un calcul il prend la commande suivante
;; dans cette zone (ou le commentaire s�parateur) 
;; 
;; 
;; s'arr�te au premier ^///--- rencontr� ou � la fin
;; 
;; �tat 0 [g�n�ral]
;;   (NL) -> 1 [d�but de ligne]     \ -> 2 [�chapement]     " -> 10 [cha�ne]
;;      / -> 3 [division]           * -> 5 [multipli�]  sinon -> 0 [g�n�ral]
;;
;; �tat 10 [chaine]
;;      \ -> 11 [�chap]            " -> 0 [g�n�ral]    sinon -> 10
;;  
;; �tat 11 [echap chaine]                               
;;      n -> 10                    r -> 10             t -> 10
;;      \ -> 10                    " -> 10         sinon -> 60 -> 10 (err.)
;;
;; �tat 2 [�chap] 
;;   (NL) -> 0 [suite de ligne]                    sinon -> 50 -> 0 (err.)
;;
;; �tat 1 [d�but de ligne] 
;;    " -> 10 [cha�ne]             \ -> 2 [�chapement]  
;; (NL) -> 1 [d�but de ligne]      / -> 4 [div. d�b. ligne]  
;;    * -> 5 [multipli�]       sinon -> 0 [g�n�ral]
;;
;; �tat 3 [division]
;;   (NL) -> 1 [d�but de ligne]    \ -> 2 [�chapement]  
;;      " -> 10 [cha�ne]           / -> 20 [commentaire]     
;;      * -> 80 -> 0           sinon ->  0 [g�n�ral]
;;
;; �tat 5 [multipli�]
;;   (NL) -> 1 [d�but de ligne]    / -> 70 -> 0 (niveau -1)
;;  sinon -> 0
;;
;; �tat 20 [commentaire]
;;   (NL) ->  1 [d�but ligne]  sinon -> 20 [commentaire]
;;
;; �tat 4 [division d�but ligne]
;;   (NL) ->  1 [d�but de ligne]   \ ->  2 [�chapement]
;;      " -> 10 [cha�ne]           / -> 55 [commentaire d�but ligne]
;;      * -> 80 -> 0 [commentaire imbriqu�]             sinon ->  0 [g�n�ral]
;;
;; �tat 55 [commentaire d�but ligne] 
;;   (NL) ->  1 [d�but de ligne]   / -> 22 [presque separateur /// et niveau 0]
;;  sinon -> 20 [g�n�ral]
;;
;; �tat 22 [presque separateur a]
;;   (NL) ->  1 [d�but de ligne]   - -> 23 [presque separateur b]
;;  sinon -> 20 [commentaire]
;;
;; �tat 23 [presque separateur b]
;;   (NL) ->  1 [d�but de ligne]   - -> 24 [presque separateur c]
;;  sinon -> 20 [commentaire]
;;
;; �tat 24 [presque separateur c]
;;   (NL) ->  1 [d�but de ligne]    - -> 25 [s�parateur] 
;;    (message (format "%d" etat)) pour tester l'automate 
;;  sinon -> 20 [commentaire]
;;
;; recherche la premi�re commande entre pos et max
;; 
(defun mupad-run-automate (pos max)
  (let ((etat 1) (niveau 0) (test (< pos max)) (err 0) br br1)
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
            (message "Erreur apr�s \\") 
            (setq err 1)
            (setq etat 0))
          ((= etat 60) 
            (message "Erreur apr�s \\ dans une cha�ne") 
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
;; commentaire ///--- : mupad-run-automate renvoie �tat 25 et le pointeur + 6 
;;                    : sinon zone pos -> pointeur et etat 0/1 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-from-edit-to-todo () 
  (let (br br1 (inhibit-read-only t))
    (mupad-run-debug-message 'edit-to-todo "DEB : edit-to-todo")
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
            (mupad-run-put-face mupad-run-edit (1+ mupad-run-edit) 
              'mupad-run-face-beginning-waiting))
         (when (<= (+ 2 (marker-position mupad-run-edit)) br1)
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
      ;; Reset the undo list to make sure that the user will not
      ;; revert inadvertently the insertion of the todo stuff, as this
      ;; would invalidate the marker positions
      (setq buffer-undo-list nil)
      (mupad-run-debug-message 'edit-to-todo  "FIN : edit-to-todo"))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-from-todo-to-output ()
  (save-excursion
    (let (br br1 br2 br3 (brp (point)) (inhibit-read-only t))
      (mupad-run-debug-message 'todo-to-output "DEB : todo-to-output")
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
          (mupad-run-put-face br1 br2 'mupad-run-face-separator)
          (forward-char 1)
          (set-marker mupad-run-todo (point))
          (if 
            (/= (marker-position mupad-run-todo) 
                (marker-position mupad-run-edit))
            (setq br (get-text-property mupad-run-todo 'item))
            (setq br nil)))
; la premi�re commande de la zone todo
        (mupad-run-print (concat mupad-run-prompt "\n") 
          'mupad-run-face-prompt (point) 'cmd nil)
        (set-marker mupad-run-last-prompt 
          (- (point) (length mupad-run-prompt) 1)))
; mettre � jour l'affichage si envoi d'une commande 
      (when (and br (eq (car br) 'cmd))
        (setq br1 
          (or (next-single-property-change mupad-run-todo 'item) (point-max)))
        (mupad-run-debug-message 'todo-to-output "MIL : todo-to-output")
; NT 04/11/2002: modifications for the debugger 
; en fonction de mupad-run-state
	 (cond
	  ((eq mupad-run-state 'wait-input)	     ; Normal input
; �ventuellement ajouter un " ;" � la fin
            (setq br2 (1- br1))
            (while (memq (char-before br2) '(?\n ?\t ?\ )) (setq br2 (1- br2)))
            (if (not mupad-run-edit) (message "nil"))
            (when 
               (and (> br2 (marker-position mupad-run-todo))
                    (not (memq (char-before br2) '(?\: ?\;))))
               (goto-char br2) 
               (insert mupad-run-input-terminator)
               (setq br1 (+ br1 (length mupad-run-input-terminator))))
            (setq br2 (buffer-substring mupad-run-todo (1- br1)))
            (setq mupad-run-rawcommand (list 1 br2)))
          ((eq mupad-run-state 'wait-debugger-input) ; Debugger input
            (setq br2 (buffer-substring mupad-run-todo (1- br1)))
            (setq mupad-run-rawcommand 
              (mupad-run-from-todo-to-output-debug br2))))
	(when mupad-run-rawcommand
	  (mupad-run-debug-message 'todo-to-output
	     (concat "MuPAD input: ["
               (number-to-string (car mupad-run-rawcommand)) "] "
	       (nth 1 mupad-run-rawcommand)))
	  (process-send-string mupad-run-process
            (concat "\006"
		    (string (car mupad-run-rawcommand))
		    (nth 1 mupad-run-rawcommand)
		    "\007\n"))
          (setq br mupad-run-state)
          (if (= (car mupad-run-rawcommand) 1)
            (setq mupad-run-state 'running)
            (setq mupad-run-state 'run-debug))
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
            (mupad-run-put-face br3 (point) 'mupad-run-face-local-prompt)
            (put-text-property br3 (point) 'rear-nonsticky t)
            (put-text-property br3 (point) 'front-sticky t)
            (put-text-property br3 (point) 'read-only t)
            (forward-line)
            (beginning-of-line))
          (put-text-property brp (point) 'item mupad-run-itemb)
          (setq mupad-run-last-type 'end-cmd))))
          (mupad-run-debug-message 'todo-to-output "MIL : todo-to-output")))

(defun mupad-run-from-todo-to-output-debug (br2)
  (cond 
    ((string= br2 "") mupad-run-rawcommand) ; Reuse previous command
    ((string-match 
       "^\\s-*\\([a-zA-Z?]\\)\\(\\s-+\\(.*[^;:       ]\\)\\)?\\s-*[;:]?$" br2)
      (let ((command (match-string 1 br2)) (args (or (match-string 3 br2) "")))
; assertion: there is no space or semicolon at the end of args
        (cond
; MPRCmdb_disp_list         35 // f -> k : request display list
;	  when should the frontend request this display list ?
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
		;; Why doesn't P work ?
; MPRCmdb_stop_at 68 (deprecated?)
	((string= command "g") (list 69 args)) ; MPRCmdb_goto_proc
; non standard shortcut in the mupad text debugger:
	((string= command "L") (list 70 args)) ; MPRCmdb_proc_level
	(t 
          (delete-region 
            (marker-position mupad-run-todo)
            (+ (marker-position mupad-run-todo) (length br2) 1))
          (message 
            (concat "Undefined debugger command: \"" command "\". Try \"h\"."))
	nil))))
   (t 
     (delete-region 
        (marker-position mupad-run-todo)
        (+ (marker-position mupad-run-todo) (length br2) 1))
     (message (concat "Incorrect debugger command: \"" br2 "\". Try \"h\"."))
     nil)))
;;
;; 
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;
;;  5/ Commandes [RET], [C-RET], [SUP] et [BS] (commentaires s�parateurs)
;;
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;
; [RET] et [C-RET] : 
;   zone �dition                     -> to-todo ou passage � la ligne
;   zone todo                        -> 
;     si dans commentaire : couper le commentaire � cet endroit
;     si dans commandes   : aller � la fin ou au d�but, et inserer commentaire
;   zone r�sultats                   -> 
;     si dans commentaire : couper le commentaire � cet endroit
;     si prompt ou commande          -> recopier la commande � la fin
;     sinon ins�rer � la fin de la commande apr�s le r�sultat
;
; mupad-run-return-position renvoie modifie le tampon en cons�quence.
;
; [C-SUP] dans zone r�sultat sur r�sultat, et compl�tion 
;                 -> cache le r�sultat ou la compl�tion
; [C-INS] r�insere la partie cach�e.
;                
(defun mupad-run-creturn ()
  (interactive)
  (mupad-run-delete-windows "*MuPAD*Completions*")
  (cond 
    ((>= (point) (marker-position mupad-run-edit)) (insert "\n"))
    ((or 
        (memq (mupad-run-get-face (point))
          '(mupad-run-face-local-prompt mupad-run-face-prompt-flag
            mupad-run-face-last-input   
            mupad-run-face-result-last-input))
        (and 
          (eq (mupad-run-get-face (point))  'mupad-run-face-prompt)
          (eq (char-after) ?\n)))
      (mupad-run-copy-cmd))
    (t (mupad-run-insert-comment))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-return ()
  (interactive)
  (mupad-run-debug-message 'input "Return")
  (mupad-run-delete-windows "*MuPAD*Completions*") 
  (cond 
    ((>= (point) (marker-position mupad-run-edit))
      (mupad-run-from-edit-to-todo)
; NT 04/11/2002
      (if (memq mupad-run-state '(wait-input wait-debugger-input)) 
        (mupad-run-from-todo-to-output)))
    ((or 
      (memq (mupad-run-get-face (point))
        '(mupad-run-face-local-prompt 
          mupad-run-face-local-prompt-flag
          mupad-run-face-last-input   
          mupad-run-face-result-last-input))
        (and 
          (eq (mupad-run-get-face (point)) 'mupad-run-face-prompt)
          (eq (char-after) ?\n)))
      (mupad-run-copy-cmd))
    (t (mupad-run-insert-comment))))

;
; [SUP]                                      
;   dans zone �ditable                          -> zone �ditable, effacer
;   dans ///---( )*(RET)                        -> supprimer
;   dans ///--- et ligne pr�c�dente commentaire -> recoller
;   sur (RET) et ligne suivante commentaire     -> recoller
;
(defun mupad-run-suppression () 
  ""
  (interactive)
  (mupad-run-delete-windows "*MuPAD*Completions*")
  (let ((br (point)) (inhibit-read-only t))
    (cond 
; caract�re modifiable 
      ((not (get-char-property (point) 'read-only)) 
        (delete-char 1) (setq br nil))
; dans la zone ///--- d'un commentaire : si commentaire avant -> recoller
     ((and
         (memq (mupad-run-get-face (point))
            '(mupad-run-face-separator-flag mupad-run-face-separator))
         (not (beginning-of-line))
         (<= (- br (point)) 5)
         (> (point) 1)
         (not (backward-char))
         (memq (mupad-run-get-face (point))
            '(mupad-run-face-separator-flag mupad-run-face-separator)))
        (mupad-run-move-flag-down (1+ (point)))
        (delete-region (point) (+ 7 (point)))
        (setq br nil))
; sur (RET) et ligne suivante commentaire     -> recoller
     ((and 
         (goto-char br)
         (eolp)
         (memq (mupad-run-get-face (point))
            '(mupad-run-face-separator-flag 
              mupad-run-face-separator))
         (< (point) (point-max))
         (not (forward-char))
         (memq (mupad-run-get-face (point))
            '(mupad-run-face-separator-flag 
              mupad-run-face-separator)))
       (mupad-run-move-flag-down (point))
       (delete-region (1- (point)) (+ 6 (point)))
       (setq br nil))
; une ligne vide de commentaires
      ((and
           (goto-char br)
           (memq (mupad-run-get-face (point))
             '(mupad-run-face-separator-flag mupad-run-face-separator))
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
  (mupad-run-delete-windows "*MuPAD*Completions*")
  (when (and (> (point) 1) (/= (point) (marker-position mupad-run-edit)))
    (backward-char) (mupad-run-suppression)))
;;
;; 
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;
;; 7/ Traitement des appels syst�mes et affichage graphique
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
;; structure des donn�es doublement chain�e avec un point d'acc�s 
;; interm�diaire, sous forme d'un tableau :
;; coordonn�es g�n�rales
;;   0 = liste directe - 1 = liste inverse - 2 = particulier - 3 = longueur.
;; L'acc�s aux �l�ments de la liste est aussi un tableau dont la valeur des 
;;   coordonn�es est 
;; 0 = valeur - 1 = terme suivant - 2 = terme pr�c�dent
;; primitives : 
;;   head-tail-void cr�e une structure vide
;;   add-head / add-tail ajoute un �l�ment en t�te / en queue
;;   remove-head / tail enl�ve l'�l�ment en t�te / en queue
;;   list-head / list-tail construit la liste � l'endroit / � l'envers
;;   ptr-to-head / ptr-to-tail avance / recule le pointeur courant
;;   struct renvoie la liste et la valeur du pointeur courant
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

(defun ptr-to-tail (A)
  (cond 
    ((not (aref A 2)) (aset A 2 'tail))
    ((eq (aref A 2) 'tail))
    ((eq (aref A 2) 'head) (or (aset A 2 (aref A 0)) (aset A 2 'tail)))
    ((not (aref (aref A 2) 1)) (aset A 2 'tail))
    (t (aset A 2 (aref (aref A 2) 1))))
  A)

(defun ptr-to-head (A)
  (cond 
    ((not (aref A 2)) (aset A 2 'head))
    ((eq (aref A 2) 'head))
    ((eq (aref A 2) 'tail)  (or (aset A 2 (aref A 1)) (aset A 2 'head)))
    ((not (aref (aref A 2) 2)) (aset A 2 'head))
    (t (aset A 2 (aref (aref A 2) 2))))
  A)

(defun struct (A) 
  (cons 
    (list-head A) 
    (if (vectorp (aref A 2)) (aref (aref A 2) 0) (aref A 2))))
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-get-previous-command (str)
  (let ((brt t) brs)
    (when (symbolp (aref mupad-run-hist-commands 2))
      (aset mupad-run-hist-commands 2 'head))
    (ptr-to-tail mupad-run-hist-commands)
    (while 
      (and 
         brt 
         (not (symbolp (aref mupad-run-hist-commands 2)))
         (setq brs (aref (aref mupad-run-hist-commands 2) 0))
; Si les d�buts de chaines sont �gaux alors brt vaut nil
         (setq brt 
           (not 
             (string= str (substring brs 0 (min (length str) (length brs)))))))
      (ptr-to-tail mupad-run-hist-commands))
; renvoie nil si le d�but de la cha�ne n'est pas trouv�, la chaine sinon
    (and (not brt) brs)))
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-get-next-command (str)
  (let ((brt t) brs)
    (when (symbolp (aref mupad-run-hist-commands 2))
      (aset mupad-run-hist-commands 2 'tail))
    (ptr-to-head mupad-run-hist-commands)
    (while 
      (and 
         brt 
         (not (symbolp (aref mupad-run-hist-commands 2)))
         (setq brs (aref (aref mupad-run-hist-commands 2) 0))
         (setq brt 
           (not 
             (string= str (substring brs 0 (min (length str) (length brs)))))))
      (ptr-to-head mupad-run-hist-commands))
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
(defun mupad-run-replace-edit-zone (str pos)
; Replace the text in the edit zone by the string str, and put the cursor 
; at position pos (or at the end of buffer if pos is past the end of buffer)
  (delete-region mupad-run-edit (point-max))
  (goto-char mupad-run-edit)
  (insert str)
  (goto-char (min pos (point-max)))
  (mupad-run-recenter)
  )
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-previous-history-search ()
; Search and display the previous command with the same beginning of line.
  (interactive)
  (mupad-run-delete-windows "*MuPAD*Completions*")
  (when (>= (point) (marker-position mupad-run-edit))
    (let 
      ( (br (buffer-substring mupad-run-edit (point-max))) 
        (brs (buffer-substring mupad-run-edit (point))) 
        (brn (point))
        (brold 
          (if (vectorp (aref mupad-run-hist-commands 2))
            (aref (aref mupad-run-hist-commands 2) 0)))
        br1)
      (unless brold (setq brold brs))
      (setq br1 (mupad-run-get-previous-command brs))
      (unless br1 
        (aset mupad-run-hist-commands 2 'head)
        (mupad-run-error "End of history list")
        (setq br1 brs))
      (unless (or (string= brs br) (string= brold br))
        (mupad-run-store-line br))
      (mupad-run-replace-edit-zone br1 brn))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-previous-history ()
; Display the previous command.
  (interactive)
  (mupad-run-delete-windows "*MuPAD*Completions*")
  (when (>= (point) (marker-position mupad-run-edit))
    (let 
      ( (br (buffer-substring mupad-run-edit (point-max)))
        (brn (point))
        (brold 
          (if (vectorp (aref mupad-run-hist-commands 2))
            (aref (aref mupad-run-hist-commands 2) 0)))
        br1)
      (unless brold (setq brold ""))
      (setq br1 (mupad-run-get-previous-command ""))
      (unless br1 
        (aset mupad-run-hist-commands 2 'head)
        (mupad-run-error "End of history list")
        (setq br1 ""))
      (unless (string= brold br) (mupad-run-store-line br))
      (mupad-run-replace-edit-zone br1 brn))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-next-history-search ()
; Search and display the next command with the same beginning of line.
  (interactive)
  (mupad-run-delete-windows "*MuPAD*Completions*")
  (when (>= (point) (marker-position mupad-run-edit))
    (let 
      ( (br (buffer-substring mupad-run-edit (point-max))) 
        (brs (buffer-substring mupad-run-edit (point)))
	(brn (point))
        (brold 
          (if (vectorp (aref mupad-run-hist-commands 2))
            (aref (aref mupad-run-hist-commands 2) 0)))
        br1)
      (unless brold (setq brold brs))
      (setq br1 (mupad-run-get-next-command brs))
      (unless br1 
        (aset mupad-run-hist-commands 2 'tail)
        (mupad-run-error "End of history list")
        (setq br1 brs))
      (unless (or (string= brs br) (string= brold br))
        (mupad-run-store-line br))
      (mupad-run-replace-edit-zone br1 brn))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-next-history ()
; Display the next command.
  (interactive)
  (mupad-run-delete-windows "*MuPAD*Completions*")
  (when (>= (point) (marker-position mupad-run-edit))
    (let 
      ( (br (buffer-substring mupad-run-edit (point-max)))
        (brn (point))
        (brold 
          (if (vectorp (aref mupad-run-hist-commands 2))
            (aref (aref mupad-run-hist-commands 2) 0)))
        br1)
      (unless brold (setq brold ""))
      (setq br1 (mupad-run-get-next-command ""))
      (unless br1 
        (aset mupad-run-hist-commands 2 'tail)
        (mupad-run-error "End of history list")
        (setq br1 ""))
      (unless (string= brold br) (mupad-run-store-line br))
      (mupad-run-replace-edit-zone br1 brn))))
;; 
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;
;;  9/ Commandes de d�placement et d'insertion / supression de r�sultats
;;
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;
(defun mupad-run-left () 
  (interactive)
  (mupad-run-delete-windows "*MuPAD*Completions*")
  (goto-char (or (previous-single-property-change (point) 'item) (point-min)))
  (beginning-of-line))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-right () 
  (interactive)
  (mupad-run-delete-windows "*MuPAD*Completions*")
  (end-of-line)
  (goto-char (or (next-single-property-change (point) 'item) (point-max))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-hide () 
  (interactive)
  (mupad-run-delete-windows "*MuPAD*Completions*")
  (let (br bra brb (inhibit-read-only t))
    (cond 
      ((and mupad-run-last-prompt (>= (point) mupad-run-last-prompt)) 
        (error "Hiding impossible at this point"))
      ((memq (mupad-run-get-face (point))
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
        (setq br (symbol-name (mupad-run-get-face brb)))
        (when (not (string= (substring br -5) "-flag"))
          (mupad-run-put-face brb (1+ brb) (intern (concat br "-flag"))))
        (delete-region bra brb)
        (recenter)))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mupad-run-show () 
  (interactive)
  (mupad-run-delete-windows "*MuPAD*Completions*")
  (let (br1 br2 (inhibit-read-only t))
    (when (not (eobp)) (forward-char 1) (mupad-run-left))
    (setq br1 (get-text-property (point) 'hide))
    (when br1 
      (put-text-property (point) (1+ (point)) 'hide (cdr br1))
      (insert-before-markers (car br1))
      (setq br2 (symbol-name (mupad-run-get-face (point))))
      (when (not (cdr br1))
        (mupad-run-put-face (point) (1+ (point))
          (intern (substring br2 0 (- (length br2) 5)))))
      (recenter))))

(defun mupad-run-move-flag-up (pt)
  (let ( (inhibit-read-only t) 
         (br (symbol-name (mupad-run-get-face pt))))
    (save-excursion 
      (when (string= (substring br -5) "-flag")
        (mupad-run-put-face pt (1+ pt)
          (intern (substring br 0 (- (length br) 5))))
        (goto-char pt)
        (mupad-run-left)
        (put-text-property (point) (1+ (point))
          'hide (get-text-property pt 'hide))
        (put-text-property pt (1+ pt) 'hide ())
        (setq br (symbol-name (mupad-run-get-face (point))))
        (mupad-run-put-face (point) (1+ (point)) 
          (intern (concat br "-flag")))))))

(defun mupad-run-move-flag-down (pt)
  (let 
    ( br1 (inhibit-read-only t) 
      (br (symbol-name (mupad-run-get-face pt))))
    (save-excursion 
      (when (string= (substring br -5) "-flag")
        (mupad-run-put-face pt (1+ pt)
          (intern (substring br 0 (- (length br) 5))))
        (mupad-run-right)
        (setq br1 (get-text-property (point) 'hide)))
        (put-text-property (point) (1+ (point)) 
          'hide (append (get-text-property pt 'hide) br1))
        (unless br1
           (setq br 
              (symbol-name (mupad-run-get-face (point))))
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
    (mupad-run-recenter)
    (setq br (point-max))
    (while (memq (char-before br) '(?\n ?\t ?\ )) (setq br (1- br)))
    (when 
      (and (> br (marker-position mupad-run-edit))
           (not (memq (char-before br) '(?\: ?\;))))
      (goto-char br)
      (insert mupad-run-input-terminator))
    (goto-char (point-max))
    (if (not (bolp)) (insert "\n"))
    (insert (buffer-substring bra brb))
    (goto-char mupad-run-edit)
    (while (not (eobp))
      (setq bra (mupad-run-get-face (point)))
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

; selectionner le bloc de commandes et le recopier � la fin de la 
; zone d'�dition
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; si zone r�sultat             : � la fin de la zone r�sultat 
;; si zone commande             : � la fin du r�sultat �ventuel,
;;                                avant le prompt suivant
;; si zone completion           : � la fin de la compl�tion
;; si zone prompt               : avant ce prompt
;; si zone commentaire          : avant ce commentaire
;; si dans commentaire          : coupure de commentaire
;; si trop bas                  : en haut de la zone edit
;; si dans la zone todo         : � la commande suivante
;;
(defun mupad-run-insert-comment ()
  (let 
    ((br (point)) (br1 (mupad-run-get-face (point)))
     (inhibit-read-only t) br2)
    (cond 
; dans la zone edit
      ((>= (point) (marker-position mupad-run-edit))
        (goto-char mupad-run-edit) (insert "///---\n") (backward-char 1))
; dans un prompt (sauf fin de ligne) : aller au d�but de la ligne
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
        (memq (mupad-run-get-face (point))
          '(mupad-run-face-prompt mupad-run-face-prompt-flag 
            mupad-run-face-separator mupad-run-face-separator-flag 
            mupad-run-face-completion 
            mupad-run-face-completion-flag)))
         (mupad-run-insert-comment-br (point) ""))
; � la fin d'une ligne de saisie (face = prompt)
      ((memq br1 '(mupad-run-face-prompt mupad-run-face-prompt-flag))
        (mupad-run-right) (mupad-run-insert-comment))
; dans la zone todo sur le caract�re de t�te
      ((memq br1 
           '(mupad-run-face-beginning-waiting 
             mupad-run-face-beginning-rem))
        (mupad-run-insert-comment-br (point) ""))
; dans un commentaire, sur l'un des caract�res de ///---
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
; dans la zone todo (sauf commentaires trait�s ci-dessus)
      (( >= (point) (marker-position mupad-run-todo))
        (mupad-run-right) (mupad-run-insert-comment))
; au milieu d'une zone...
      ((and 
          (not (eq (point) 1))
          (eq 
            (get-text-property (point) 'item)  
            (get-text-property (1- (point)) 'item)))
         (mupad-run-right) (mupad-run-insert-comment))
; sinon (dans la zone de mupad) avancer jursqu'� trouver le bon endroit
      (t
        (goto-char br) 
        (while 
          (memq (mupad-run-get-face (point))
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
  (mupad-run-put-face (- (point) 6) (1+ (point)) 'mupad-run-face-separator)
  (cond 
    ((< (point) mupad-run-todo)
      (setq mupad-run-itemb (1+ mupad-run-itemb))
      (put-text-property (- (point) 6) (1+ (point)) 'item mupad-run-itemb))
    ((< (point) mupad-run-edit)
      (setq mupad-run-itema (1+ mupad-run-itema))
      (put-text-property (- (point) 6) (1+ (point))
        'item (cons 'rem mupad-run-itema))
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
  (let (br br1 br2) 
; curseur dans la zone d'�dition 
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
                  (memq (mupad-run-get-face br1)
                    '(mupad-run-face-prompt mupad-run-face-local-prompt 
                      mupad-run-face-prompt-flag))
                  (not (eq (char-after) ?\n)))
                (delete-char 1)
                (setq br2 (1- br2)))
              (t (setq br1 (1+ br1)) (forward-char)))))
          (set-text-properties br br2 nil))
; curseur dans une zone de commentaire
      ((and 
          (not (mupad-run-get-face (point)))
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

(defun mupad-run-yank-at-click (click arg)
  (interactive "e\nP")
; Give temporary modes such as isearch a chance to turn off.
  (run-hooks 'mouse-leave-buffer-hook)
  (or mouse-yank-at-point (mouse-set-point click))
  (setq this-command 'yank)
  (setq mouse-selection-click-count 0)
  (mupad-run-yank arg))
;;
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;
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
;; We switch to the buffer *MuPAD*Help* and erase its content:
          (set-buffer (get-buffer-create "*MuPAD*Help*"))
          (erase-buffer)
          (message where-it-is)  ;; tell *Messages* which version is used.
          (insert-file where-it-is)
;; Show the help buffer and tell user how to remove help window:
          (mupad-bus-window-manager "*MuPAD*Help*" 'mupad-show-help)
          (setq buffer-read-only t)
          (mupad-info-wind-conf)
          (select-window wind))
;; Tell the user the file was not found:
        (t 
          (mupad-bus-window-manager "*MuPAD*Help*" 'mupad-beginning-temp)
          (insert 
             (concat 
               "The file mupad-run.el-info was not found."
               "You should discover where it is, say in the directory" 
               "/usr/local/share/emacs/site-lisp and add the line\n"
               "(setq load-path "
               "(concat load-path \"/usr/local/share/emacs/sitelisp\"))\n"
            "to your .emacs file (create it if it doesn't already exist)."))
          (setq fill-column 
            (1- (window-width (get-buffer-window "*MuPAD*Help*"))))
          (fill-individual-paragraphs (point-min) (point-max) 'left)
;; Remove help window :
          (setq buffer-read-only t)
          (mupad-bus-window-manager 
            "*MuPAD*Help*" 'mupad-remove-help-old-config)
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
      ["Info about mupad-run" describe-mode
       :active t]
      ["Info about mupad-run (en Fran�ais)" mupad-run-show-mupad-info
       :active t
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

(defun mupad-run-delete-windows (str)
  (let ((br (get-buffer str))) (if br (delete-windows-on br))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility for automatic completion with mupad-run-completion-style = inline
;;
;; Taken from momentary-string-display in subr.el, emacs 21.2
;; Changed the vertical recentering to just display enough
;; of the output instead of moving to the middle of the window

(defun mupad-run-momentary-string-display (string pos &optional exit-char message) 
  "Momentarily display STRING in the buffer at POS.
Display remains until next character is typed.
If the char is EXIT-CHAR (optional third arg, default is SPC) it is swallowed;
otherwise it is then available as input (as a command if nothing else).
Display MESSAGE (optional fourth arg) in the echo area.
If MESSAGE is nil, instructions to type EXIT-CHAR are displayed there."
  (or exit-char (setq exit-char ?\ ))
  (let ((inhibit-read-only t)
	;; Don't modify the undo list at all.
	(buffer-undo-list t)
	(modified (buffer-modified-p))
	(name buffer-file-name)
	insert-end)
    (unwind-protect
	(progn
	  (save-excursion
	    (goto-char pos)
	    ;; defeat file locking... don't try this at home, kids!
	    (setq buffer-file-name nil)
	    ;(insert-before-markers string)
	    (insert string)
	    (setq insert-end (point))
	    (cond
	     ;; If the message does not fit in the window height,
	     ;; scroll so that the top of the message is on the second
	     ;; line of the window
	     ;((> (count-lines pos insert-end) (window-height))
	     ; (goto-char pos)
	     ; (recenter 1))
	     ;; Otherwise, if the message ends below the last line of
	     ;; the window, scroll up so that the bottom of the
	     ;; message is on that last line
	     ((>= (count-screen-lines (window-start) insert-end)
		  (window-height))
	      (recenter -1))))
	  (message (or message "Type %s to continue editing.")
		   (single-key-description exit-char))
	  (let ((char (read-event)))
	    (or (eq char exit-char)
		(setq unread-command-events (list char)))))
      (if insert-end
	  (save-excursion
	    (delete-region pos insert-end)))
      (setq buffer-file-name name)
      (set-buffer-modified-p modified))))


;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;;=-= FIN du fichier mupad-run.el =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
