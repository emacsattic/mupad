;; file mupad-bus.el
;;      exchange of stuff between mupad-script and mupad-run
;;

(provide 'mupad-bus)

(defsubst name-extension (filename)
  "Returns the extension suffix of filename, if any."
  (if (> (length filename) (length (file-name-sans-extension filename)))
      (substring filename (1+ (length (file-name-sans-extension filename))))
      ""))

(defmacro mupad-pgrmp (abuffer)
  "Returns t if buffer abuffer has a name with a .mupad extension suffix."
  (` (string= (name-extension (buffer-name (, abuffer))) "mu")))

(defun mupad-possible-file-name nil
  "Try to guess the name of a likely mupad-program"
  ;; First tries the existing windows, then the existing buffers.
  (let ((pgrm nil))
       (walk-windows
         (lambda (wind)
          (if (mupad-pgrmp (window-buffer wind))
              (push (buffer-name (window-buffer wind)) pgrm))))
       (if pgrm (car pgrm) ;; Return value if a window is displaying
                           ;; a candidate mupad-program.
           (mapcar
             (lambda (abuffer)
              (if (mupad-pgrmp abuffer)
                  (push (buffer-name abuffer) pgrm)))
             (buffer-list))
           (if pgrm (car pgrm) ;; Return value if a buffer is
                               ;; candidate mupad-program.
                    nil        ;; Return value if fail.
   ))))


(defun mupad-bus-visible-command (acommand)
  (condition-case err
      (progn
        (mupad-bus-switch-to-mupad)
	(let ((pt (point)) (ref (marker-position mupad-run-edit))
	      (old (buffer-substring-no-properties
		    mupad-run-edit (goto-char (point-max)))))
	  ;; erase what was there:
	  (delete-region mupad-run-edit (point-max))
	  ;; do your stuff:
	  (insert acommand) ;(print (list "My Visible Command: " acommand))
	  (mupad-run-return) ; comes back at the end of the stuff.
	  ;; reput what was there:
	  (insert old)
	  ;; replace cursor properly:
	  (if (< pt ref)
	      (goto-char pt)
	    (goto-char (+ (marker-position mupad-run-edit) (- pt ref))))
	  ))
    (error (princ err) nil)))

(defun mupad-bus-switch-to-mupad nil
  (mupad-bus-window-manager "*MuPAD*" 'mupad-beginning)
  (unless mupad-run-process (mupad-run-mode)))

(defun mupad-bus-start nil
  (interactive)
  (condition-case err
      (mupad-bus-switch-to-mupad)
    (error (princ "Could not start MuPAD: ")(princ err) nil)))

(defun mupad-bus-region (beg end)
  "Run MuPAD on the current region.  A temporary file (zap-file) is
written in directory mupad-temp-directory, but MuPAD is run in the current
directory."
  (interactive "r")
  (save-excursion
    (save-restriction
      (widen)
      (write-region beg end zap-file nil nil)))
    (mupad-bus-switch-to-mupad)
    (mupad-bus-visible-command (concat "read(\"" zap-file "\"):")))

(defun mupad-bus-read-input (prompt default sep flag)
  " If flag is non-nil, reads string (if string is \"\" uses default).
    Else, if flag is nil, set string to default.
    If resulting string is not \"\" prepends sep.
    As a special case, if string is \" \", return \"\"."

  (let ((string
    (if flag
;; If flag is non-nil prompt for input from mini-buffer.
      (read-input
        (concat prompt " (Default "default") "))
;; Else use the default string.
        default)))

    (if (equal string "")
      (if (equal default "")
         ""                     ;; If string and default both "":
         (concat sep default))  ;; If string "" and default is non empty:
      (if (equal string " ")
        ""                      ;; If string is a space:
        (concat sep string))))) ;; If string is non empty:

(defun mupad-bus-send-file (closing-statement)
  "Run MuPAD on a file. If CLOSING-STATEMENT is \":\" then `read' will be used
with a final `:' which means errors won't be displayed. Alternatively,
CLOSING-STATEMENT can be \";\". See `mupad-region' for more information."
  (let  ((mupad-pgrm (mupad-bus-read-input "Name of the MUPAD programm : "
                                 (mupad-possible-file-name) "" t)))
        (if (get-buffer mupad-pgrm)
            (save-excursion
              (set-buffer mupad-pgrm)
              (if (buffer-modified-p) (save-buffer))
                 ;; input is *not* on file: input is "read(file);" !!
              (mupad-reads-mupad-loaded-libraries)
              ;; In case 'mupad-input-filter modified the buffer:
              (setq mupad-pgrm (buffer-file-name))
              (if (buffer-modified-p) (save-buffer 0))))

        (mupad-bus-visible-command
           (concat "read(\"" mupad-pgrm "\")" closing-statement))))

(defun mupad-bus-file nil
  (interactive)
  (mupad-bus-send-file ":"))

(defun mupad-bus-execute-file nil
  (interactive)
  (mupad-bus-send-file ";"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Control !
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mupad-bus-set-digits nil
  (interactive)
  (mupad-bus-visible-command (concat "DIGITS:=" (read-string "New Value: ") ":")))

(defun mupad-bus-adapt-textwidth nil
  (interactive)
  (mupad-bus-visible-command
    (concat "TEXTWIDTH:=" (number-to-string (1- (window-width))) ":")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mupad-bus-window-manager (my-buffer-name option)
"Takes care of the windows in mupad-mode and mupad-run-mode.
Displays the buffer MY-BUFFER-NAME in a proper window.
The variable OPTION is
  -- mupad-beginning when we handle the beginning of a procedure. If a buffer
                  already exists with this name, do nothing.
  -- mupad-beginning-temp when we handle the beginning of a procedure. If a
                       buffer already exists with this name, store it.
  -- mupad-remove-help-now to remove help-window,
  -- mupad-remove-help-old-config to wait and remove help-window without
                               touching to the other windows.
  -- mupad-remove-help-now-old-config to remove help-window without
                               touching to the other windows.
  -- mupad-show-help.
  -- nil when it is the end of a call.
The variable MY-BUFFER-NAME is one of
\"*MuPAD*\"  \"*MuPAD Help*\". "

  (cond ((and (string= my-buffer-name "*MuPAD*")
              (eq option 'mupad-beginning)
              (get-buffer-window "*MuPAD*"))
         ;; We go to *MuPAD* and a window already exists with this buffer.
         (select-window (get-buffer-window "*MuPAD*")))
       
        ((and (string= my-buffer-name "*MuPAD*")
              (eq option 'mupad-beginning)
              (not (get-buffer-window "*MuPAD*")))
         ;; We go to *MuPAD* and a window doesn't exist with this buffer.
         (if (= (count-windows) 1)
             ;; If there is only one window containing anything but scratch,
             ;; split the window in 2, else use this window:
             (progn (if (not (string= (buffer-name) "*scratch*"));(mupad-pgrmp (window-buffer))
                        (select-window (split-window-vertically)))
                    (switch-to-buffer "*MuPAD*"))
             ;; At least two windows exist. Do not create another one
             ;; and first try to use the help window, else the
             ;; starting window.
             (mupad-store-wind-conf)
             (cond ((get-buffer-window "*MuPAD Help*")
                    (select-window (get-buffer-window "*MuPAD Help*"))
                    (switch-to-buffer "*MuPAD*"))
                   (t (switch-to-buffer-other-window "*MuPAD*")))))

        ((and (string= my-buffer-name "*MuPAD*")
              (not option)
              (get-buffer "*MuPAD*"))
         ;; We want to exit from *MuPAD*.
         (if (> (count-windows) 1)
             (delete-windows-on "*MuPAD*")
             ;; Else only one window.
             (if (string= (buffer-name (window-buffer)) "*MuPAD*")
                 ;; This only window displays "*MuPAD*"
                 (let ((next-buffer (mupad-possible-file-name)))
                      (if next-buffer (switch-to-buffer next-buffer)
                          ;; Else, don't know what to do !
                          (mupad-restore-wind-conf)
                          ))))
         (with-current-buffer (get-buffer "*MuPAD*")
           (let ((inhibit-read-only t))
             (remove-text-properties (point-min) (point-max) '(read-only nil))))
         (kill-buffer "*MuPAD*"))

        ((and (get-buffer my-buffer-name)
              (string= my-buffer-name "*MuPAD Help*")
              (eq option 'mupad-remove-help-now))
         ;; A buffer displaying "*MuPAD Help*" exists.
         ;; We want to remove the message.
	 ;(let ((inhibit-read-only t)) (kill-buffer my-buffer-name))
         (let ((buffer-to-select ""))
           (save-excursion
           (let ((abufferlist (buffer-list)))
                (while (and (string= buffer-to-select "") abufferlist)
                  (set-buffer (car abufferlist))
                  (if (memq major-mode '(mupad-run-mode mupad-mode))
                      (setq buffer-to-select (buffer-name)))
                  (setq abufferlist (cdr abufferlist)))))
         (if (string= buffer-to-select "") nil
             (switch-to-buffer buffer-to-select)))
         (mupad-restore-wind-conf))

        ((and (get-buffer my-buffer-name)
              (string= my-buffer-name "*MuPAD Help*")
              (memq option '(mupad-remove-help-old-config
                             mupad-remove-help-now-old-config)))
         ;; A buffer displaying "*MuPAD Help*" exists.
         ;; We want to remove the message without touching
         ;; to the window-configuration.
         (cond ((eq option 'mupad-remove-help-old-config)
                (message "PRESS ANY KEY TO CONTINUE.")
                (read-event)))
         (let ((inhibit-read-only t)) (kill-buffer my-buffer-name)))

        ((and (string= my-buffer-name "*MuPAD Help*")
              (memq option '(mupad-beginning mupad-show-help))
              (get-buffer-window "*MuPAD Help*"))
         ;; We go to *MuPAD Help* and a window already exists with this buffer.
         (select-window (get-buffer-window "*MuPAD Help*"))
         (or (eq option 'mupad-show-help) (erase-buffer)))

        ((and (string= my-buffer-name "*MuPAD Help*")
              (eq option 'mupad-beginning-temp)
              (get-buffer-window "*MuPAD Help*"))
         ;; We go temporarily to *MuPAD Help* and a window already exists with
         ;; this buffer.
         (mupad-store-wind-conf)
         (select-window (get-buffer-window "*MuPAD Help*"))
         (erase-buffer))

        ((and (string= my-buffer-name "*MuPAD Help*")
              (memq option '(mupad-beginning mupad-beginning-temp mupad-show-help))
              (not (get-buffer-window "*MuPAD Help*")))
         ;; We go to *MuPAD Help* and a window doesn't exist with this buffer.
         (mupad-store-wind-conf)
         (if (= (count-windows) 1)
             (progn (select-window (split-window-vertically))
                    (switch-to-buffer "*MuPAD Help*"))
             (cond ((and (get-buffer-window "*MuPAD*")
                     (not (eq (get-buffer-window "*MuPAD*") (selected-window))))
                    (select-window (get-buffer-window "*MuPAD*"))
                    (switch-to-buffer "*MuPAD Help*"))
                   (t (switch-to-buffer-other-window "*MuPAD Help*"))))
         (or (eq option 'mupad-show-help) (erase-buffer)))
        ))  ;; end of 'mupad-bus-window-manager

;;----------------------------------------------------------------------------------

(defconst mupad-max-nb-wind-conf 20
 "Maximal number of saved window configurations.")

(defvar mupad-registers-list nil
"List of registers [0-mupad-max-nb-wind-conf] where window-configurations
are stored. See `mupad-store-wind-conf' and `mupad-restore-wind-conf'.")

(defun mupad-info-wind-conf nil
  (message "M-o or ESC-o will remove the help/completion window"))

(defun mupad-depile-wind-conf nil
  (setq mupad-registers-list (cdr mupad-registers-list)))

(defun mupad-backward-wind-conf nil
  "Restore previously stored window configuration."
 (if (not (equal mupad-registers-list nil))
      (progn
        (jump-to-register (car mupad-registers-list))
        (setq mupad-registers-list (cdr mupad-registers-list)))))

(defun mupad-skim-list (alist abound)
  "If the length of ALIST is <= than ABOUND, does nothing,
else replaces ALIST by the same list minus its last element."
  (or (< (length (eval alist)) abound)
      (set alist (nreverse (cdr (nreverse (eval alist)))))))

(defun mupad-store-wind-conf nil
  "Adds a the current window configuration to the pile. If the pile
has more than  mupad-max-nb-wind-conf items [numbered
(0,1,...,(1- mupad-max-nb-wind-conf))] then the first item is lost."
  (mupad-skim-list 'mupad-registers-list mupad-max-nb-wind-conf)
  (let ((next (if (equal mupad-registers-list nil) 0
                 (if (= (car mupad-registers-list)
                        (1- mupad-max-nb-wind-conf))
                  0 (1+ (car mupad-registers-list))))))
       (window-configuration-to-register next)
       (setq mupad-registers-list (cons next mupad-registers-list))))

(defun mupad-restore-wind-conf (&optional arg)
  "Restores the previous window-configuration, killing the *MuPAD Help* buffer
if it was and is no more displayed. When called with prefix ^U, just now
does nothing !"
  (interactive "P")
  (condition-case err
      (if (and arg (= (car arg) 4)) ;; Meaning that the call has been C-u-M-o
          nil
        (let ((had-help-windowp (and (get-buffer "*MuPAD Help*")
                                     (get-buffer-window "*MuPAD Help*"))))
          (mupad-backward-wind-conf)
          ;; Kill the buffer *MuPAD Help* if it is not displayed anymore:
          (if had-help-windowp
              (if (not (get-buffer-window "*MuPAD Help*"))
                  (let ((inhibit-read-only t)) (kill-buffer "*MuPAD Help*")))))
        ;; When called from menu-bar, write nothing in the minibuffer:
        (message ""))
    (error (princ err) nil)))

;;-------------------------------------------------------------------------------
