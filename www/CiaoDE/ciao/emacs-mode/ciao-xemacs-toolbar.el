
(provide 'ciao-xemacs-toolbar)

(defvar ciao-xemacs-toolbar-specifier nil)

(defvar ciao-xemacs-toolbar-next-button
  [ciao-xemacs-toolbar-next-icon
   ciao-xemacs-toolbar-next-command
   (ciao-xemacs-toolbar-any-messages-p)
   "Go to the next message.\n
The command `ciao-xemacs-toolbar-next-command' is run, which is normally
fbound to `ciao-xemacs-next-message'.
You can make this button run some other command by using a Lisp
s-expression like this one in your .vm file:
   (fset 'ciao-xemacs-toolbar-next-command 'some-other-command)"])
(defvar ciao-xemacs-toolbar-next-icon nil)
(or (fboundp 'ciao-xemacs-toolbar-next-command)
    (fset 'ciao-xemacs-toolbar-next-command 'ciao-xemacs-next-message))

(defvar ciao-xemacs-toolbar-previous-button
  [ciao-xemacs-toolbar-previous-icon
   ciao-xemacs-toolbar-previous-command
   (ciao-xemacs-toolbar-any-messages-p)
   "Go to the previous message.\n
The command `ciao-xemacs-toolbar-previous-command' is run, which is normally
fbound to `ciao-xemacs-previous-message'.
You can make this button run some other command by using a Lisp
s-expression like this one in your .vm file:
   (fset 'ciao-xemacs-toolbar-previous-command 'some-other-command)"])
(defvar ciao-xemacs-toolbar-previous-icon nil)
(or (fboundp 'ciao-xemacs-toolbar-previous-command)
    (fset 'ciao-xemacs-toolbar-previous-command 'ciao-xemacs-previous-message))

(defvar ciao-xemacs-toolbar-autofile-button
  [ciao-xemacs-toolbar-autofile-icon
   ciao-xemacs-toolbar-autofile-message
   (ciao-xemacs-toolbar-can-autofile-p)
  "Save the current message to a folder selected using ciao-xemacs-auto-folder-alist."])
(defvar ciao-xemacs-toolbar-autofile-icon nil)

(defvar ciao-xemacs-toolbar-file-button
  [ciao-xemacs-toolbar-file-icon ciao-xemacs-toolbar-file-command (ciao-xemacs-toolbar-any-messages-p)
   "Save the current message to a folder.\n
The command `ciao-xemacs-toolbar-file-command' is run, which is normally
fbound to `ciao-xemacs-save-message'.
You can make this button run some other command by using a Lisp
s-expression like this one in your .vm file:
   (fset 'ciao-xemacs-toolbar-file-command 'some-other-command)"])
(defvar ciao-xemacs-toolbar-file-icon nil)
(or (fboundp 'ciao-xemacs-toolbar-file-command)
    (fset 'ciao-xemacs-toolbar-file-command 'ciao-xemacs-save-message))

(defvar ciao-xemacs-toolbar-getmail-button
  [ciao-xemacs-toolbar-getmail-icon ciao-xemacs-toolbar-getmail-command
   (ciao-xemacs-toolbar-mail-waiting-p)
   "Retrieve spooled mail for the current folder.\n
The command `ciao-xemacs-toolbar-getmail-command' is run, which is normally
fbound to `ciao-xemacs-get-new-mail'.
You can make this button run some other command by using a Lisp
s-expression like this one in your .vm file:
   (fset 'ciao-xemacs-toolbar-getmail-command 'some-other-command)"])
(defvar ciao-xemacs-toolbar-getmail-icon nil)
(or (fboundp 'ciao-xemacs-toolbar-getmail-command)
    (fset 'ciao-xemacs-toolbar-getmail-command 'ciao-xemacs-get-new-mail))

(defvar ciao-xemacs-toolbar-print-button
  [ciao-xemacs-toolbar-print-icon
   ciao-xemacs-toolbar-print-command
   (ciao-xemacs-toolbar-any-messages-p)
   "Print the current message.\n
The command `ciao-xemacs-toolbar-print-command' is run, which is normally
fbound to `ciao-xemacs-print-message'.
You can make this button run some other command by using a Lisp
s-expression like this one in your .vm file:
   (fset 'ciao-xemacs-toolbar-print-command 'some-other-command)"])
(defvar ciao-xemacs-toolbar-print-icon nil)
(or (fboundp 'ciao-xemacs-toolbar-print-command)
    (fset 'ciao-xemacs-toolbar-print-command 'ciao-xemacs-print-message))

(defvar ciao-xemacs-toolbar-visit-button
  [ciao-xemacs-toolbar-visit-icon ciao-xemacs-toolbar-visit-command t
   "Visit a different folder.\n
The command `ciao-xemacs-toolbar-visit-command' is run, which is normally
fbound to `ciao-xemacs-visit-folder'.
You can make this button run some other command by using a Lisp
s-expression like this one in your .vm file:
   (fset 'ciao-xemacs-toolbar-visit-command 'some-other-command)"])
(defvar ciao-xemacs-toolbar-visit-icon nil)
(or (fboundp 'ciao-xemacs-toolbar-visit-command)
    (fset 'ciao-xemacs-toolbar-visit-command 'ciao-xemacs-visit-folder))

(defvar ciao-xemacs-toolbar-reply-button
  [ciao-xemacs-toolbar-reply-icon
   ciao-xemacs-toolbar-reply-command
   (ciao-xemacs-toolbar-any-messages-p)
   "Reply to the current message.\n
The command `ciao-xemacs-toolbar-reply-command' is run, which is normally
fbound to `ciao-xemacs-followup-include-text'.
You can make this button run some other command by using a Lisp
s-expression like this one in your .vm file:
   (fset 'ciao-xemacs-toolbar-reply-command 'some-other-command)"])
(defvar ciao-xemacs-toolbar-reply-icon nil)
(or (fboundp 'ciao-xemacs-toolbar-reply-command)
    (fset 'ciao-xemacs-toolbar-reply-command 'ciao-xemacs-followup-include-text))

(defvar ciao-xemacs-toolbar-compose-button
  [ciao-xemacs-toolbar-compose-icon ciao-xemacs-toolbar-compose-command t
   "Compose a new message.\n
The command `ciao-xemacs-toolbar-compose-command' is run, which is normally
fbound to `ciao-xemacs-mail'.
You can make this button run some other command by using a Lisp
s-expression like this one in your .vm file:
   (fset 'ciao-xemacs-toolbar-compose-command 'some-other-command)"])
(defvar ciao-xemacs-toolbar-compose-icon nil)
(or (fboundp 'ciao-xemacs-toolbar-compose-command)
    (fset 'ciao-xemacs-toolbar-compose-command 'ciao-xemacs-mail))

(defvar ciao-xemacs-toolbar-decode-mime-button
  [ciao-xemacs-toolbar-decode-mime-icon ciao-xemacs-toolbar-decode-mime-command
   (ciao-xemacs-toolbar-can-decode-mime-p)
   "Decode the MIME objects in the current message.\n
The objects might be displayed immediately, or buttons might be
displayed that you need to click on to view the object.  See the
documentation for the variables ciao-xemacs-mime-internal-content-types
and ciao-xemacs-mime-external-content-types-alist to see how to control
whether you see buttons or objects.\n
The command `ciao-xemacs-toolbar-decode-mime-command' is run, which is normally
fbound to `ciao-xemacs-decode-mime-messages'.
You can make this button run some other command by using a Lisp
s-expression like this one in your .vm file:
   (fset 'ciao-xemacs-toolbar-decode-mime-command 'some-other-command)"])
(defvar ciao-xemacs-toolbar-decode-mime-icon nil)
(or (fboundp 'ciao-xemacs-toolbar-decode-mime-command)
    (fset 'ciao-xemacs-toolbar-decode-mime-command 'ciao-xemacs-decode-mime-message))

;; The values of these two are used by the FSF Emacs toolbar
;; code.  The values don't matter as long as they are different
;; (as compared with eq).  Under XEmacs these values are ignored
;; and overwritten.
(defvar ciao-xemacs-toolbar-delete-icon t)
(defvar ciao-xemacs-toolbar-undelete-icon nil)

(defvar ciao-xemacs-toolbar-delete/undelete-button
  [ciao-xemacs-toolbar-delete/undelete-icon
   ciao-xemacs-toolbar-delete/undelete-message
   (ciao-xemacs-toolbar-any-messages-p)
   "Delete the current message, or undelete it if it is already deleted."])
(defvar ciao-xemacs-toolbar-delete/undelete-icon nil)
(make-variable-buffer-local 'ciao-xemacs-toolbar-delete/undelete-icon)

(defvar ciao-xemacs-toolbar-help-icon nil)

(defvar ciao-xemacs-toolbar-recover-icon nil)

(defvar ciao-xemacs-toolbar-helper-icon nil)
(make-variable-buffer-local 'ciao-xemacs-toolbar-helper-icon)

(defvar ciao-xemacs-toolbar-help-button
  [ciao-xemacs-toolbar-helper-icon ciao-xemacs-toolbar-helper-command
   (ciao-xemacs-toolbar-can-help-p)
   "Don't Panic.\n
VM uses this button to offer help if you're in trouble.
Under normal circumstances, this button runs `ciao-xemacs-help'.
If the current folder looks out-of-date relative to its auto-save
file then this button will run `recover-file'
If there is mail waiting in one of the spool files associated
with the current folder, and the `getmail' button is not on the
toolbar, this button will run `ciao-xemacs-get-new-mail'.
If the current message needs to be MIME decoded then this button
will run 'ciao-xemacs-decode-mime-message'."])

(defvar ciao-xemacs-toolbar-helper-command nil)
(make-variable-buffer-local 'ciao-xemacs-toolbar-helper-command)

(defun ciao-xemacs-toolbar-helper-command ()
  (interactive)
  (setq this-command ciao-xemacs-toolbar-helper-command)
  (call-interactively ciao-xemacs-toolbar-helper-command))

(defvar ciao-xemacs-toolbar-quit-button
  [ciao-xemacs-toolbar-quit-icon ciao-xemacs-toolbar-quit-command
   (ciao-xemacs-toolbar-can-quit-p)
   "Quit visiting this folder.\n
The command `ciao-xemacs-toolbar-quit-command' is run, which is normally
fbound to `ciao-xemacs-quit'.
You can make this button run some other command by using a Lisp
s-expression like this one in your .vm file:
   (fset 'ciao-xemacs-toolbar-quit-command 'some-other-command)"])
(defvar ciao-xemacs-toolbar-quit-icon nil)
(or (fboundp 'ciao-xemacs-toolbar-quit-command)
    (fset 'ciao-xemacs-toolbar-quit-command 'ciao-xemacs-quit))

(defun ciao-xemacs-toolbar-any-messages-p ()
  (condition-case nil
      (save-excursion
	(ciao-xemacs-check-for-killed-folder)
	(ciao-xemacs-select-folder-buffer-if-possible)
	ciao-xemacs-message-list)
    (error nil)))

(defun ciao-xemacs-toolbar-delete/undelete-message (&optional prefix-arg)
  (interactive "P")
  (ciao-xemacs-follow-summary-cursor)
  (ciao-xemacs-select-folder-buffer)
  (ciao-xemacs-check-for-killed-summary)
  (ciao-xemacs-error-if-folder-read-only)
  (ciao-xemacs-error-if-folder-empty)
  (let ((current-prefix-arg prefix-arg))
    (if (ciao-xemacs-deleted-flag (car ciao-xemacs-message-pointer))
	(call-interactively 'ciao-xemacs-undelete-message)
      (call-interactively 'ciao-xemacs-delete-message))))

(defun ciao-xemacs-toolbar-can-autofile-p ()
  (interactive)
  (condition-case nil
      (save-excursion
	(ciao-xemacs-check-for-killed-folder)
	(ciao-xemacs-select-folder-buffer-if-possible)
	(and ciao-xemacs-message-pointer
	     (ciao-xemacs-auto-select-folder ciao-xemacs-message-pointer ciao-xemacs-auto-folder-alist)))
    (error nil)))

(defun ciao-xemacs-toolbar-autofile-message ()
  (interactive)
  (ciao-xemacs-follow-summary-cursor)
  (ciao-xemacs-select-folder-buffer)
  (ciao-xemacs-check-for-killed-summary)
  (ciao-xemacs-error-if-folder-read-only)
  (ciao-xemacs-error-if-folder-empty)
  (let ((file (ciao-xemacs-auto-select-folder ciao-xemacs-message-pointer ciao-xemacs-auto-folder-alist)))
    (if file
	(progn
	  (ciao-xemacs-save-message file 1)
	  (message "Message saved to %s" file))
      (error "No match for message in ciao-xemacs-auto-folder-alist."))))

(defun ciao-xemacs-toolbar-can-recover-p ()
  (condition-case nil
      (save-excursion
	(ciao-xemacs-select-folder-buffer)
	(and ciao-xemacs-folder-read-only
	     buffer-file-name
	     buffer-auto-save-file-name
	     (null (buffer-modified-p))
	     (file-newer-than-file-p
	      buffer-auto-save-file-name
	      buffer-file-name)))
    (error nil)))

(defun ciao-xemacs-toolbar-can-decode-mime-p ()
  (condition-case nil
      (save-excursion
	(ciao-xemacs-select-folder-buffer)
	(and
	 ciao-xemacs-display-using-mime
	 ciao-xemacs-message-pointer
	 ciao-xemacs-presentation-buffer
	 (not (ciao-xemacs-mime-plain-message-p (car ciao-xemacs-message-pointer)))))
    (error nil)))

(defun ciao-xemacs-toolbar-can-quit-p ()
  (condition-case nil
      (save-excursion
	(ciao-xemacs-select-folder-buffer)
	(memq major-mode '(ciao-xemacs-mode ciao-xemacs-virtual-mode)))
    (error nil)))

(defun ciao-xemacs-toolbar-mail-waiting-p ()
  (condition-case nil
      (save-excursion
	(ciao-xemacs-select-folder-buffer)
	(or (not (natnump ciao-xemacs-mail-check-interval))
	    ciao-xemacs-spooled-mail-waiting))
    (error nil)))

(fset 'ciao-xemacs-toolbar-can-help-p 'ciao-xemacs-toolbar-can-quit-p)

(defun ciao-xemacs-toolbar-update-toolbar ()
  (if (and ciao-xemacs-message-pointer (ciao-xemacs-deleted-flag (car ciao-xemacs-message-pointer)))
      (setq ciao-xemacs-toolbar-delete/undelete-icon ciao-xemacs-toolbar-undelete-icon)
    (setq ciao-xemacs-toolbar-delete/undelete-icon ciao-xemacs-toolbar-delete-icon))
  (cond ((ciao-xemacs-toolbar-can-recover-p)
	 (setq ciao-xemacs-toolbar-helper-command 'recover-file
	       ciao-xemacs-toolbar-helper-icon ciao-xemacs-toolbar-recover-icon))
	((and (ciao-xemacs-toolbar-mail-waiting-p)
	      (not (memq 'getmail ciao-xemacs-use-toolbar)))
	 (setq ciao-xemacs-toolbar-helper-command 'ciao-xemacs-get-new-mail
	       ciao-xemacs-toolbar-helper-icon ciao-xemacs-toolbar-getmail-icon))
	((and (ciao-xemacs-toolbar-can-decode-mime-p) (not ciao-xemacs-mime-decoded)
	      (not (memq 'mime ciao-xemacs-use-toolbar)))
	 (setq ciao-xemacs-toolbar-helper-command 'ciao-xemacs-decode-mime-message
	       ciao-xemacs-toolbar-helper-icon ciao-xemacs-toolbar-decode-mime-icon))
	(t
	 (setq ciao-xemacs-toolbar-helper-command 'ciao-xemacs-help
	       ciao-xemacs-toolbar-helper-icon ciao-xemacs-toolbar-help-icon)))
  (if ciao-xemacs-summary-buffer
      (ciao-xemacs-copy-local-variables ciao-xemacs-summary-buffer
			       'ciao-xemacs-toolbar-delete/undelete-icon
			       'ciao-xemacs-toolbar-helper-command
			       'ciao-xemacs-toolbar-helper-icon))
  (if ciao-xemacs-presentation-buffer
      (ciao-xemacs-copy-local-variables ciao-xemacs-presentation-buffer
			       'ciao-xemacs-toolbar-delete/undelete-icon
			       'ciao-xemacs-toolbar-helper-command
			       'ciao-xemacs-toolbar-helper-icon))
  (and ciao-xemacs-toolbar-specifier
       (progn
	 (set-specifier ciao-xemacs-toolbar-specifier 
			(cons (current-buffer) nil))
	 (set-specifier ciao-xemacs-toolbar-specifier 
			(cons (current-buffer) ciao-xemacs-toolbar)))))

(defun ciao-xemacs-toolbar-install-or-uninstall-toolbar ()
  (and (ciao-xemacs-toolbar-support-possible-p) ciao-xemacs-use-toolbar
       (ciao-xemacs-toolbar-install-toolbar))
  (if (and ciao-xemacs-fsfemacs-p (not ciao-xemacs-use-toolbar))
      (ciao-xemacs-toolbar-fsfemacs-uninstall-toolbar)))

(defun ciao-xemacs-toolbar-install-toolbar ()
  ;; drag these in now instead of waiting for them to be
  ;; autoloaded.  the "loading..." messages could come at a bad
  ;; moment and wipe an important echo area message, like "Auto
  ;; save file is newer..."
;;  (require 'ciao-xemacs-save)
;;  (require 'ciao-xemacs-summary)
;;  (if ciao-xemacs-fsfemacs-p
;;      (if (not ciao-xemacs-fsfemacs-toolbar-installed-p)
;;	  (ciao-xemacs-toolbar-fsfemacs-install-toolbar))
    (if (not (and (stringp ciao-xemacs-toolbar-pixmap-directory)
		  (file-directory-p ciao-xemacs-toolbar-pixmap-directory)))
	(progn
	  (message "Bad toolbar pixmap directory, can't setup toolbar.")
	  (sit-for 2))
      (ciao-xemacs-toolbar-initialize)
      (let ((height (+ 4 (glyph-height (car ciao-xemacs-toolbar-help-icon))))
	    (width (+ 4 (glyph-width (car ciao-xemacs-toolbar-help-icon))))
	    (frame (selected-frame))
	    (buffer (current-buffer))
	    (tag-set '(win))
	    (myframe (ciao-xemacs-created-this-frame-p))
	    toolbar )
	;; glyph-width and glyph-height return 0 at startup sometimes
	;; use reasonable values if they fail.
	(if (= width 4)
	    (setq width 68))
	(if (= height 4)
	    (setq height 46))
	;; honor user setting of ciao-xemacs-toolbar if they are daring enough
	;; to set it.
	(if ciao-xemacs-toolbar
	    (setq toolbar ciao-xemacs-toolbar)
	  (setq toolbar (ciao-xemacs-toolbar-make-toolbar-spec)
		ciao-xemacs-toolbar toolbar))
	(cond ((eq ciao-xemacs-toolbar-orientation 'right)
	       (setq ciao-xemacs-toolbar-specifier right-toolbar)
	       (if myframe
		   (set-specifier right-toolbar toolbar frame tag-set))
	       (set-specifier right-toolbar toolbar buffer)
	       (set-specifier right-toolbar-width width frame tag-set))
	      ((eq ciao-xemacs-toolbar-orientation 'left)
	       (setq ciao-xemacs-toolbar-specifier left-toolbar)
	       (if myframe
		   (set-specifier left-toolbar toolbar frame tag-set))
	       (set-specifier left-toolbar toolbar buffer)
	       (set-specifier left-toolbar-width width frame tag-set))
	      ((eq ciao-xemacs-toolbar-orientation 'bottom)
	       (setq ciao-xemacs-toolbar-specifier bottom-toolbar)
	       (if myframe
		   (set-specifier bottom-toolbar toolbar frame tag-set))
	       (set-specifier bottom-toolbar toolbar buffer)
	       (set-specifier bottom-toolbar-height height frame tag-set))
	      (t
	       (setq ciao-xemacs-toolbar-specifier top-toolbar)
	       (if myframe
		   (set-specifier top-toolbar toolbar frame tag-set))
	       (set-specifier top-toolbar toolbar buffer)
	       (set-specifier top-toolbar-height height frame tag-set)))))))

(defun ciao-xemacs-toolbar-make-toolbar-spec ()
  (let ((button-alist '(
			(autofile . ciao-xemacs-toolbar-autofile-button)
			(compose . ciao-xemacs-toolbar-compose-button)
			(delete/undelete . ciao-xemacs-toolbar-delete/undelete-button)
			(file . ciao-xemacs-toolbar-file-button)
			(getmail . ciao-xemacs-toolbar-getmail-button)
			(help . ciao-xemacs-toolbar-help-button)
			(mime . ciao-xemacs-toolbar-decode-mime-button)
			(next . ciao-xemacs-toolbar-next-button)
			(previous . ciao-xemacs-toolbar-previous-button)
			(print . ciao-xemacs-toolbar-print-button)
			(quit . ciao-xemacs-toolbar-quit-button)
			(reply . ciao-xemacs-toolbar-reply-button)
			(visit . ciao-xemacs-toolbar-visit-button)
			))
	(button-list ciao-xemacs-use-toolbar)
	cons
	(toolbar nil))
    (while button-list
      (cond ((null (car button-list))
	     (setq toolbar (cons nil toolbar)))
	    ((integerp (car button-list))
	     (if (< 0 (car button-list))
		 (setq toolbar (cons (vector ':size (car button-list)
					     ':style '2d)
				     toolbar))))
	    (t
	     (setq cons (assq (car button-list) button-alist))
	     (if cons
		 (setq toolbar (cons (symbol-value (cdr cons)) toolbar)))))
      (setq button-list (cdr button-list)))
    (nreverse toolbar) ))

(defun ciao-xemacs-toolbar-initialize ()
  (cond
   (ciao-xemacs-fsfemacs-p nil)
   ((null ciao-xemacs-toolbar-help-icon)
    (let ((tuples
	   (if (featurep 'xpm)
	       (list
		(if (and (device-on-window-system-p)
			 (>= (device-bitplanes) 16))
      '(ciao-xemacs-toolbar-decode-mime-icon "mime-colorful-up.xpm"
				    "mime-colorful-dn.xpm"
				    "mime-colorful-xx.xpm")
   '(ciao-xemacs-toolbar-decode-mime-icon "mime-simple-up.xpm"
				 "mime-simple-dn.xpm"
				 "mime-simple-xx.xpm"))
 '(ciao-xemacs-toolbar-next-icon "next-up.xpm" "next-dn.xpm" "next-dn.xpm")
 '(ciao-xemacs-toolbar-previous-icon "previous-up.xpm" "previous-dn.xpm"
			   "previous-dn.xpm")
 '(ciao-xemacs-toolbar-delete-icon "delete-up.xpm" "delete-dn.xpm" "delete-dn.xpm")
 '(ciao-xemacs-toolbar-undelete-icon "undelete-up.xpm" "undelete-dn.xpm"
			   "undelete-dn.xpm")
 '(ciao-xemacs-toolbar-autofile-icon "autofile-up.xpm" "autofile-dn.xpm"
			   "autofile-dn.xpm")
 '(ciao-xemacs-toolbar-getmail-icon "getmail-up.xpm" "getmail-dn.xpm" "getmail-dn.xpm")
 '(ciao-xemacs-toolbar-file-icon "file-up.xpm" "file-dn.xpm" "file-dn.xpm")
 '(ciao-xemacs-toolbar-reply-icon "reply-up.xpm" "reply-dn.xpm" "reply-dn.xpm")
 '(ciao-xemacs-toolbar-compose-icon "compose-up.xpm" "compose-dn.xpm" "compose-dn.xpm")
 '(ciao-xemacs-toolbar-print-icon "print-up.xpm" "print-dn.xpm" "print-dn.xpm")
 '(ciao-xemacs-toolbar-visit-icon "visit-up.xpm" "visit-dn.xpm" "visit-dn.xpm")
 '(ciao-xemacs-toolbar-quit-icon "quit-up.xpm" "quit-dn.xpm" "quit-dn.xpm")
 '(ciao-xemacs-toolbar-help-icon "help-up.xpm" "help-dn.xpm" "help-dn.xpm")
 '(ciao-xemacs-toolbar-recover-icon "recover-up.xpm" "recover-dn.xpm" "recover-dn.xpm")
	   )
	       '(
 (ciao-xemacs-toolbar-decode-mime-icon "mime-up.xbm" "mime-dn.xbm" "mime-xx.xbm")
 (ciao-xemacs-toolbar-next-icon "next-up.xbm" "next-dn.xbm" "next-xx.xbm")
 (ciao-xemacs-toolbar-previous-icon "previous-up.xbm" "previous-dn.xbm"
			   "previous-xx.xbm")
 (ciao-xemacs-toolbar-delete-icon "delete-up.xbm" "delete-dn.xbm" "delete-xx.xbm")
 (ciao-xemacs-toolbar-undelete-icon "undelete-up.xbm" "undelete-dn.xbm"
			   "undelete-xx.xbm")
 (ciao-xemacs-toolbar-autofile-icon "autofile-up.xbm" "autofile-dn.xbm"
			   "autofile-xx.xbm")
 (ciao-xemacs-toolbar-getmail-icon "getmail-up.xbm" "getmail-dn.xbm" "getmail-xx.xbm")
 (ciao-xemacs-toolbar-file-icon "file-up.xbm" "file-dn.xbm" "file-xx.xbm")
 (ciao-xemacs-toolbar-reply-icon "reply-up.xbm" "reply-dn.xbm" "reply-xx.xbm")
 (ciao-xemacs-toolbar-compose-icon "compose-up.xbm" "compose-dn.xbm" "compose-xx.xbm")
 (ciao-xemacs-toolbar-print-icon "print-up.xbm" "print-dn.xbm" "print-xx.xbm")
 (ciao-xemacs-toolbar-visit-icon "visit-up.xbm" "visit-dn.xbm" "visit-xx.xbm")
 (ciao-xemacs-toolbar-quit-icon "quit-up.xbm" "quit-dn.xbm" "quit-xx.xbm")
 (ciao-xemacs-toolbar-help-icon "help-up.xbm" "help-dn.xbm" "help-xx.xpm")
 (ciao-xemacs-toolbar-recover-icon "recover-up.xbm" "recover-dn.xbm" "recover-xx.xpm")
	   )))
	  tuple files var)
      (while tuples
	(setq tuple (car tuples)
	      var (car tuple)
	      files (cdr tuple))
	(set var (mapcar
		  (function
		   (lambda (f)
		     (make-glyph
		      (expand-file-name f ciao-xemacs-toolbar-pixmap-directory))))
		  files))
	(setq tuples (cdr tuples))))))
  (setq ciao-xemacs-toolbar-delete/undelete-icon ciao-xemacs-toolbar-delete-icon)
  (setq-default ciao-xemacs-toolbar-delete/undelete-icon ciao-xemacs-toolbar-delete-icon)
  (setq ciao-xemacs-toolbar-helper-command 'ciao-xemacs-help)
  (setq ciao-xemacs-toolbar-helper-icon ciao-xemacs-toolbar-help-icon)
  (setq-default ciao-xemacs-toolbar-helper-icon ciao-xemacs-toolbar-help-icon))

(defun ciao-xemacs-toolbar-fsfemacs-uninstall-toolbar ()
  (define-key ciao-xemacs-mode-map [toolbar] nil)
  (setq ciao-xemacs-fsfemacs-toolbar-installed-p nil))

(defun ciao-xemacs-toolbar-fsfemacs-install-toolbar ()
  (let ((button-list (reverse ciao-xemacs-use-toolbar))
	(dir ciao-xemacs-toolbar-pixmap-directory)
	(extension (if (and (display-color-p)
			    (image-type-available-p 'xpm))
		       "xpm"
		     "xbm"))
	item t-spec sym name images)
    (defvar tool-bar-map)
    ;; hide the toolbar entries that are in the global keymap so
    ;; VM has full control of the toolbar in its buffers.
    (if (and (boundp 'tool-bar-map)
	     (consp tool-bar-map))
	(let ((map (cdr tool-bar-map))
	      (v [tool-bar x]))
	  (while map
	    (aset v 1 (car (car map)))
	    (define-key ciao-xemacs-mode-map v 'undefined)
	    (setq map (cdr map)))))
    (while button-list
      (setq sym (car button-list))
      (cond ((null sym)
	     ;; can't do flushright in FSF Emacs
	     t)
	    ((integerp sym)
	     ;; can't do separators in FSF Emacs
	     t)
	    ((memq sym '(autofile compose file getmail
			 mime next previous print quit reply visit))
	     (setq t-spec (symbol-value
			   (intern (format "ciao-xemacs-toolbar-%s-button"
					   (if (eq sym 'mime)
					       'decode-mime
					     sym)))))
	     (if (and (eq sym 'mime) (string= extension "xpm"))
		 (setq name "mime-colorful")
	       (setq name (symbol-name sym)))
	     (setq images (ciao-xemacs-toolbar-make-fsfemacs-toolbar-image-spec
			   name extension dir
			   (if (eq sym 'mime) nil 'heuristic)))
	     (setq item
		   (list 'menu-item
			 (aref t-spec 3)
			 (aref t-spec 1)
			 ':enable (aref t-spec 2)
			 ':button '(:toggle nil)
			 ':image images))
	     (define-key ciao-xemacs-mode-map (vector 'tool-bar sym) item))
	    ((eq sym 'delete/undelete)
	     (setq t-spec ciao-xemacs-toolbar-delete/undelete-button)
	     (setq name "delete")
	     (setq images (ciao-xemacs-toolbar-make-fsfemacs-toolbar-image-spec
			   name extension dir 'heuristic))
	     (setq item
		   (list 'menu-item
			 (aref t-spec 3)
			 (aref t-spec 1)
			 ':visible '(eq ciao-xemacs-toolbar-delete/undelete-icon
					ciao-xemacs-toolbar-delete-icon)
			 ':enable (aref t-spec 2)
			 ':button '(:toggle nil)
			 ':image images))
	     (define-key ciao-xemacs-mode-map (vector 'tool-bar 'delete) item)
	     (setq name "undelete")
	     (setq images (ciao-xemacs-toolbar-make-fsfemacs-toolbar-image-spec
			   name extension dir 'heuristic))
	     (setq item
		   (list 'menu-item
			 (aref t-spec 3)
			 (aref t-spec 1)
			 ':visible '(eq ciao-xemacs-toolbar-delete/undelete-icon
					ciao-xemacs-toolbar-undelete-icon)
			 ':enable (aref t-spec 2)
			 ':button '(:toggle nil)
			 ':image images))
	     (define-key ciao-xemacs-mode-map (vector 'tool-bar 'undelete) item))
	    ((eq sym 'help)
	     (setq t-spec ciao-xemacs-toolbar-help-button)
	     (setq name "help")
	     (setq images (ciao-xemacs-toolbar-make-fsfemacs-toolbar-image-spec
			   name extension dir 'heuristic))
	     (setq item
		   (list 'menu-item
			 (aref t-spec 3)
			 (aref t-spec 1)
			 ':visible '(eq ciao-xemacs-toolbar-helper-command 'ciao-xemacs-help)
			 ':enable (aref t-spec 2)
			 ':button '(:toggle nil)
			 ':image images))
	     (define-key ciao-xemacs-mode-map (vector 'tool-bar 'help-help) item)
	     (setq name "recover")
	     (setq images (ciao-xemacs-toolbar-make-fsfemacs-toolbar-image-spec
			   name extension dir 'heuristic))
	     (setq item
		   (list 'menu-item
			 (aref t-spec 3)
			 (aref t-spec 1)
			 ':visible '(eq ciao-xemacs-toolbar-helper-command
					'recover-file)
			 ':enable (aref t-spec 2)
			 ':button '(:toggle nil)
			 ':image images))
	     (define-key ciao-xemacs-mode-map (vector 'tool-bar 'help-recover) item)
	     (setq name "getmail")
	     (setq images (ciao-xemacs-toolbar-make-fsfemacs-toolbar-image-spec
			   name extension dir 'heuristic))
	     (setq item
		   (list 'menu-item
			 (aref t-spec 3)
			 (aref t-spec 1)
			 ':visible '(eq ciao-xemacs-toolbar-helper-command
					'ciao-xemacs-get-new-mail)
			 ':enable (aref t-spec 2)
			 ':button '(:toggle nil)
			 ':image images))
	     (define-key ciao-xemacs-mode-map (vector 'tool-bar 'help-getmail) item)
	     (if (string= extension "xpm")
		 (setq name "mime-colorful")
	       (setq name "mime"))
	     (setq images (ciao-xemacs-toolbar-make-fsfemacs-toolbar-image-spec
			   name extension dir nil))
	     (setq item
		   (list 'menu-item
			 (aref t-spec 3)
			 (aref t-spec 1)
			 ':visible '(eq ciao-xemacs-toolbar-helper-command
					'ciao-xemacs-decode-mime-message)
			 ':enable (aref t-spec 2)
			 ':button '(:toggle nil)
			 ':image images))
	     (define-key ciao-xemacs-mode-map (vector 'tool-bar 'help-mime) item)))
      (setq button-list (cdr button-list))))
  (setq ciao-xemacs-fsfemacs-toolbar-installed-p t))

(defun ciao-xemacs-toolbar-make-fsfemacs-toolbar-image-spec (name extension dir mask)
  (if (string= extension "xpm")
      (vector
       (list 'image
	     ':type (intern extension)
	     ':mask mask
	     ':file (expand-file-name
		     (format "%s-dn.%s"
			     name extension)
		     dir))
       (list 'image
	     ':type (intern extension)
	     ':mask mask
	     ':file (expand-file-name
		     (format "%s-up.%s"
			     name extension)
		     dir))
       (list 'image
	     ':type (intern extension)
	     ':mask mask
	     ':file (expand-file-name
		     (format "%s-dn.%s"
			     name extension)
		     dir))
       (list 'image
	     ':type (intern extension)
	     ':mask mask
	     ':file (expand-file-name
		     (format "%s-dn.%s"
			     name extension)
		     dir)))
    (vector
     (list 'image
	   ':type (intern extension)
	   ':mask mask
	   ':file (expand-file-name
		   (format "%s-dn.%s"
			   name extension)
		   dir))
     (list 'image
	   ':type (intern extension)
	   ':mask mask
	   ':file (expand-file-name
		   (format "%s-up.%s"
			   name extension)
		   dir))
     (list 'image
	   ':type (intern extension)
	   ':mask mask
	   ':file (expand-file-name
		   (format "%s-xx.%s"
			   name extension)
		   dir))
     (list 'image
	   ':type (intern extension)
	   ':mask mask
	   ':file (expand-file-name
		   (format "%s-xx.%s"
			   name extension)
		   dir)))))
