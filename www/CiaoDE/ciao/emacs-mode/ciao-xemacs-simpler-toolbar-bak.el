
(defcustom ciao-xemacs-inhibit-toolbar nil
  "*Non-nil means don't use the specialized ciao toolbar in xemacs."
  :type 'boolean
  :group 'ciao)

(defcustom ciao-toolbar-icon-directory "/home/clip/Systems/ciao/emacs-mode/"
  "Directory for icons....."
  :type 'string
  :group 'ciao)

;; *** Just for testing 
(defun ciao-tool-bar-add-item-from-menu (comm icon &optional map &rest props)
  (if (boundp 'xemacs-logo)
      ;; xmacs
      (progn 
	`[(toolbar-make-button-list
	  (expand-file-name "icons/ciaopl.xpm" ciao-toolbar-icon-directory)) 
	 ]
	)
    ;; FSF emacs
    (unless map
      (setq map global-map))
    (tool-bar-add-item-from-menu comm icon map props)))

;; *** Just for testing 
(defun ciao-tool-bar-add-item (icon def key &rest props)
  (tool-bar-add-item icon def key props))

(defvar ciao-xemacs-toolbar
  (if (and (boundp 'xemacs-logo) (featurep 'toolbar))
      `([,(toolbar-make-button-list
	   (expand-file-name "icons/ciaopl.xpm" ciao-toolbar-icon-directory))
	 ;; ciao-xemacs-toolbar-open-file-icon ;; icon
	 ciao-xemacs-toolbar-delete         ;; function
	 t                                  ;; enabled
	 "Open ciao file"]                  ;; tooltip
	[:style 2d :size 30]
	(ciao-tool-bar-add-item-from-menu 
	 ciao-xemacs-toolbar-delete
	 "info-exit.xpm"
	 'foo 'bar)
;; 	[ciao-xemacs-toolbar-exit-icon
;; 	 ciao-xemacs-toolbar-delete
;; 	 t
;; 	 "Exit info"]
	[ciao-xemacs-toolbar-up-icon
	 ciao-xemacs-toolbar-up
	 t
	 "Up entry to enclosing section"]
	)))

;;; Ciao-xemacs-toolbar toolbar support

;; (defvar ciao-xemacs-toolbar-open-file-icon
;;   (if (featurep 'toolbar)
;;       (toolbar-make-button-list
;;        (expand-file-name "icons/ciaopl.xpm" ciao-toolbar-icon-directory)))
;;   "Open file icon")

;; exit icon taken from GNUS
(defvar ciao-xemacs-toolbar-exit-icon
  (if (featurep 'toolbar)
      (toolbar-make-button-list
       (expand-file-name (if (featurep 'xpm) "info-exit.xpm" "info-exit.xbm")
			 toolbar-icon-directory)))
  "Exit Ciao-xemacs-toolbar icon")

(defvar ciao-xemacs-toolbar-up-icon
  (if (featurep 'toolbar)
      (toolbar-make-button-list
       (expand-file-name (if (featurep 'xpm) "info-up.xpm" "info-up.xbm")
			 toolbar-icon-directory)))
  "Up icon")

(defvar ciao-xemacs-toolbar-next-icon
  (if (featurep 'toolbar)
      (toolbar-make-button-list
       (expand-file-name (if (featurep 'xpm) "info-next.xpm" "info-next.xbm")
			 toolbar-icon-directory)))
  "Next icon")

(defvar ciao-xemacs-toolbar-prev-icon
  (if (featurep 'toolbar)
      (toolbar-make-button-list
       (expand-file-name (if (featurep 'xpm) "info-prev.xpm" "info-prev.xbm")
			 toolbar-icon-directory)))
  "Prev icon")

(defun ciao-xemacs-setup-toolbar ()
  ""
  (interactive)
  ;; #### The console-on-window-system-p check is to allow this to
  ;; work on tty's.  The real problem here is that featurep really
  ;; needs to have some device/console domain knowledge added to it.
  (if (and (featurep 'toolbar)
	   (console-on-window-system-p)
	   (not ciao-xemacs-inhibit-toolbar))
      ;; (set-specifier left-toolbar-width 50)
      (set-specifier 
       default-toolbar 
       ;; left-toolbar
       (cons 
	(current-buffer) 
	(append
	 (specifier-specs default-toolbar 'global)
	 '([:style 2d :size 30])
	 ciao-xemacs-toolbar
	 )
	;;	 ciao-xemacs-toolbar
	)))

  ;; Interesting to filter existing menubar...
  (if (featurep 'menubar)
      (progn
	;; make a local copy of the menubar, so our modes don't
	;; change the global menubar
	(easy-menu-add '("ciao-xemacs-toolbar" 
			 :filter ciao-xemacs-toolbar-menu-filter))))
  )

(defun ciao-xemacs-toolbar-delete ()
  "Exit ciao-xemacs-toolbar by selecting some other buffer."
  (interactive)
  (if (and (featurep 'toolbar)
	   (boundp 'toolbar-info-frame)
	   (eq toolbar-info-frame (selected-frame)))
      (condition-case ()
	  (delete-frame toolbar-info-frame)
	(error (bury-buffer)))
    (switch-to-buffer (other-buffer (current-buffer)))))

