
(defcustom ciao-inhibit-toolbar nil
  "*Non-nil means don't use the specialized ciao toolbar in xemacs."
  :type 'boolean
  :group 'ciao)

(defcustom ciao-toolbar-icon-directory "/home/clip/Systems/ciao/emacs-mode/"
  "Directory for icons....."
  :type 'string
  :group 'ciao)

;; -------------------------------------
;; The portable functions:

;; Menu bar accummulator for xemacs version
(defvar ciao-xemacs-tool-bar-tmp nil)

;; Portable tool-bar-add-item-from-menu function
;; Adds to accumulators
(defun ciao-tool-bar-add-item-from-menu 
  (comm icon &optional from-map &rest props)
  (if (boundp 'xemacs-logo)
      ;; xemacs
      (progn 
	(setq ciao-xemacs-tool-bar-tmp
	      (cons 
	       `[,(toolbar-make-button-list ;; icon
		   (expand-file-name (concat icon ".xpm")
				     ciao-toolbar-icon-directory))
		 ,comm ;; the actual callback
		 t  ;; enabled
		 "" ;; tooltip
		 ] 
	       ciao-xemacs-tool-bar-tmp))
	ciao-xemacs-tool-bar-tmp)
    ;; FSF emacs
    (unless from-map
      (setq from-map global-map))
    (tool-bar-add-item-from-menu comm icon from-map props)))

;; Portable low level tool-bar function (to be developed!)
(defun ciao-tool-bar-add-item (icon def key &rest props)
  (tool-bar-add-item icon def key props))

;; -------------------------------------
;; The xemacs toolbar

(defun ciao-xemacs-setup-toolbar ()
  ""
  (interactive)
  ;; #### The console-on-window-system-p check is to allow this to
  ;; work on tty's.  The real problem here is that featurep really
  ;; needs to have some device/console domain knowledge added to it.
  (if (and (featurep 'toolbar)
	   (console-on-window-system-p)
	   (not ciao-inhibit-toolbar))
      (progn
	(setq ciao-xemacs-tool-bar-tmp nil)
	(ciao-tool-bar-add-item-from-menu 
	 'ciao-pressed-message "icons/ciaopl" 'foo 'bar)
	;; [:style 2d :size 30]
	(ciao-tool-bar-add-item-from-menu 
	 'ciao-pressed-message "icons/ciaoasr" 'foo 'bar)
	(ciao-tool-bar-add-item-from-menu 
	 'ciao-pressed-message "icons/ciaoasr" 'foo 'bar)
	;; (set-specifier left-toolbar-width 50)
	(set-specifier 
	 default-toolbar 
	 ;; left-toolbar
	 (cons 
	  (current-buffer) 
	  (append
	   (specifier-specs default-toolbar 'global)
	   '([:style 2d :size 30])
	   ;;;;;;;;;;;;;;;;;;;;;
	   ;; (ciao-xemacs-toolbar)
	   ciao-xemacs-tool-bar-tmp
	   ;;;;;;;;;;;;;;;;;;;;;
	   )
	  ;;	 ciao-xemacs-toolbar
	  ))))

;; ???
;;   ;; Interesting to filter existing menubar...
;;   (if (featurep 'menubar)
;;       (progn
;; 	;; make a local copy of the menubar, so our modes don't
;; 	;; change the global menubar
;; 	(easy-menu-add '("ciao-xemacs-toolbar" 
;; 			 :filter ciao-xemacs-toolbar-menu-filter))))
  )

;; For testing:
(defun ciao-pressed-message () (message "Button pressed!"))

;; ---------------------
;; The FSF emacs toolbar (and adding xemacs gradually)

;; *** For playing around
;; temporary
(defvar tool-bar-map)
(defun ciao-setup-tool-bar () 
  (interactive) ;; temporary
  ;; Tool bar available and not inhibited?
  (if (and (not ciao-inhibit-toolbar)
	   (or 
	    ;; FSF emacs case 
	    (fboundp 'tool-bar-mode)
	    ;; xemacs case 
	    (and (boundp 'xemacs-logo)
		 (featurep 'toolbar)
		 (console-on-window-system-p))
	   ))
      (ciao-do-setup-tool-bar)))

;; The idea is that with the same code in FSF emacs items are added to
;; tool-bar-map (a map), and in xemacs they are added to
;; ciao-xemacs-tool-bar-tmp (a simple list) 
(defun ciao-do-setup-tool-bar () 
  (make-local-variable 'tool-bar-map)
  (set 'tool-bar-map
       (let (tool-bar-map)
	 (if (boundp 'xemacs-logo)
	     (setq ciao-xemacs-tool-bar-tmp nil)
	   (setq tool-bar-map (make-sparse-keymap)))
;; 	 (ciao-tool-bar-add-item "icons/ciaopl" 'find-file 'find-file 
;; 				 :help "Open or create a (Prolog) file") 
	 (ciao-tool-bar-add-item-from-menu 
	  'ciao-pressed-message "icons/ciaopl")
	 (ciao-tool-bar-add-item-from-menu 
	  'ciao-pressed-message "icons/ciaoasr")
;; 	 (ciao-tool-bar-add-item-from-menu 
;; 	  'save-buffer "save" nil
;; 	  :visible '(or buffer-file-name
;; 			(not (eq 'special
;; 				 (get major-mode
;; 				      'mode-class)))))
;;
;; 	   (ciao-tool-bar-add-item-from-menu 
;; 	    'run-ciao-toplevel "icons/ciao" ciao-mode-map)
;; 	   (ciao-tool-bar-add-item 
;; 	    "icons/manuals" 'ciao-goto-ciao-manuals 'ciao-goto-ciao-manuals 
;; 	    :help "Go to area containing the Ciao system manuals")
;; 	   (ciao-tool-bar-add-item 
;; 	    "preferences" 
;; 	    (lambda ()
;; 	      (interactive)
;; 	      (customize-group 'ciao-environment))
;; 	    'ciao-customize
;; 	    :help "Edit  (customize) preferences for Ciao, CIaoPP, LPdoc")
	   tool-bar-map))
  (if (boundp 'xemacs-logo)
      (progn
	(set-specifier 
	 default-toolbar 
	 ;; left-toolbar
	 (cons 
	  (current-buffer) 
	  (append
	   (specifier-specs default-toolbar 'global)
	   '([:style 2d :size 30])
	   ciao-xemacs-tool-bar-tmp
	   )))
	)))

;; (if (boundp 'xemacs-logo)
;;     ()
;;   (if (not (display-graphic-p))
;;       (set 'tool-bar-map nil)
;;     (set 'tool-bar-map tool-bar-map-or-list)))

