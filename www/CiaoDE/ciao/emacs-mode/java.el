(require 'easymenu)
(require 'ciao)

(defun java-ciaopp ()
  (interactive)
  ;; option in the menu
  (easy-menu-define ciao-java-menu-ciaopp java-mode-map 
    "CiaoPP Mode Menus" ciao-mode-menus-ciaopp)
  (easy-menu-add ciao-java-menu-ciaopp)
  ;; toolbar
  (ciao-java-setup-tool-bar)
 )


;---------------------------------------------------


(defun ciao-java-setup-tool-bar () 
  (interactive)
  (set (make-local-variable 'tool-bar-map) 
       (if (display-graphic-p) 
	   (let ((tool-bar-map (make-sparse-keymap)))
;; General stuff
;; 	     (tool-bar-add-item "icons/ciaopl" 'find-file 'find-file 
;;               :help "Open or create a (Ciao) file") 
	     (tool-bar-add-item-from-menu 'dired "open")
	     (tool-bar-add-item-from-menu 'kill-this-buffer "close")
	     (tool-bar-add-item-from-menu 'save-buffer "save" nil
			       :visible '(or buffer-file-name
					     (not (eq 'special
						      (get major-mode
							   'mode-class)))))
	     (tool-bar-add-item-from-menu 'write-file "saveas" nil
			       :visible '(or buffer-file-name
					     (not (eq 'special
						      (get major-mode
							   'mode-class)))))
	     (tool-bar-add-item-from-menu 'undo "undo" nil
			       :visible '(not (eq 'special (get major-mode
								'mode-class))))
	     (tool-bar-add-item-from-menu 'kill-region "cut" nil
			       :visible '(not (eq 'special (get major-mode
								'mode-class))))
	     (tool-bar-add-item-from-menu 'menu-bar-kill-ring-save "copy")
	     (tool-bar-add-item-from-menu 'yank "paste" nil
			       :visible '(not (eq 'special (get major-mode
								'mode-class))))
	     (tool-bar-add-item-from-menu 
                               'nonincremental-search-forward "search")
	     (tool-bar-add-item-from-menu 'print-buffer "print")
;; Ciao-specific stuff
	     (tool-bar-add-item-from-menu 
	      'run-ciao-toplevel "icons/ciao" java-mode-map)
	     (tool-bar-add-item-from-menu 
	      'ciao-fontify-buffer "icons/ciaorehighlight" java-mode-map)
	     (tool-bar-add-item-from-menu 
	      'ciao-load-buffer "icons/ciaoload" java-mode-map)
	     (tool-bar-add-item-from-menu 
	      'ciao-find-last-run-errors "jump_to" java-mode-map)
	     (tool-bar-add-item-from-menu 
	      'ciao-unmark-last-run-errors "icons/clear" java-mode-map)
	     (tool-bar-add-item-from-menu 
	      'ciao-check-buffer-syntax "icons/ciaoasr" java-mode-map)
;; 	     (tool-bar-add-item-from-menu 
;; 	      'ciao-check-types-modes    "icons/checkassertions" java-mode-map)
;; 	     (tool-bar-add-item-from-menu 
;; 	      'ciao-preprocess-buffer-menu 
;; 	      "icons/ciaopreprocask" java-mode-map)
;; 	     (tool-bar-add-item-from-menu 
;; 	      'ciao-preprocess-buffer    "icons/ciaopreproc" java-mode-map)
;; 	     (tool-bar-add-item-from-menu 
;; 	      'ciao-preprocess-buffer-and-show-output
;; 	      "icons/ciaopreprocsee" java-mode-map)
	     (tool-bar-add-item-from-menu 
	      'ciao-analyze-buffer "icons/ciaoanalysis" java-mode-map)
 	     (tool-bar-add-item-from-menu 
 	      'ciao-check-assertions "icons/checkassertions" java-mode-map)
 	     (tool-bar-add-item-from-menu 
 	      'ciao-optimize-buffer "icons/ciaopeval" java-mode-map)
	     (tool-bar-add-item-from-menu 
 	      'ciao-browse-preprocessor-options
 	      "icons/ciaocustomize" java-mode-map)
	     (tool-bar-add-item-from-menu 
	      'ciao-debug-buffer "icons/ciaodebug" java-mode-map)
	     (tool-bar-add-item-from-menu 
	      'ciao-gen-buffer-doc "icons/lpdoc" java-mode-map)
	     (tool-bar-add-item-from-menu 
	      'ciao-start-viewer "icons/lpdocview" java-mode-map)
	     (tool-bar-add-item-from-menu 
	      'ciao-make-exec "icons/ciaoexeout" java-mode-map)
	     (tool-bar-add-item-from-menu 
	      'ciao-insert-script-header "icons/ciaoscrt" java-mode-map)
;; 	     (tool-bar-add-item-from-menu 
;; 	      'ciao-make-po "icons/ciaopo" java-mode-map)
;; 	     (tool-bar-add-item-from-menu 
;; 	      'ciao-make-exec "icons/ciaoitf" java-mode-map)
;;	     (tool-bar-add-item "ciaomanuals" 
	     (tool-bar-add-item "icons/manuals" 
              'ciao-goto-ciao-manuals 'ciao-goto-ciao-manuals 
	      :help "Go to area containing the Ciao system manuals")
	     (tool-bar-add-item-from-menu 
	      'ciao-help-on-current-symbol "icons/wordhelp" java-mode-map)
	     (tool-bar-add-item-from-menu 
	      'ciao-complete-current-symbol "icons/complete" java-mode-map)
	     (tool-bar-add-item "preferences" 
				(lambda ()
				  (interactive)
				  (customize-group 'ciao-environment))
				'ciao-customize
 	      :help "Edit  (customize) preferences for Ciao, CIaoPP, LPdoc")
	     tool-bar-map))))



