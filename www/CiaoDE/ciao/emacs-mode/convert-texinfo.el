
;; (define-key ciao-mode-map "\C-c\C-t" 'ciao-partially-convert-texinfo-region)

(defun ciao-partially-convert-texinfo-region (begin end) 

"Does a 90% job of converting a given region from texinfo format into
the format accepted by the documentation strings which can be added to
Ciao assertions and comments (and which the LPdoc automatic program
documenter processes). This is very useful when pasting parts of
manuals which have been previously written in texinfo into an
assertion in a program, for later documentation using lpdoc."

  (interactive "r")
  (setq region-tmp-char (point))

  (ciao-replace-string-region begin end "@dfn{"     "@concept{")
  (ciao-replace-string-region begin end "@emph{"    "@em{")
  (ciao-replace-string-region begin end "@code{"    "@tt{")
  (ciao-replace-string-region begin end "@refill"   " ")
  (ciao-replace-string-region begin end "@key{"     "@key{")
  (ciao-replace-string-region begin end "@kbd{"     "@tt{")

  (ciao-replace-string-region begin end "@itemize"     "@begin{itemize}")
  (ciao-replace-string-region begin end "@enumerate"   "@begin{enumerate}")
  (ciao-replace-string-region begin end "@example"     "@begin{verbatim}")
  ;; Needs to appear before treatment of @end below:
  (ciao-replace-string-region begin end "@end example" "@end{verbatim}")
  
  (ciao-wrap-command-region   begin end "@cindex ")
  (ciao-wrap-command-region   begin end "@end ")
  (ciao-wrap-command-region   begin end "@section ")
  (ciao-wrap-command-region   begin end "@subsection ")

  (goto-char region-tmp-char)
  )


(defun ciao-replace-string-region (begin end source target)
    (goto-char begin)
    (while 
	(search-forward source end t) 
      (replace-match target nil t)))

(defun ciao-wrap-command-region (begin end command)
    (goto-char begin)
    (while 
	(search-forward command end t) 
      (progn 
	(backward-char 1)
	(delete-char 1)
	(insert-string "{")
	(end-of-line)
	(insert-string "}")
	)))

(defun ciao-convert-latex-region (begin end) 

"Does a (typically) 75% job of converting a given region from LaTeX
format into the format accepted by the documentation strings which can
be added to Ciao assertions and comments (and which the lpdoc
automatic program documenter processes). This is very useful when
pasting parts of documents or manuals which have been previously
written in LaTeX into an assertion or comment in a program, for
documentation using lpdoc. 

The conversion is necessarily incomplete, since lpdoc currently
supports only a small subset of LaTeX.  After running the command you
can look for '\\'. 

**** WARNING: still not implemented.
"

  (interactive "r")
  (message "**** WARNING: still not implemented")
  (setq region-tmp-char (point))

;;   (ciao-replace-string-region begin end "@dfn{"     "@concept{")
;;   (ciao-replace-string-region begin end "@emph{"    "@em{")
;;   (ciao-replace-string-region begin end "@code{"    "@tt{")
;;   (ciao-replace-string-region begin end "@refill"   " ")
;;   (ciao-replace-string-region begin end "@key{"     "@key{")
;;   (ciao-replace-string-region begin end "@kbd{"     "@tt{")
;; 
;;   (ciao-replace-string-region begin end "@itemize"     "@begin{itemize}")
;;   (ciao-replace-string-region begin end "@enumerate"   "@begin{enumerate}")
;;   (ciao-replace-string-region begin end "@example"     "@begin{verbatim}")
;;   ;; Needs to appear before treatment of @end below:
;;   (ciao-replace-string-region begin end "@end example" "@end{verbatim}")
;;   
;;   (ciao-wrap-command-region   begin end "@cindex ")
;;   (ciao-wrap-command-region   begin end "@end ")
;;   (ciao-wrap-command-region   begin end "@section ")
;;   (ciao-wrap-command-region   begin end "@subsection ")

  (goto-char region-tmp-char)
  )


