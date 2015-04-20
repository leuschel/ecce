; -*- mode: emacs-lisp; -*-
;;
;; Ciao/Prolog mode initialization
;; -------------------------------
;; (can normally be used with other Prolog modes and the default prolog.el)
;; 
(setq load-path (cons "/home/asap/asap-online-demo/CiaoDE/ciao/emacs-mode" load-path))

(autoload 'run-ciao-toplevel "ciao"
          "Start a Ciao/Prolog top-level sub-process." t)
(autoload 'ciao-startup "ciao"
          "The Ciao/Prolog program development system startup screens." t)
(autoload 'ciao "ciao"
          "Start a Ciao/Prolog top-level sub-process." t)
(autoload 'prolog "ciao"
          "Start a Ciao/Prolog top-level sub-process." t)
(autoload 'run-ciao-preprocessor "ciao"
          "Start a Ciao/Prolog preprocessor sub-process." t)
(autoload 'ciaopp "ciao"
          "Start a Ciao/Prolog preprocessor sub-process." t)
(autoload 'ciao-mode "ciao"
          "Major mode for editing and running Ciao/Prolog" t)
(autoload 'ciao-inferior-mode "ciao"
          "Major mode for running Ciao/Prolog, CiaoPP, LPdoc, etc." t)
(setq auto-mode-alist (cons '("\\.pl$" . ciao-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.pls$" . ciao-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.lpdoc$" . ciao-mode) auto-mode-alist))
(setq completion-ignored-extensions
      (append '(".dep" ".itf" ".po" ".asr" ".cpx")
              completion-ignored-extensions))
;; ------------------------------------------------------------------------
;; In Un*x, the following (or similar) lines should be included in your
;; .cshrc or .profile to find the manuals (the Ciao installation leaves
;; in the Ciao library directory 'DOTcshrc' and 'DOTprofile' files with
;; the right paths which can be included directly in your startup scripts):
;; 
;; setenv INFOPATH /usr/local/info:/usr/info:<LPDOCDIR>
;; ------------------------------------------------------------------------
