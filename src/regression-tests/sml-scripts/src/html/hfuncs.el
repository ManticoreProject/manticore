;; hfuncs.el

;; Adam Shaw
;; Aug 17 2008

;; Some ad-hoc emacs functions for writing an ML signature.

;; hfuncs-non-interactive : string -> unit
;; generate ML specs for given name; read the code to see which specs
;; ex: (hfuncs-non-interactive "h1")
(defun hfuncs-non-interactive (name)
  "Generate a bunch of ML specs for given NAME."
  (princ (concat "val " name "   : string -> html\n"
		 "val " name "H  : html -> html\n"
		 "val " name "CS : class_name * string -> html\n"
		 "val " name "CH : class_name * html -> html\n\n")
	 (current-buffer)))

;; sample use
(mapc 'hfuncs-non-interactive '("a" "b" "c"))

;; hfuncs : string -> unit
;; interactive version of the above
(defun hfuncs (name)
  "Generate a bunch of ML specs for given NAME."
  (interactive "sName (e.g. h1): ")
  (hfuncs-non-interactive name))






