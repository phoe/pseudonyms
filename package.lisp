;;;; package.lisp

(defpackage #:pseudonyms
  (:use #:cl
	#:trivial-garbage
	#:named-readtables)
  (:export :*pseudonym-table*
	   :set-pseudonym-macro-character 
	   :defpseudonym
	   :pmakunbound
	   :print-pseudonyms
	   :pseudonym-reader)
  (:documentation
   "=== PSEUDONYMS by phoe ===

See attached README.md for documentation."))
