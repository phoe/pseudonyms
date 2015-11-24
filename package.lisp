;;;; package.lisp

(defpackage #:pseudonyms
  (:use #:cl #:trivial-garbage)
  (:export :*pseudonym-table*
	   :defpseudonym
	   :pmakunbound
	   :print-pseudonyms
	   :pseudonym-reader))

