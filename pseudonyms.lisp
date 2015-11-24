;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; pseudonyms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; thanks to:
;;;; #lisp@freenode: pjb, blubjr, sid_cypher, PuercoPop, shka
;;;; for testing and ideas
;;;; license: FreeBSD (BSD 2-clause)
;;;;
;;;; pseudonyms.lisp

(in-package #:pseudonyms)

;;; ========================================================================
;;; GLOBAL VARIABLES

(defparameter *pseudonym-table*
  (make-weak-hash-table :test #'equal :weakness :key)
  "This is a global package-name-indexed hashtable holding name-pseudonym
plists.")

;;; ========================================================================
;;; HELPER FUNCTIONS AND TYPES

(deftype string-designator () '(or string symbol character))

(defun string=-getf (plist indicator)
  "This is a version of getf utilizing string= for comparison.
Given a plist and a key, returns a value."
  (loop for key in plist by #'cddr
     for value in (rest plist) by #'cddr
     when (string= key indicator)
     return value))

(defun string=-getf-key (plist indicator)
  "This is a version of getf utilizing string= for comparison.
Given a plist and a value, returns a key."
  (loop for key in plist by #'cddr
     for value in (rest plist) by #'cddr
     when (string= value indicator)
     return (values key)))

;;; ========================================================================
;;; DEFINE/UNDEFINE FUNCTIONS

(defun defpseudonym (raw-name raw-pseudonym &key (package (package-name *package*)))
  "This, given a package name and a pseudonym for it, allows you
to use a local pseudonym in form $pseudonym:symbol instead of
name:symbol within your code. This pseudonym is local to the package
you called defpseudonym in (as shown by the global variable
*PACKAGE*).

Arguments must be a pair of non-empty non-equal string designators.
An optional argument allows you to set a pseudonym for a different package.

Pseudonyms are always converted to lowercase.

This will signal an error whenever a nickname or pseudonym is
already taken."
  (check-type raw-name string-designator)
  (check-type raw-pseudonym string-designator)
  (let* ((table (gethash (string-downcase package) *pseudonym-table*))
	 (name (string-downcase raw-name))
	 (pseudonym (string-downcase raw-pseudonym))
	 (first (car table)))
    (assert (not (member "" (list name pseudonym) :test #'string=))
	    (name pseudonym)
	    "Name and pseudonym may not be empty.")
    (assert (not (string= name pseudonym))
	    (name pseudonym)
	    "Pseudonyming ~S to itself is not a good idea." name)
    (assert (not (string=-getf table name))
	    (name)
	    "This name is already taken by pseudonym ~S."
	    (string=-getf table name))
    (assert (not (string=-getf-key table pseudonym))
	    (pseudonym)
	    "This pseudonym is already taken by name ~S."
	    (string=-getf-key table pseudonym))
    (if (null table)
	(setf (gethash (string-downcase package) *pseudonym-table*)
	      (cons name (cons pseudonym nil)))
	(setf (car table) name
	      (cdr table) (cons pseudonym (cons first (cdr table)))))
    (format nil "~A => ~A" pseudonym name)))

(defun pmakunbound (datum &key (package (package-name *package*)))
  "This, given either a nickname-bound package name or a
package name-bound nickname, clears any name-nickname pair bound to it.

Argument must be a string designator.
An optional argument allows you to clear a pseudonym from a different
package."
  (check-type datum string-designator)
  (let ((table (gethash (string-downcase package) *pseudonym-table*)))
    (setf datum (string-downcase datum)
	  table
	  (loop for (key value) on table by #'cddr
	     unless (or (equal key datum) (equal value datum))
	     collect key and collect value)))
  (string-downcase datum))

;;; ========================================================================
;;; UTILITIES

(defun print-pseudonyms (&key (package (package-name *package*)))
  "This prints all pseudonyms in a fancy manner.
Optional argument designates the package name, from inside which 
pseudonyms should be printed."
  (check-type package string-designator)
  (let* ((string (string-downcase package))
	 (table (gethash string *pseudonym-table*)))
    (if (null table)
	(format t "No pseudonyms defined for package ~:@(~A~).~%" package)
	(progn
	  (format t "pseudonym => name (package ~:@(~A~)):~%" package)
	  (list-length
	   (loop for (key value) on table by #'cddr collecting key
	      do (format t "~S => ~S~%" value key)))))))

;;; ========================================================================
;;; READER MACRO

(set-macro-character #\$ 'pseudonym-reader)

(defun pseudonym-reader (stream char)
  "This is the reader macro for local pseudonyms."
  (declare (ignore char))
  (let* ((table (gethash (string-downcase (package-name *package*)) *pseudonym-table*))
	 (pseudlist (loop for char = (read-char stream)
		       collect char
		       until (equal (peek-char nil stream) #\:)))
	 (pseudonym (string-downcase (concatenate 'string pseudlist)))
	 (name (string=-getf-key table pseudonym))
	 (symbol (read stream)))
    ;;(format t "debug: ~A ~A ~A ~A ~A ~A~%"
    ;;(string-downcase (package-name *package*))
    ;;pseudlist pseudonym table name symbol)
    (assert (not (null name)) ()
	    "Pseudonym ~S was not set. Check your spelling or use defpseudonym."
	    pseudonym)
    (read-from-string (format nil "~A:~A" name symbol))))

;; todo: named readtables
