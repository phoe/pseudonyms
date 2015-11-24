;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; pseudonyms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; thanks to:
;;;; #lisp@freenode: pjb, blubjr, sid_cypher, PuercoPop
;;;; for testing and ideas
;;;; license: FreeBSD (BSD 2-clause)
;;;;
;;;; pseudonyms.lisp

#|

============================ PSEUDONYMS ===================================

I found that Lisp nicknames, as defined in CLHS, have a few problems that I
will count here.

1) They are not changeable without internal side-effects. RENAME-PACKAGE
is destructive, as it kills off any previous names the package.
2) They collide. Nickname GL is used by at least three different Lisp
packages.

The solution I provide here is a different approach to nicknames that does
not use any of the original nickname code, as defined in CLHS.

Pseudonyms, in opposition to nicknames, can be defined by the user inside
one's code, like this:
> (defpseudonym "longpackagename" "lpn")
And removed like this:
> (pmakunbound "lpn") ;; OR (pmakunbound "longpackagename")

From within the code, one can refer to a pseudonymized package this way:
> $lpn:something
A reader macro will automatically translate it to its normal version of
longpackagename:something. This is usable both within the REPL and within
usual code.

All pseudonyms are local to the current package: for instance, pseudonyms
defined within CL-USER are not usable anywhere outside the CL-USER package.

An utility function print-pseudonyms will print all pseudonyms for a given
package. If not supplied a package name as an argument, it will print all
pseudonyms for current package (as shown by the *package* global variable).

|#

(in-package #:pseudonyms)

;;; ========================================================================
;;; GLOBAL VARIABLES

(defparameter *pseudonym-table*
  (make-weak-hash-table :test #'equal :weakness :key)
  "This is a global package-name-indexed hashtable holdingname-pseudonym
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

(defun defpseudonym (name pseudonym)
  "This, given a package name and a pseudonym for it, allows you
to use a local pseudonym in form $pseudonym:symbol instead of
name:symbol within your code. This pseudonym is local to the package
you called defpseudonym in (as shown by the global variable
*PACKAGE*).

Arguments must be a pair of non-empty non-equal string designators.

Pseudonyms are always converted to lowercase.

This will signal an error whenever a nickname or pseudonym is
already taken."
  
  (check-type name string-designator)
  (check-type pseudonym string-designator)
  (setf name (string-downcase (string name))
	pseudonym (string-downcase (string pseudonym)))
  (assert (not (member "" (list name pseudonym) :test #'string=))
	  (name pseudonym)
	  "Name and pseudonym may not be empty.")
  (assert (not (string= name pseudonym))
	  (name pseudonym)
	  "Pseudonyming ~S to itself is not a good idea." name)
  (let* ((pkgname       (package-name *package*))
	 (name-test     (string=-getf
			 (gethash pkgname *pseudonym-table*) name))
	 (pseudonym-test (string=-getf-key
			  (gethash pkgname *pseudonym-table*) pseudonym)))
    (assert (not name-test) ()
	    "This name is already taken by pseudonym ~S.
Use pmakunbound first if you are sure what you're doing."
	    name-test)
    (assert (not pseudonym-test) ()
	    "This pseudonym is already taken by name ~S.
Use pmakunbound first if you are sure what you're doing."
	    pseudonym-test)
    
    (push pseudonym (gethash pkgname *pseudonym-table*))
    (push name (gethash pkgname *pseudonym-table*))
    (format nil "~A => ~A" pseudonym name)))

(defun pmakunbound (datum)
  "This, given either a nickname-bound package name or a
package name-bound nickname, clears any name-nickname pair bound to it.

Argument must be a string designator."
  (check-type datum string-designator)
  (setf datum (string-downcase (string datum))
	(gethash (package-name *package*) *pseudonym-table*)
	(loop for (key value)
	   on (gethash (package-name *package*) *pseudonym-table*)
	   by #'cddr
	   unless (or (equal key datum) (equal value datum))
	   collect key and collect value))
  datum)

;;; ========================================================================
;;; UTILITIES

(defun print-pseudonyms (&optional (pkgname (package-name *package*)))
  "This prints all pseudonyms in a fancy manner.
Optional argument designates the package name, from inside which 
pseudonyms should be printed."
  (check-type pkgname string)
  (cond
    ((null (gethash pkgname *pseudonym-table*))
     (format t "No pseudonyms defined for package ~:@(~A~).~%" pkgname)
     nil)
    (t
     (format t "pseudonym => name (package ~:@(~A~)):
==============================~%" pkgname)
     (list-length
      (loop for (key value)
	 on (gethash pkgname *pseudonym-table*)
	 by #'cddr collecting key
	 do (format t "~S => ~S~%" value key))))))

;;; ========================================================================
;;; READER MACRO

(set-macro-character #\$ 'pseudonym-reader)

(defun pseudonym-reader (stream char)
  "This is the reader macro for local pseudonyms."
  (declare (ignore char))
  (let* ((pseudlist (loop for char = (read-char stream)
		       collect char
		       until (equal (peek-char nil stream) #\:)))
	 (pseudonym (string-downcase (concatenate 'string pseudlist)))
	 (name (string=-getf-key (gethash (package-name *package*)
					  *pseudonym-table*)
				 pseudonym))
	 (symbol (read stream)))
    (assert (not (null name)) ()
	    "Pseudonym ~S was not set. Check your spelling or use defpseudonym."
	    pseudonym)
    ;;(format t "debug: ~A ~A ~A ~A ~A~%"
    ;;(package-name *package*) pseudlist pseudonym name symbol)
    (read-from-string (format nil "~A:~A" name symbol))))

;; todo: named readtables
