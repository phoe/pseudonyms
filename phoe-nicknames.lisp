;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; phoe-nicknames
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; thanks to pjb for the idea
;;;; license: FreeBSD
;;;;
;;;; phoe-nicknames.lisp

(in-package #:phoe-nicknames)

(defparameter *nickname-table* nil
  "This is a global variable holding local nicknames.")

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

(defun defnickname (name nickname)
  "This, given a package name and a nickname for it, allows you
to use a local nickname in form $nickname:symbol instead of
name:symbol within your code. Arguments must be a pair of
non-empty non-equal strings.

This will signal an error whenever a name or nickname is already
taken."
  (cond
    ((or (not (stringp name))
	 (not (stringp nickname)))
     (error "Arguments must be a pair of strings."))
    ((or (string= name "")
	 (string= nickname ""))
     (error "Using an empty string *will* get you in trouble."))
    ((string= name nickname)
     (error (format nil "Nicknaming \"~A\" to itself is not a good idea." name)))
    ((string=-getf *nickname-table* name)
     (error (format
	     nil
	     "This name is already taken by nickname \"~A\". Use nmakunbound first."
	     (string=-getf *nickname-table* name))))
    ((string=-getf-key *nickname-table* nickname)
     (error (format
	     nil
	     "This nickname is already taken by name \"~A\". Use nmakunbound first."
	     (string=-getf-key *nickname-table* nickname))))
    (t
     (push nickname *nickname-table*)
     (push name *nickname-table*)
     name)))

(defun nmakunbound (string)
  "This, given either a nickname-bound package name or a
package name-bound nickname, clears any name-nickname pair bound to it."
  (setf *nickname-table*
	(loop for (key value) on *nickname-table* by #'cddr
	   unless (or (member key (list string) :test #'equal)
		      (member value (list string) :test #'equal))
	   collect key and collect value))
  string)

(defun print-nicknames ()
  "This prints all local nicknames in a fancy manner."
  (loop for (key value) on *nickname-table* by #'cddr
     do (format t "~A => ~A~%" value key)))

(set-macro-character #\$ 'nickname-reader)

(defun nickname-reader (stream char)
  "This is the reader macro for local nicknames."
  (declare (ignore char))
  (let* ((nicklist (loop for char = (read-char stream)
		      then (read-char stream)
		      collect char
		      until (equal (peek-char nil stream) #\:)))
	 (nickname (concatenate 'string nicklist))
	 (name (string=-getf-key *nickname-table* nickname))
	 (symbol (read stream)))
    (read-from-string (format nil "~A:~A" name symbol))))
