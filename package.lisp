;;;; package.lisp

(defpackage #:pseudonyms
  (:use #:cl #:trivial-garbage)
  (:export :*pseudonym-table*
	   :defpseudonym
	   :pmakunbound
	   :print-pseudonyms
	   :pseudonym-reader)
  (:documentation
   "============================ PSEUDONYMS ===================================

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
> (defpseudonym \"longpackagename\" \"lpn\")
And removed like this:
> (pmakunbound \"lpn\") ;; OR (pmakunbound \"longpackagename\")

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

"))
