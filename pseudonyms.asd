;;;; pseudonyms.asd

(asdf:defsystem #:pseudonyms
  :description "Relative package nicknames through macros"
  :author "Michal \"phoe\" Herda"
  :license "FreeBSD (BSD 2-clause)"
  :depends-on (#:trivial-garbage)
  :serial t
  :components ((:file "package")
               (:file "pseudonyms")))
