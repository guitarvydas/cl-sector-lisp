(defsystem :cl-sector-lisp
  ;;:depends-on (:alexandria)
  :around-compile (lambda (next)
                    (proclaim '(optimize (debug 3) (safety 3) (speed 0)))
                    (funcall next))
  :components ((:module "source"
                        :pathname "./"
                        :components ((:file "package")
                                     (:file "cl-sector-lisp" :depends-on ("package"))
                                     (:file "atom-memory" :depends-on ("cl-sector-lisp"))
                                     (:file "support" :depends-on ("atom-memory"))
                                     (:file "lookup-atom.asc" :depends-on ("support"))))))

