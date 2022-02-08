(defsystem :cl-sector-lisp
  ;;:depends-on (:alexandria)
  :around-compile (lambda (next)
                    (proclaim '(optimize (debug 3) (safety 3) (speed 0)))
                    (funcall next))
  :components ((:module "source"
                        :pathname "./"
                        :components ((:file "package")
                                     (:file "cl-sector-lisp" :depends-on ("package"))))))

