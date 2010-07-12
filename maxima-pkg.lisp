(defpackage :bld-maxima-pkg 
  (:use :common-lisp)
  (:export :simp))

(in-package :bld-maxima-pkg)

(eval-when (:compile-toplevel :load-toplevel)
  (load "c:/Documents and Settings/BDiedrich/src/cl/maxima-5.21.1/lisp-utils/defsystem.lisp")
  (load "c:/Documents and Settings/BDiedrich/src/cl/maxima-5.21.1/src/maxima.system")
  (funcall (intern (symbol-name :operate-on-system) :mk) "maxima" :load :verbose t))

(let ()
  (setf maxima::*load-verbose* nil)
  (setf *debugger-hook* #'maxima::maxima-lisp-debugger)
  (let ((input-stream maxima::*standard-input*)
	(batch-flag nil))
    (progn
      (maxima::set-readtable-for-macsyma)
      (setf maxima::*read-default-float-format* 'double-float))
    (catch 'to-lisp
      (maxima::initialize-real-and-run-time)
      (intl::setlocale)
      (maxima::set-locale-subdir)
      (maxima::adjust-character-encoding)
      (maxima::set-pathnames)
      (when (boundp 'maxima::*maxima-prefix*)
	(push (pathname (concatenate 'string maxima::*maxima-prefix* "/share/locale/"))
	      intl::*locale-directories*))
      (setf (values input-stream batch-flag)
	    (maxima::process-maxima-args input-stream batch-flag)))))

(defparameter *maxima-package* (find-package :maxima))

(defun maxima::intern-invert-case (string)
  (intern (maxima::maybe-invert-string-case string) *maxima-package*))

(defparameter *maxima-i* #C(0d0 1d0))
(defparameter *maxima-e* (exp 1d0))

(defparameter *maxima-lisp-table* (make-hash-table))
(defun init-maxima-translation-table ()
  (loop for (a b) in
       '((maxima::mplus +)
	 (maxima::mminus -)
	 (maxima::mtimes *)
	 (maxima::mexpt expt)
	 (maxima::mlist list)
	 (maxima::mequal =)
	 (maxima::mgreaterp >)
	 (maxima::mabs abs)
	 (maxima::rat /)
	 (maxima::%sin sin)
	 (maxima::%cos cos)
	 (maxima::%tan tan)
	 (maxima::%exp exp)
	 (maxima::%log log)
	 (maxima::%sqrt sqrt)
	 (maxima::%sinh sinh)
	 (maxima::%cosh cosh)
	 (maxima::%atan atan)
	 (maxima::%atan2 atan2)
	 (maxima::%signum signum)
	 (maxima::%max max))
     do (setf (gethash a *maxima-lisp-table*) b)
       (setf (gethash b *maxima-lisp-table*) a)))
(init-maxima-translation-table)

(defun translate-to-maxima (expr)
  (cond ((atom expr)
	 expr)
	(t
	 (let* ((head (first expr))
		(head-translated (gethash head *maxima-lisp-table*)))
	   ;; intern unknown symbols & add to table as merely appending $ to their name
	   ;; e.g. for Lisp functions without a counterpart in Maxima
	   (when (null head-translated)
	     (let ((translation
		    (intern (concatenate 'string "$" (string head))
			    *maxima-package*)))
	       (setf (gethash head *maxima-lisp-table*) translation)
	       (setf (gethash translation *maxima-lisp-table*) head)
	       (setq head-translated translation)))
	   (maxima::mfuncall 'maxima::$trigsimp
			     (maxima::simplify
			      (cons (list head-translated)
				    (mapcar #'translate-to-maxima (cdr expr)))))))))

(defun translate-from-maxima (expr)
  (cond ((atom expr)
	 (case expr
	   (maxima::$%i *maxima-i*)
	   (maxima::$%e *maxima-e*)
	   (maxima::$%pi pi)
	   (t expr)))
	((consp (car expr))
	 (let ((operator (caar expr)))
	   (let ((head-translated (gethash operator *maxima-lisp-table*)))
	     (when (null head-translated)
	       (error "no back translation for ~s." operator))
	     (cons head-translated
		   (mapcar #'translate-from-maxima (cdr expr))))))
	(t (error "not implemented"))))

(defun simp (expr)
  (translate-from-maxima (translate-to-maxima expr)))
