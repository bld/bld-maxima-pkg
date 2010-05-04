(defpackage :bld-maxima-pkg.system
  (:use :asdf :cl))
(in-package :bld-maxima-pkg.system)
(defsystem :bld-maxima-pkg
    :name "bld-maxima-pkg"
    :author "Benjamin L. Diedrich <ben@solarsails.info>"
    :version "0.0.1"
    :maintainer "Benjamin L. Diedrich <ben@solarsails.info>"
    :license "GPL"
    :description "Load Maxima as a library and provide interface"
    :components 
    ((:file "maxima-pkg"))
    :depends-on ("maxima"))

