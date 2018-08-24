;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:cl-hdl-asd
  (:use :cl :asdf))

(in-package :cl-hdl-asd)

(defsystem cl-hdl
  :name "cl-hdl"
  :version "0.0.0"
  :maintainer "fouric"
  :author "fouric"
  :license "All rights reserved"
  :description "DESCRIPTION HERE"

  :serial t
  :depends-on ()
  :pathname "src"
  :components ((:file "package")
               (:file "util" :depends-on ("package"))
               (:file "cl-hdl" :depends-on ("package" "util"))))
