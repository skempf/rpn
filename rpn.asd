;;;-----------------------------------------------------------------------------
;;; rpn.asd -- asdf system definition for rpn
;;; 
;;; Copyright (C) 2019 Severin Kempf skempf@indyeng.com
;;; 
;;; Permission to use, copy, modify, and/or distribute this software for any
;;; purpose with or without fee is hereby granted, provided that the above
;;; copyright notice and this permission notice appear in all copies.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;;

(in-package :asdf)

;;;-----------------------------------------------------------------------------
(defsystem "rpn"
  :name "RPN"
  :version "0.1.0"
  :description "RPN Calculator"
  :long-description "RPN Calculator implemented in Lisp."
  :author "Severin Kempf skempf@indyeng.com"
  :maintainer "Severin Kempf skempf@indyeng.com"
  :license "ISC"
  :depends-on nil
  :serial t
  :components ((:file "package")
               (:file "rpn"))
  :build-operation "program-op"
  :build-pathname "bin/rpn"
  :entry-point "rpn::rpn-main")

;;;-----------------------------------------------------------------------------
;;; end
