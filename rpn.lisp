;;;-----------------------------------------------------------------------------
;;; rpn lisp calculator
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
(in-package #:com.indyeng.rpn)

;;;-----------------------------------------------------------------------------
;;; Extra math operations
(defparameter deg2rad (/ pi 180.0))
(defparameter rad2deg (/ 180.0 pi))
(defparameter ft2m 0.3048)
(defparameter lb2kg 0.45359237)
(defparameter gc 32.174)
(defparameter g 9.80665)

(defun inc (x)
  "return `x' plus 1."
  (+ x 1))

(defun dec (x)
  "return `x' minus 1."
  (- x 1))

(defun sign (x)
  "return sign of `x'"
  (if (< x 0.0)
      -1.0
      1.0))

(defun sq (x)
  "return square of `x'"
  (* x x))

(defun inv (x)
  "return inverse of `x'"
  (/ 1.0 x))

(defun log10 (x)
  "return log base 10 of `x'"
  (log x 10))

(defun atan2 (y x)
  "return arc-tangent 4 quadrant."
  (if (and (zerop x) (zerop y))
      'error
      (if (> (abs x) (abs y))
          (if (> x 0.0)
              (atan (/ y x))
              (+ (atan (/ y x)) (* pi (sign y))))
          (- (* (/ pi 2) (sign y)) (atan (/ x y))))))

(defun mean (&rest vals)
  "return average of input values."
  (/ (apply #'+ vals) (length vals)))

(defun factorial (x &optional (prod 1))
  "return `x' factorial."
  (if (> x 1)
      (factorial (- x 1) (* prod x))
      prod))

;;;-----------------------------------------------------------------------------
;;; Fun stuff
(defun rpn-time-check (n)
  "run dotimes `n' times and report time."
  (labels ((f (n) (dotimes (i n nil) (factorial 100) )))
    (time (f n))))

;;;-----------------------------------------------------------------------------
;;; Operator table
;;; improvements?
;;; 1. turn this into a hash-table
;;; 2. allow rule such that we can have a function return multiple
;;;    values; e.g. make mod take two items off the stack, and push the
;;;    remainder then the mod onto the stack
;;; 3. create an expandle a-list to hold stored parameters
;;;    - maybe 2 'a sto; which can be used later as 'a rcl
;;; 4. allow an alias for a chain of commands, like ((f . constant) (/ . 2))
;;; 5. allow functions to request the number, like 1 2 3 4 2 sumn would
;;;    take 2 as the number of stack items to pull, and then add 3 and 4.
;
;;; an idea for #5
;; maybe rpn-table should be of form (function type #args)
;; where type is either constant, conversion, alias, stack-operation, operation
;; e.g. (sumn function 1) -- sumn function itself handles poping the n-args off the stack
(defparameter *rpn-table*
  '((pi . constant)
    (gc . constant)
    (g . constant)
    (ft2m . constant)
    (lb2kg . constant)
    (deg2rad . conversion)
    (rad2deg . conversion)
    (float . 1)
    (+ . 2)
    (- . 2)
    (* . 2)
    (x . (* . 2)) ; alias for *
    (/ . 2)
    (sqrt . 1)
    (sq . 1)
    (inv . 1)
    (abs . 1)
    (sin . 1)
    (cos . 1)
    (tan . 1)
    (asin . 1)
    (acos . 1)
    (atan . 1)
    (atan2 . 2)
    (sinh . 1)
    (cosh . 1)
    (tanh . 1)
    (asinh . 1)
    (acosh . 1)
    (atanh . 1)
    (exp . 1)
    (expt . 2)
    (pow . (expt . 2))
    (^ . (expt . 2))
    (ln . (log . 1))
    (log10 . 1)
    (log . (log . 2))
    (mod . 2)
    (rem . 2)
    (min . 2)
    (max . 2)
    (sum . (+ . all))
    (mean . all)
    (prod . (* . all))
    (! . (factorial . 1))
    (fac . (factorial . 1))
    (inc . 1)
    (dec . 1)
    (swap . swap)
    (xy . swap)
    (show . (rpn-show-stack . 0))
    (ent . (rpn-last-value . 0))
    (clear . (rpn-clear-stack . 0))
    (time-check . (rpn-time-check . 1))
    (quit . (uiop:quit . 0)))
  "RPN Calculator rule table.")

(defun rpn-op-p (op)
  "return nil if this is not a recongnized RPN operator; otherwise
return the number of required arguments."
  (let ((res (assoc op *rpn-table*)))
    (if res
        (cdr res)
        nil)))

;;;-----------------------------------------------------------------------------
;;; Calculation stack
(defvar *rpn-stack* (cons nil 0)
  "RPN Calculator value stack.")

(defun rpn-clear-stack ()
  "clear the stack."
  (setf (symbol-value '*rpn-stack*) (cons nil 0)) nil)

(defun rpn-stack-count ()
  "return current size of stack."
  (cdr *rpn-stack*))

(defun rpn-last-value ()
  "return last value on the stack."
  (first (first *rpn-stack*)))

(defun rpn-show-stack ()
  "print the stack."
  (format t "stack-> ~A~%" (first *rpn-stack*)))

(defun rpn-push (val)
  "push `val' onto stack."
  (incf (cdr *rpn-stack*))
  (push val (first *rpn-stack*))
  val)

(defun rpn-pop (&optional (n 1))
  "pop `n' values from stack."
  (if (or (= n 0) (> n (rpn-stack-count)))
      nil
      (do ((i 0 (+ i 1))
           (res nil (push (pop (car *rpn-stack*)) res)))
          ((= i n) (and (decf (cdr *rpn-stack*) n) res)))))

(defun rpn-swap ()
  "swap last two values in stack."
  (when (> (rpn-stack-count) 1)
    (let ((x (first (rpn-pop)))
          (y (first (rpn-pop))))
      (rpn-push x)
      (rpn-push y)
      nil)))

;;;-----------------------------------------------------------------------------
;;; Calculator
(defun rpn-apply-op (op nop)
  "apply `op' to `nop' arguments."
  (let ((res (apply op (rpn-pop nop))))
    (when res
      (rpn-push res))))

(defun rpn-eval (op &optional (op-type nil))
  "RPN calculator evaluation."
  ;; (format t "~% Hello ~A : ~A : stack= ~A~%" op op-type *rpn-stack*)
  (cond ((numberp op) (rpn-push op))
        ((setf op-type (or op-type (rpn-op-p op)))
         ;; (format t "~% setf op-type ~A~%" op-type)
         (cond ((numberp op-type) (rpn-apply-op op op-type))
               ((eql op-type 'all) (rpn-apply-op op (rpn-stack-count)))
               ((eql op-type 'constant) (rpn-push (eval op)))
               ((eql op-type 'conversion) (rpn-push (eval op)) (rpn-eval '* 2))
               ((eql op-type 'swap) (rpn-swap))
               ((listp op-type) (rpn-eval (car op-type) (cdr op-type)))
               (t nil)))
        (t (format t "~% Unrecognized argument: ~A . ~A~%" op op-type))))

(defun rpn-parse-line (line)
  "turn input `line' (string) into a list of read processed items."
  (do ((*read-eval* nil)
       (len (length line))
       (start 0)
       (res nil))
      ((= start len) (reverse res))
    (multiple-value-bind (arg iloc)
        (read-from-string line nil nil :start start)
      (push arg res)
      (setf start iloc))))

;;;-----------------------------------------------------------------------------
;;; Public
(defun rpn (&rest args)
  "loop through all arguments."
  (dolist (arg args (rpn-last-value))
    (if (listp arg)
        (mapcar #'rpn arg)
        (rpn-eval arg))))

(defun rpn-repl ()
  "RPN Calculator REPL."
  (loop
     (format t "~%rpn> ") (finish-output)
     (let ((res (rpn (rpn-parse-line (read-line)))))
       (when res (format t "~A~%" res)))))

;;;-----------------------------------------------------------------------------
;;; Executable
(defun rpn-exe ()
  "toplevel for external call."
  (let ((*package* (find-package :com.indyeng.rpn)))
    (let ((aline (format nil "~{~a~^ ~}" (cdr (uiop:command-line-arguments)))))
      (if (zerop (length aline))
          (rpn-repl)
          (format t "~A" (rpn (rpn-parse-line aline)))))))

;;;-----------------------------------------------------------------------------
;;; End
