;;;
;;; Name        : lisp2.lisp
;;; Author      : Vijay Ramakrishna
;;; Date        : 02016.11.21
;;; Description : This file contains the program #lisp2. This program
;;; generates 3 different functions, arithmetic, used to solve an
;;; expression, given inputs. The second is carcdr, which continuously calls
;;; car and cdr on a list until the list is empty. The last function is
;;; use-and-and-atom, which is used to analyze the contents of a list.
;;; These 3 functions are defined and then called in this file, using 
;;; pre-existing data sets for each of the functions.
;;;

(defun arithmetic (a m s d)
  (if (= d 0) 'NIL (/ (- (+ (* (- m 1) m) a) s) d))
)

(defun carcdr (alist)
  (format t "(car '~a) = " alist)(format t "~a~%" (car alist))
  (format t "(cdr '~a) = " alist)(format t "~a~%" (cdr alist))
  (if (> (length alist) 1)(carcdr (cdr alist)))
)

(defun use-and-and-atom (alist)
  (let ((evencnt 0) (atomcnt 0) (listcnt 0))
    (format t "list ~a has ~a elements~%" alist (length alist))
    (setf evencnt (loop for n in alist count (if (integerp n)(and (evenp n)(atom n)))))
    (setf atomcnt (loop for n in alist count (atom n)))  
    (format t "...~a of the ~a atom elements are even numbers~%" evencnt atomcnt)
    (setf listcnt (loop for n in alist count (typep n 'list)))
    (format t "...~a of the elements are lists~%" listcnt)
  )
)


(format t "***PART 0 (arithmetic)***~%");
(format t "~d ~d ~d~%" (arithmetic 2 4 5 3) 
                       (arithmetic 3 1 3 8)
                       (arithmetic 2 5 6 0)) 

(format t "~%***PART 1 (carcdr)***~%");
(carcdr '(5 8 13 21 34))

(format t "~%***PART 2 (use-and-and-atom)***~%");
(use-and-and-atom '(5 (3 6) 8 (1 1 1) 9 4 (21) 8))
(use-and-and-atom '((red green blue) stop 5 2 (no go) 7))
