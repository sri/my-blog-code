;;; word-find.lisp -- 
;;;
;;; Author: <sri -at- asu -dot- edu>
;;; Date: 2004-02-25T16:56:51
;;;
;;; Ideas:
;;;
;;;  o All the characters in the array should be part of a word.
;;;    Find words  that are randomly made up due to the intersection
;;;    of all the words and add it to the list of words.
;;;  o Difficulties:  either have a large matrix with small words, or so
;;;    similar words (characters)  that overlap.
;;;  o Try circular ones [for the array].
;;;
;;; [Tested and implemented under
;;;  CMU Common Lisp CVS Head 2003-10-03 00:16:55]
;;;
;;; This code is
;;;     Copyright (c) 2004 by Sriram Thaiyar.
;;; The terms are: You may do as you please with this code, as long as
;;; you do not delete this notice or hold me responsible for any outcome
;;; related to its use.

(in-package "COMMON-LISP-USER")

(unless (find-package "HTML")
  (warn "word-find.lisp requires Paul Foley's HTML package"))


(defmacro walking-along ((var array &key (direction (required-argument))
                              start-x start-y)
                         &body body)
  `(block nil
     (let ((.x. ,start-x) (.y. ,start-y))
       (symbol-macrolet ((,var (aref ,array .y. .x.)))
         (loop
           (unless (array-in-bounds-p ,array .y. .x.)
             (return))
           ,@body
           (ecase ,direction
             (:UP      (decf .y.))
             (:DOWN    (incf .y.))
             (:LEFT    (decf .x.))
             (:RIGHT   (incf .x.))
             (:DL-UP   (decf .x.) (decf .y.))
             (:DL-DOWN (decf .x.) (incf .y.))
             (:DR-UP   (incf .x.) (decf .y.))
             (:DR-DOWN (incf .x.) (incf .y.))))))
    (values)))

(defun elts-at (array x y length direction)
  (let ((elts '())
        (i -1))
    (walking-along (e array :direction direction :start-x x :start-y y)
      (when (= (incf i) length)
        (return))
      (push e elts))
    (nreverse elts)))

(defun (setf elts-at) (new array x y direction &optional (length (length new)))
  (let ((i -1) (positions '()))
    (walking-along (e array :direction direction :start-x x :start-y y)
      (when (= (incf i) length)
        (return))
      (let ((elt (elt new i)))
        (setq e elt)
        (push (list elt .x. .y.) positions)))
    positions))

(defparameter *directions*
  '(:UP :DOWN :LEFT :RIGHT :DL-UP :DL-DOWN :DR-UP :DR-DOWN))

(let ((dirs nil)
      (null-returned nil))
  (defun random-direction (&optional reset)
    (when (or reset null-returned)
      (setq dirs (copy-list *directions*)
            null-returned nil))
    (if (and (null dirs) (null null-returned))
        (progn
          (setq null-returned t)
          nil)
        (let ((dir (nth (random (length dirs)) dirs)))
          (setq dirs (delete dir dirs))
          dir)))
  )

(defparameter *colors*
  '(:RED :BLUE :GREEN :ORANGE :PURPLE :GRAY :BROWN :PINK))

(let ((cols nil))
  (defun random-color ()
    (when (null cols)
      (let ((*directions* *colors*)) ;This is code reuse ;)
        (push (random-direction t) cols)
        (loop
          (let ((next (random-direction)))
            (when (null next)
              (return))
            (push next cols)))))
    (pop cols))
  )


;;; Return values for RANDOM-FIT & FORCE-FIT:
;;;  (<seq> <col> (<c0> <x0> <y0>) ...)

(defun random-fit (seq array width height)
  (let ((x (random width))
        (y (random height))
        (len (length seq)))
    (loop
      (let ((dir (random-direction)))
        (when (null dir)
          (return nil))
        (let ((old (elts-at array x y len dir)))
          (when (and (every (complement #'characterp) old)
                     (= (length old) len))
            (return
              (list* seq
                     (random-color)
                     (setf (elts-at array x y dir) seq)))))))))

(defun force-fit (seq array)
  (let ((open-spaces '())
        (dim (array-dimensions array)))
    (dotimes (x (car dim))
      (dotimes (y (cadr dim))
        (dolist (dir *directions*)
          (let ((count 0))
            (walking-along (e array :direction dir :start-x x :start-y y)
              (if (characterp e)
                  (return)
                  (incf count)))
            (push (list count x y dir)
                  open-spaces)))))
    (setq open-spaces
          (sort open-spaces #'> :key #'first))
    (let ((len (length seq)))
      (setq open-spaces
            (delete-if (lambda (n) (< n len)) open-spaces :key #'first))
      (when open-spaces
        (let ((x (nth (random (length open-spaces)) open-spaces)))
          (list* seq
                 (random-color)
                 (setf (elts-at array (second x) (third x) (fourth x))
                       seq)))))))

(defvar *error-on-nonfit* nil)

(defun wordfind-simple (words width height)
  (setq words (sort (copy-seq words) #'< :key #'length))
  (let ((array (make-array (list height width) :initial-element nil))
        (solution '()))
    (dolist (word words)
      (dotimes (i 15 (let ((x (force-fit word array)))
                       (if x
                           (push x solution)
                           (when *error-on-nonfit*
                             (error "Unable to fit ~A in ~S" word array)))))
        (let ((x (random-fit word array width height)))
          (when x
            (push x solution)
            (return)))))
    (dotimes (i (array-total-size array))
      (unless (characterp (row-major-aref array i))
        (setf (row-major-aref array i)
              (char "ABCDEFGHIJKLMNOPQRSTUVWXYZ" (random 26))))
      (setf (row-major-aref array i)
            (char-upcase (row-major-aref array i))))
    (values array solution)))


;;; Uses Paul Foley's HTML macro package:
(defun generate-html (array solution output)
  (let ((words (mapcar #'car solution))
        (solution-pathname
         (make-pathname
          :defaults output
          :name (concatenate 'string (pathname-name output) "-sol"))))
    (flet
      ((generate-html-1 (&optional solution-p)
         (with-open-file (stream (if solution-p solution-pathname output)
                                 :direction :output
                                 :if-exists :supersede)
           (html (:stream stream)
            (head ()
             (title () (if solution-p "Word Finder: Solution" "Word Finder")))
            (body ()
             (strong () "Words: ")
             (if solution-p
                 (maplist (lambda (elts)
                            (format t "<font color=\"~A\">~A</font>~@[, ~]"
                                    (cadar elts)
                                    (string-capitalize (caar elts))
                                    (cdr elts)))
                          solution)
                 (format t "~{~A~^,&nbsp; ~}"
                         (mapcar #'string-capitalize words)))
             (br ())
             (p (:align :center)
              (table (:border t :width 100 :cellpadding 5)
               (when solution-p
                 (dolist (x solution)
                   (destructuring-bind (word color . positions)
                       x
                     (declare (ignore word))
                     (dolist (pos positions)
                       (let ((x (cadr pos)) (y (caddr pos)))
                         (unless (char-equal (car pos) (aref array y x))
                           (error "Characters don't match"))
                         (setf (aref array y x)
                               (cons color (car pos))))))))
                   (let ((dim (array-dimensions array)))
                     (dotimes (x (car dim))
                       (tr ()
                        (dotimes (y (cadr dim))
                          (td (:align :center)
                           (let ((elt (aref array y x)))
                             (if (consp elt) ;Solution and part of word
                                 (format t
                                         "<strong><font color=\"~A\">~C</font>~
                                          </strong>"
                                         (car (aref array y x))
                                         (char-upcase (cdr (aref array y x))))
                                 (format t "~C" (aref array y x)))))))))))
             (br ())
             (unless solution-p
               (p (:align :center)
                (a (:href (namestring solution-pathname)) "Solution"))))))))
      (generate-html-1)
      (generate-html-1 t)
      (values))))

(defun make-wordfind (&key (total-words 300) (width 20) (height 20)
                           (dict #p"/etc/alternatives/dictionary")
                           (output #p"word-find.html"))
  (let ((*error-on-nonfit* nil)
        (words '()))
    (if (or (stringp dict) (pathnamep dict))
        (with-open-file (stream dict)
          (let ((count 0))
            (loop
              (when (> count total-words)
                (return))
              (file-position stream (random (file-length stream)))
              (loop
                (let ((next (read-char stream nil nil)))
                  (cond ((null next) (return))
                        ((char= next #\Newline)
                         (let ((word (read-line stream nil nil)))
                           (when (> (length word) 0)
                             (push word words)
                             (incf count))
                           (return)))))))))
        (setq words dict))
    (multiple-value-bind (array solution)
        (wordfind-simple words width height)
      (generate-html array solution output))
    (values)))
