(in-package "COMMON-LISP-USER")


(progn
  (defmacro html-tag (tag attributes end-tag-p &body body)
    `(progn
       ,(if end-tag-p
            `(format t "<~A~{ ~A=~S~}>" ',tag  (list ,@attributes))
            `(format t "<~A~{ ~A=~S~}/>" ',tag (list ,@attributes)))
       (let ((result (multiple-value-list (progn ,@body))))
         (when (and (null (rest result)) (stringp (first result)))
           (princ (first result)))
         ,(when end-tag-p `(format t "</~A>" ',tag))
         (values))))
         
  (defmacro do-tag (tag &optional end-tag-p)
    `(defmacro ,tag (attributes &body body)
       `(html-tag ,',tag ,attributes ,',end-tag-p ,@body)))


  (do-tag html t)
  (do-tag head t)
  (do-tag title t)
  (do-tag h3 t)
  (do-tag form t)
  (do-tag input)
  (do-tag b t)
  (do-tag i t)
  (do-tag br)
  (do-tag a t)
  (do-tag hr)

  )


;; set the commands up so that
;; ~A refers to the original, initial
;; filename -- see the one of TeX
(defvar *all-converter*
  '(
    (ps ("ps") "converts PostScript to PDF"
     "ps2pdf ~A")

    (cweb ("w") "converts CWEB to PDF"
     "cweave ~A - ~A.tex"
     "tex ~A.tex"
     "dvips ~A.dvi"
     "ps2pdf ~A.ps")

    (tex ("tex") "converts TeX to PDF"
     "cp ~A ~A.tex"
     "tex ~A.tex"
     "dvips ~A.dvi"
     "ps2pdf ~A.ps")

    (dvi ("dvi") "converts DVI to PDF"
     "cp ~A ~A.dvi"
     "dvips ~A.dvi"
     "ps2pdf ~A.ps")

    (tar-gzipped :continue ("taz" "tgz" "tar.gz"
                            "tar.Z" "tar.z" "xtar.gz"
                            "xtar.Z" "xtar.z" "nif")
     "unpacks TAR GZIPPED files"
     "gzip -dc ~A | tar -xpvf -")

    (tar-bzip2ped :continue ("tbz" "tbz2" "tar.bz2"
                             "xtar.bz2" "tar.bz")
     "unpacks TAR BZIPPED files"
     "bzip2 -dc ~A | tar -xpvf -")
    
    (tar :continue ("tar" "xtar")
     "unpacks TAR files"
     "tar -xpvf ~A")
    
    (zip :continue ("zip" "jar")
     "unpacks ZIP files"
     "unzip -o ~A")

    #+(or)
    (rpm :continue ("rpm")
     "rpm2cpio ~A | cpio -dimv --no-absolute-filenames")

    #+(or)
    (lzh :continue ("lzh")
     "lha x ~A")

    #+(or)
    (rar :continue ("rar")
     "unrar x ~A")

    (gunzip :continue ("gz" "Z")
     "unpacks GZIPPED files"
     "gunzip ~A")

    (bzip2 :continue ("bz2" "bzip2")
     "unpacks BZIPPED files"
     "bunzip2 ~A")

    (metapost ("mp")
     "converts MetaPost to PDF"
     "mptopdf ~A")
    
    
    ))


;;;
;;; misc utilities
;;;

(defvar *url-decoders*
  '(("24" . #\$)
    ("26" . #\&)
    ("2B" . #\+)
    ("2C" . #\,)
    ("2F" . #\/)
    ("3A" . #\:)
    ("3B" . #\;)
    ("3D" . #\=)
    ("3F" . #\?)
    ("40" . #\@)
    ("20" . #\Space)
    ("22" . #\")
    ("3C" . #\<)
    ("3E" . #\>)
    ("23" . #\#)
    ("25" . #\%)
    ("7B" . #\{)
    ("7D" . #\})
    ("7C" . #\|)
    ("5C" . #\\)
    ("5E" . #\^)
    ("7E" . #\~)
    ("5B" . #\[)
    ("5D" . #\])
    ("60" . #\`)
    ))


(defun decode-url (url)
  (flet ((quit () (return-from decode-url nil)))
    (with-output-to-string (out)
      (with-input-from-string (in url)
        (loop (let ((char (or (read-char in nil nil) (return))))
                (if (not (char= char #\%))
                    (princ char out)
                    (let ((next (or (read-char in nil nil) (quit))))
                      (if (char= next #\%)
                          (princ #\% out)
                          (let ((next2 (or (read-char in nil nil) (quit))))
                            (princ (or (cdr (assoc (concatenate 'string
                                                                (string next)
                                                                (string next2))
                                                   *url-decoders*
                                                   :test #'string-equal))
                                       (quit))
                                   out)))))))))))

(defun newest ()
  (let* ((entries (mapcar (lambda (x) (cons (or (file-write-date x) 0) x))
                          (directory ".")))
         (sorted (sort entries #'> :key #'car)))
    (and sorted (namestring (cdar sorted)))))


(defun tmpdir ()
  (let ((tmp (format nil "/tmp/convert-~A-~A"
                     (unix:unix-getpid)
                     (random 100000))))
    (unix:unix-mkdir tmp #o755)
    tmp))

(defvar *shell-before*)

(defun shell (cmd &rest args)
  (let ((cmd (apply #'format nil cmd args)))
    (when (boundp '*shell-before*) (funcall *shell-before* cmd))
    (ext:run-program "/bin/bash"
                     (list "-c" cmd))))

(defun ext-match (ext name)
  (and (> (length name) (1+ (length ext)))
       (string= ext name :start2 (- (length name) (length ext)))
       (char= #\. (char name (- (length name) (length ext) 1)))))



(defun decode-converter (spec)
  (let ((name (pop spec))
        (continuep (and (eq (first spec) :continue) (pop spec))))
    (values name
            continuep
            (first spec)
            (second spec)
            (rest (rest spec)))))

(defun convert-1 (name)
  (dolist (x *all-converter*)
    (multiple-value-bind (cname continuep exts doc cmds)
        (decode-converter x)
      (declare (ignore cname doc))
      (when (some (lambda (e) (ext-match e name)) exts)
        (dolist (cmd cmds)
          (let ((cmd (apply #'format nil cmd (make-list 10 ;Fixme?
                                                        :initial-element name))))
            (shell cmd)))
        (return (values (newest)
                        continuep))))))

(defun convert (name &optional zip-only)
  (flet ((handle-zip (newname)
           (ignore-errors (delete-file name))
           (setq newname (substitute #\- #\. newname))
           (shell "zip -r ~A.zip ." newname)
           (concatenate 'string newname ".zip"))
         (file-too-complicated ()
           (error "file ~S is too compilcated to do comvertion" name)))
    (dotimes (i 20 (file-too-complicated))
      (multiple-value-bind (newname continuep)
          (convert-1 name)
        (unless newname (error "couldn't find converter for ~S" name))
        (if zip-only
            (return (handle-zip newname))
            (unless continuep (return newname)))
        (setq name newname)))))


(defun data-file-link (path)
  (setq path (namestring path))
  (let* ((pos (position #\/ path :from-end t))
         (base (if pos
                   (subseq path (1+ pos))
                   path)))
    (a (:href (format nil "data/~A" base)) base)))
  


(defun cgi-main ()
  (flet ((get-params ()
           (let* ((qs (cdr (assoc :query_string ext::*environment-list*)))
                  (ps (and qs (position #\& qs)))
                  (parts (if ps
                             (list (subseq qs 0 ps) (subseq qs (1+ ps)))
                             (list qs)))
                  (params (mapcan (lambda (x)
                                    (let ((p (position #\= x)))
                                      (and p (list (cons (subseq x 0 p)
                                                         (subseq x (1+ p)))))))
                                  parts))
                  (url (cdr (assoc :url params :test #'string-equal)))
                  (zip-only (cdr (assoc :zip_only params :test #'string-equal))))
             (values (and url (plusp (length url)) (decode-url url))
                     (string-equal zip-only :on))))
         (print-cmds (cmd)
           ;; force-output to show user this info in their browser
           ;; while the command is still running -- works under Apache
           (princ "Running: ") (b () cmd) (br ()) (force-output))
         (print-cgi-header ()
           (let ((newline (coerce '(#\Return #\Newline) 'string)))
             (format t "Content-Type: text/html~A~A" newline newline))))

    (let ((original-dir (default-directory))
          (*shell-before* #'print-cmds)
          (url)
          (zip-only))
      (multiple-value-setq (url zip-only) (get-params))
      (print-cgi-header)
      (if (not url)
          (cgi-welcome)
          (letf (((default-directory) (tmpdir)))
            (html ()
              (head () (title () "convert.lisp"))
              (shell "wget -q '~A'" url)
              (multiple-value-bind (name error)
                  (ignore-errors (convert (or (newest)
                                              (error "couldn't download ~S" url))
                                          zip-only))
                (cond (error (princ "An error occurred: ")
                             (br ())
                             (b () (apply #'format nil
                                          (simple-condition-format-control
                                           error)
                                          (simple-condition-format-arguments
                                           error))))
                      (t (hr ())
                         (let ((base (subseq name
                                             (1+ (position #\/ name :from-end t)))))
                           (shell "cp ~A ~A/data/" base (namestring original-dir))
                           (data-file-link base)))))))))))
                         


(defun cgi-welcome ()
  (html ()
    (head () (title () "convert.lisp"))
    (h3 () "convert.lisp -- convert files to different formats")
    (form (:method "get" :action "convert.sh")
      (princ "url: ")
      (input (:type "textfield" :name "url" :size 100))
      (princ " ")
      (input (:type "checkbox" :name "zip_only"))
      (princ "[zip only] ")
      (input (:type "submit" :value "Convert")))
    (h3 () "available converters [they are searched in order]:")
    (dolist (x *all-converter*)
      (multiple-value-bind (name cp exts doc cmds)
          (decode-converter x)
        (declare (ignore cp cmds))
        (b () (string-downcase name))
        (princ " -- ")
        (i () doc)
        (princ " [exts: ")
        (format t "~{~A~^, ~}" exts)
        (princ "]")
        (br ())))
    (h3 () "converted files [newest first]:")
    (let ((converted-files (mapcar (lambda (x) (cons (or (file-write-date x) 0) x))
                                   (ignore-errors (directory  "data/")))))
      (dolist (x (sort converted-files #'> :key #'car))
        (data-file-link (cdr x))
        (br ())))
    (br ())
    (br ())
    (a (:href "convert.lisp-txt") "{mysrc}")))
    


(cgi-main)
(ext:quit)
