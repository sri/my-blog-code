;;; This version of the MD5 algorithm implementation
;;; is made mainly for human consumption.
;;; It maps almost 1-to-1 to the detailed algorithm given
;;; in RFC 1321 (and doesn't follow the provided sample
;;; implementation that is provided there).
;;;
;;; [some parts copied from Pierre Mai's md5.lisp]
;;; 2006-11-09T00:39:01, sri

;;; released under mit license

;; converts an ordinary integer to a 8-bit binary "list"
;; bits are represented by fixnums 0 and 1
(defun decimal-int-to-binary (int)
  (let ((list nil))
    (loop
      (multiple-value-bind (q r) (floor int 2)
        (when (zerop q)
          (push r list)
          (dotimes (i (- 8 (mod (length list) 8)))
            (push 0 list))
          (return list))
        (push r list)
        (setq int q)))))

;; converts seq of ascii chars to a binary "list"
(defun convert-ascii-string-to-bit-list (string)
  (mapcan #'decimal-int-to-binary (map 'list #'char-int string)))

(defun mod32+ (x y) (ldb (byte 32 0) (+ x y)))

;; ??
(defun rol32 (a s)
  (logior (ldb (byte 32 0) (ash a s)) (ash a (- s 32))))
  

(defun F (x y z) (logior (logand x y) (logandc1 x z)))
(defun G (x y z) (logior (logand x z) (logandc2 y z)))
(defun H (x y z) (logxor x y z))
(defun I (x y z) (logxor y (logorc2 x z)))

;; a single round
(defmacro R (a b c d k s i fn)
  `(setq ,a (mod32+ ,b
                    (rol32 (mod32+ (mod32+ ,a (,fn ,b ,c ,d))
                                   (mod32+ (nth ,k X)
                                           (nth (1- ,i) table-T)))
                           ,s))))


(defun hex-it (a b c d)
  (let ((int-array (make-array 16 :element-type '(unsigned-byte 8)))
        (i -1))
    (macrolet ((frob (V)
                 `(setf (aref int-array (incf i)) (ldb (byte 8  0) ,V)
                        (aref int-array (incf i)) (ldb (byte 8  8) ,V)
                        (aref int-array (incf i)) (ldb (byte 8 16) ,V)
                        (aref int-array (incf i)) (ldb (byte 8 24) ,V))))
      (frob a)
      (frob b)
      (frob c)
      (frob d))
    (format nil "~(~{~2,'0X~}~)"
            (map 'list #'identity int-array))))


(defun md5 (string)
  (let* ((bits (convert-ascii-string-to-bit-list string))
         (len (length bits)))
    
    (setq bits (append bits (list 1)))
    (loop until (= 448 (mod (length bits) 512)) do
          (setq bits (append bits (list 0))))

    (let ((rep (decimal-int-to-binary len))
          (lo)
          (hi))
      
      (loop while (> (length rep) 64) do (pop rep))
      (loop until (= (length rep) 64) do (push 0 rep))
      (setq lo (subseq rep 32 64)
            hi (subseq rep  0 32))

      (setq bits (append bits
                         ;; append bits length
                         ;; as 64-bit little endian
                         (subseq lo 24 32)
                         (subseq lo 16 24)
                         (subseq lo  8 16)
                         (subseq lo  0  8)
                         (subseq hi 24 32)
                         (subseq hi 16 24)
                         (subseq hi  8 16)
                         (subseq hi  0  8))))
    
    (assert (zerop (mod (length bits) 512)))

    (let ((a #X67452301)
          (b #XEFCDAB89)
          (c #X98BADCFE)
          (d #X10325476)

          (aa) (bb) (cc) (dd)
          
          (N (/ (length bits) 32))
          (X (make-list 16))
          (table-T (loop for i from 1 to 64
                         collect
                         (truncate (* 4294967296
                                      (abs (sin (float i 0.0d0))))))))
      
      (dotimes (i (/ N 16))
        ;;
        ;; M[N] locates a *word* in M --
        ;; we need to convert all bits to bytes
        ;; and then to words. (remember, when
        ;; converting bytes to words -- the
        ;; least significant byte comes first!)
        ;;
        (let ((bytes
               (loop with start-idx = (* i 512)       ; which 512-th block?
                     for byte-count below 64          ; 64 bytes in a block
                     collect
                     (let ((start-byte (+ start-idx (* byte-count 8)))
                           (ans 0))
                       (do ((e 7 (1- e)))
                           ((< e 0) ans)
                         (incf ans (* (expt 2 e)
                                      (nth (+ start-byte (- 7 e)) bits))))))))

          (dotimes (j 16)
            (setf (nth j X)
                  (let ((next (* j 4)))
                    (logior (ash (nth (+ next 0) bytes)  0)
                            (ash (nth (+ next 1) bytes)  8)
                            (ash (nth (+ next 2) bytes) 16)
                            (ash (nth (+ next 3) bytes) 24))))))

        
        (setq aa a  bb b  cc c  dd d)
        ;; Round 1
        (R a b c d  0  7  1 F) (R d a b c  1 12  2 F) (R c d a b  2 17  3 F) (R b c d a  3 22  4 F)
        (R a b c d  4  7  5 F) (R d a b c  5 12  6 F) (R c d a b  6 17  7 F) (R b c d a  7 22  8 F)
        (R a b c d  8  7  9 F) (R d a b c  9 12 10 F) (R c d a b 10 17 11 F) (R b c d a 11 22 12 F)
        (R a b c d 12  7 13 F) (R d a b c 13 12 14 F) (R c d a b 14 17 15 F) (R b c d a 15 22 16 F)
        ;; Round 2
        (R a b c d  1  5 17 G) (R d a b c  6  9 18 G) (R c d a b 11 14 19 G) (R b c d a  0 20 20 G)
        (R a b c d  5  5 21 G) (R d a b c 10  9 22 G) (R c d a b 15 14 23 G) (R b c d a  4 20 24 G)
        (R a b c d  9  5 25 G) (R d a b c 14  9 26 G) (R c d a b  3 14 27 G) (R b c d a  8 20 28 G)
        (R a b c d 13  5 29 G) (R d a b c  2  9 30 G) (R c d a b  7 14 31 G) (R b c d a 12 20 32 G)
        ;; Round 3
        (R a b c d  5  4 33 H) (R d a b c  8 11 34 H) (R c d a b 11 16 35 H) (R b c d a 14 23 36 H)
        (R a b c d  1  4 37 H) (R d a b c  4 11 38 H) (R c d a b  7 16 39 H) (R b c d a 10 23 40 H)
        (R a b c d 13  4 41 H) (R d a b c  0 11 42 H) (R c d a b  3 16 43 H) (R b c d a  6 23 44 H)
        (R a b c d  9  4 45 H) (R d a b c 12 11 46 H) (R c d a b 15 16 47 H) (R b c d a  2 23 48 H)
        ;; Round 4
        (R a b c d  0  6 49 I) (R d a b c  7 10 50 I) (R c d a b 14 15 51 I) (R b c d a  5 21 52 I)
        (R a b c d 12  6 53 I) (R d a b c  3 10 54 I) (R c d a b 10 15 55 I) (R b c d a  1 21 56 I)
        (R a b c d  8  6 57 I) (R d a b c 15 10 58 I) (R c d a b  6 15 59 I) (R b c d a 13 21 60 I)
        (R a b c d  4  6 61 I) (R d a b c 11 10 62 I) (R c d a b  2 15 63 I) (R b c d a  9 21 64 I)

        (setq a (mod32+ a aa)
              b (mod32+ b bb)
              c (mod32+ c cc)
              d (mod32+ d dd)))

      (hex-it a b c d)

      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-me ()
  (let ((x
         (list
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; RFC 1321 test suite:
          ""                           "d41d8cd98f00b204e9800998ecf8427e"
          "a"                          "0cc175b9c0f1b6a831c399e269772661"
          "abc"                        "900150983cd24fb0d6963f7d28e17f72"
          "message digest"             "f96b697d7cb7938d525a2f31aaf161d0"
          "abcdefghijklmnopqrstuvwxyz" "c3fcd3d76192e4007dfb496cca67e13b"

          "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
                                       "d174ab98d277d9f5a5611c2c9f419d9f"
          (concatenate 'string
                       "1234567890123456789012345678901234567890"
                       "1234567890123456789012345678901234567890")
                                       "57edf4a22be3c955ac49da2e2107b67a"

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; additional tests from md5.lisp:
          "1631901HERR BUCHHEISTERCITROEN NORD1043360796beckenbauer" 
          "d734945e5930bb28859ccd13c830358b"
          
          ""
          "d41d8cd98f00b204e9800998ecf8427e"
          
          "a"
          "0cc175b9c0f1b6a831c399e269772661"
          
          "aa"
          "4124bc0a9335c27f086f24ba207a4912"
          
          "aaa"
          "47bce5c74f589f4867dbd57e9ca9f808"
          
          "aaaa"
          "74b87337454200d4d33f80c4663dc5e5"
          
          "aaaaa"
          "594f803b380a41396ed63dca39503542"
          
          "aaaaaa"
          "0b4e7a0e5fe84ad35fb5f95b9ceeac79"
          
          "aaaaaaa"
          "5d793fc5b00a2348c3fb9ab59e5ca98a"
          
          "aaaaaaaa"
          "3dbe00a167653a1aaee01d93e77e730e"
          
          "aaaaaaaaa"
          "552e6a97297c53e592208cf97fbb3b60"
          
          "aaaaaaaaaa"
          "e09c80c42fda55f9d992e59ca6b3307d"
          
          "aaaaaaaaaaa"
          "d57f21e6a273781dbf8b7657940f3b03"
          
          "aaaaaaaaaaaa"
          "45e4812014d83dde5666ebdf5a8ed1ed"
          
          "aaaaaaaaaaaaa"
          "c162de19c4c3731ca3428769d0cd593d"
          
          "aaaaaaaaaaaaaa"
          "451599a5f9afa91a0f2097040a796f3d"
          
          "aaaaaaaaaaaaaaa"
          "12f9cf6998d52dbe773b06f848bb3608"
          
          "aaaaaaaaaaaaaaaa"
          "23ca472302f49b3ea5592b146a312da0"
          
          "aaaaaaaaaaaaaaaaa"
          "88e42e96cc71151b6e1938a1699b0a27"
          
          "aaaaaaaaaaaaaaaaaa"
          "2c60c24e7087e18e45055a33f9a5be91"
          
          "aaaaaaaaaaaaaaaaaaa"
          "639d76897485360b3147e66e0a8a3d6c"
          
          "aaaaaaaaaaaaaaaaaaaa"
          "22d42eb002cefa81e9ad604ea57bc01d"
          
          "aaaaaaaaaaaaaaaaaaaaa"
          "bd049f221af82804c5a2826809337c9b"
          
          "aaaaaaaaaaaaaaaaaaaaaa"
          "ff49cfac3968dbce26ebe7d4823e58bd"
          
          "aaaaaaaaaaaaaaaaaaaaaaa"
          "d95dbfee231e34cccb8c04444412ed7d"
          
          "aaaaaaaaaaaaaaaaaaaaaaaa"
          "40edae4bad0e5bf6d6c2dc5615a86afb"
          
          "aaaaaaaaaaaaaaaaaaaaaaaaa"
          "a5a8bfa3962f49330227955e24a2e67c"
          
          "aaaaaaaaaaaaaaaaaaaaaaaaaa"
          "ae791f19bdf77357ff10bb6b0e97e121"
          
          "aaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "aaab9c59a88bf0bdfcb170546c5459d6"
          
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "b0f0545856af1a340acdedce23c54b97"
          
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "f7ce3d7d44f3342107d884bfa90c966a"
          
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "59e794d45697b360e18ba972bada0123"
          
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "3b0845db57c200be6052466f87b2198a"
          
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "5eca9bd3eb07c006cd43ae48dfde7fd3"
          
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "b4f13cb081e412f44e99742cb128a1a5"
          
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "4c660346451b8cf91ef50f4634458d41"
          
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "11db24dc3f6c2145701db08625dd6d76"
     
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "80dad3aad8584778352c68ab06250327"
          
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"     
          "1227fe415e79db47285cb2689c93963f"
     
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "8e084f489f1bdf08c39f98ff6447ce6d"
     
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "08b2f2b0864bac1ba1585043362cbec9"
     
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "4697843037d962f62a5a429e611e0f5f"
     
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "10c4da18575c092b486f8ab96c01c02f"
     
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "af205d729450b663f48b11d839a1c8df"
     
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "0d3f91798fac6ee279ec2485b25f1124"
     
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "4c3c7c067634daec9716a80ea886d123"
     
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "d1e358e6e3b707282cdd06e919f7e08c"
     
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "8c6ded4f0af86e0a7e301f8a716c4363"
     
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "4c2d8bcb02d982d7cb77f649c0a2dea8"
     
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "bdb662f765cd310f2a547cab1cfecef6"
     
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "08ff5f7301d30200ab89169f6afdb7af"
     
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "6eb6a030bcce166534b95bc2ab45d9cf"
     
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "1bb77918e5695c944be02c16ae29b25e"
     
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "b6fe77c19f0f0f4946c761d62585bfea"
     
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "e9e7e260dce84ffa6e0e7eb5fd9d37fc"
     
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "eced9e0b81ef2bba605cbc5e2e76a1d0"
     
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "ef1772b6dff9a122358552954ad0df65"
     
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "3b0c8ac703f828b04c6c197006d17218"
     
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "652b906d60af96844ebd21b674f35e93"
     
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "dc2f2f2462a0d72358b2f99389458606"
     
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "762fc2665994b217c52c3c2eb7d9f406"
     
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "cc7ed669cf88f201c3297c6a91e1d18d"
     
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "cced11f7bbbffea2f718903216643648"
     
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "24612f0ce2c9d2cf2b022ef1e027a54f"
     
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "b06521f39153d618550606be297466d5"
     
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "014842d480b571495a4a0363793f7367"
     
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "c743a45e0d2e6a95cb859adae0248435"
     
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "def5d97e01e1219fb2fc8da6c4d6ba2f"
     
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "92cb737f8687ccb93022fdb411a77cca"
     
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "a0d1395c7fb36247bfe2d49376d9d133"
     
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "ab75504250558b788f99d1ebd219abf2")))

    (loop for (in expect . rest) on x by #'cddr
          if (string= (md5 in) expect)
           do (princ #\.)
          else
           do (princ #\F))
    (princ #\newline)
    (values)))
