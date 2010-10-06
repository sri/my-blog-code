;;; lynx.el -- an interface to the Lynx text browser

;; Author: Sriram Thaiyar <sri -at- asu.edu>
;; Last-modified: 2004-07-28T14:10:04

;; Features:
;; ========
;;
;; o navigation via keywords:  (lynx-fetch 'cnn) or (lynx-fetch "cnn")
;;   will fetch "http://www.cnn.com" if `lynx-execute-actions-p' is
;;   non-NIL...
;; o runs a macro or function when a certain url is fetched:
;;   For example,
;;
;;     ;; cnn.com headline begins with a non-space (the first line to do that):
;;     (def-lynx-action (cnn "http://www.cnn.com") ()
;;       (if (re-search-forward "^\\[" nil t)
;;           (beginning-of-line)))
;;
;;   will fetch "http://www.cnn.com" and display the headline
;;   as the first line of the window (the above will run when
;;   "http://www.cnn.com" is fetched).
;;
;; o Files whose extension appears in AUTO-MODE-ALIST,
;;   will put you in that mode.
;;
;; o Visiting mailto links will let you compose email.
;;
;; o Recordable webpages -- record keystrokes that'll
;;   be executed after the page is retrieved.
;;   [With a user option set to T, do this silently
;;    without bothering the user.]
;;
;; o List buffer refs that match a certain url.
;;   [Also, list all urls.]
;;
;; Ideas:
;; =====
;;
;; o turns off matching-paren feature automatically [should
;;   this be done automatically] Does this currently.
;;
;; o keymap for mouse clicks
;; o [syntax-tables -- what are they?]
;; o .tar.gz files -> download
;; o Recordable webpages -- record keystrokes that'll
;;   be executed after the page is retrieved.
;;   [With a user option set to T, do this silently
;;    behind the user's back.]
;; o C-c C-<letter> keys that'll be bound to
;;   known websites; they go to a particular link or
;;   search for a particular phrase and goes to that
;;   location
;; o After viewing an image, automatically
;;   moves point to the next link...moving to
;;   the next link is determined by how
;;   the user has previously moved....
;; o handling forms -- would be great
;;   [maybe have user define a list of them.]
;; o Imitate features of other browsers (how to open tab
;;   in Emacs -- other window??)
;; o Refresh after a certain number of seconds....
;;

;; lynx.el "...is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details."

;;; Code:


(require 'regexp-opt)
(require 'rx)
(require 'cl)
(require 'timer)
(require 'thingatpt)
(require 'ffap)

(defvar lynx-buffer-name
  (function
    (lambda (name)
      (if (string-match (rx (and string-start (or "http://" "ftp://"))) name)
          (substring name (match-end 0))
        name))))

;;; Should maybe ask to user before modifying this...
(make-variable-buffer-local
 'show-paren-mode)
(setq-default show-paren-mode t)

(defvar lynx-hands-off-links nil)

;;; Not really a good idea because user might want to keep a certain buffer
;;; around for a long time.  (An important buffer/URL might get deleted and
;;; it is too much trouble to specify if each URL is important (don't
;;; delete) or not.)

;(defvar lynx-kill-buffer-after 300.0
;  "If non-NIL, this should specify the amount of time (seconds)
;after which lynx-mode buffers are destroyed.")

;;; If non-NIL, execute associated actions when a particular URL loads up.
(defvar lynx-execute-actions-p t)

(defvar lynx-mode-map
  (let ((map (make-sparse-keymap)))
    (mapcar
     (lambda (x)
       (let ((key (if (stringp (car x)) (read-kbd-macro (car x)) (car x))))
         (define-key map key (cdr x))))
     '(("0"     . digit-argument)
       ("1"     . digit-argument)
       ("2"     . digit-argument)
       ("3"     . digit-argument)
       ("4"     . digit-argument)
       ("5"     . digit-argument)
       ("6"     . digit-argument)
       ("7"     . digit-argument)
       ("8"     . digit-argument)
       ("9"     . digit-argument)
       ("a"     . lynx-absolute-link)
       ("b"     . lynx-backward-follow)
       ("d"     . lynx-download-url)
       ("c"     . lynx-show-buffer-url)
       ("e"     . lynx-record-action)
       ("f"     . lynx-forward-follow)
       ("F"     . lynx-follow-url-in-other-window)
       ("h"     . lynx-fetch-in-other-window)
       ("g"     . lynx-google)
       ("G"     . lynx-google-groups)
       ("k"     . lynx-copy-current-link-as-kill)
       ("n"     . lynx-next-link)
       ("m"     . lynx-list-matching-urls)
       ("o"     . lynx-fetch)
       ("p"     . lynx-previous-link)
       ("q"     . bury-buffer)
       ("r"     . isearch-backward)
       ("s"     . isearch-forward)
       ("u"     . lynx-up-directory)
       ("-"     . digit-argument)
       ("SPC"   . scroll-up)
       ("<"     . beginning-of-buffer)
       (">"     . end-of-buffer)
       ("TAB"   . lynx-next-link)
       ("RET"   . lynx-follow-url)
       (")"     . lynx-record-action)
       ([left]  . lynx-backward-history)
       ;;
       ;;
       ("C-c C-c" . lynx-finish-kbd-macro)
       ;; Should be from where user clicked...
       ([down-mouse-1] . lynx-forward-follow)
       ([right] . lynx-forward-history)))
    map)
  "Keymap for `lynx-mode'.")


;;; Misc internal variables

(defvar lynx-buffer-url nil)
(make-variable-buffer-local
 'lynx-buffer-url)

(defvar lynx-buffer-references nil)
(make-variable-buffer-local
 'lynx-buffer-references)

(defface lynx-current-link-face
  '((t :background "black" :foreground "red"))
  "")

(defface lynx-link-face
  '((t :background "black" :foreground "blue"))
  "")

(defface lynx-mouse-over-link-face
  '((t :background "black"))
  "")


(define-derived-mode lynx-mode fundamental-mode "Lynx"
  "Major mode for viewing URLs using Lynx.
\\{lynx-mode-map}"
  (goto-char (point-min))
  (save-excursion
    (let (buffer-read-only)
      (unless lynx-hands-off-links
        (lynx-make-references)
        (lynx-highlight-links))))
  (set-buffer-modified-p nil)
  (setq show-paren-mode nil)
  (setq buffer-read-only t))

;;; Default options for invoking Lynx:

(defvar lynx-default-options
  (list "-dump" "-hiddenlinks=ignore" "-image_links"
        "-connect_timeout=30"))

(defun lynx-exec (url)
  (zerop (apply 'call-process "lynx" nil t nil url lynx-default-options)))

(defun lynx-make-references ()
  (let ((refs '()))
    (goto-char (point-max))
    (when (re-search-backward "^References$" nil t)
      (let ((start (point)))
        (while (re-search-forward "^ *[0-9]+\\. " nil t)
          (push (buffer-substring (point) (progn (end-of-line) (point)))
                refs))
        (delete-region start (point-max))))
    (setq lynx-buffer-references
          (vconcat (nreverse refs)))))

(defun lynx-highlight-links ()
  (let ((n 0)
        (end (length lynx-buffer-references)))
    (goto-char (point-min))
    (while (< n end)
      (when (re-search-forward (format "\\[\n?[ \t]*%d\n?[ \t]*\\]" (incf n))
                               nil t)
        (let ((start (save-excursion (search-backward "["))))
          (lynx-associate-link start (point) n
            (aref lynx-buffer-references (- n 1))))))))

(defvar lynx-link-overlay-properties
  '((mouse-face lynx-mouse-over-link-face)
    (face lynx-link-face)))

(defun lynx-associate-link (start end n link-url)
  (let ((overlay (make-overlay start end)))
    (dolist (plist lynx-link-overlay-properties)
      (apply #'overlay-put overlay plist))
    (mapcar* (lambda (prop value) (overlay-put overlay prop value))
             '(link-url number help-echo)
             (list link-url n link-url))))


(defun lynx-previous-link (arg)
  (interactive "p")
  (lynx-next-link (- arg)))

(defvar lynx-next-increment nil)
(defvar lynx-trim-urls-to-fit-window-width t)

(defun lynx-next-link (arg)
  (interactive "p")
  (cond ((and (memq last-command '(lynx-previous-link lynx-next-link))
              lynx-next-increment)
         ;;
         ;; eg: when LYNX-NEXT-LINK is invoked with an arg
         ;; > 1, and then LYNX-PREVIOUS-LINK immediately.
         ;; maybe L-P-L should have the same arg??
         ;; doesn't work properly because movements to
         ;; different links are messed up... when point is
         ;; in the current link, LYNX-NEXT-LINK remains on
         ;; this link, while LYNX-PREVIOUS-LINK goes to
         ;; the previous one.
         ;;
         (unless (eq last-command this-command)
           (setq lynx-next-increment (- lynx-next-increment)))
         (setq arg lynx-next-increment))
        ((or (> arg 1) (< arg -1))
         (setq lynx-next-increment arg))
        (t (setq lynx-next-increment nil)))
  (let (overlay (point (point)))
    (catch 'done
      (dotimes (i (abs arg))
        (let ((tmp (lynx-get-overlay arg point)))
          (if (null tmp)
              (throw 'done t)
            (setq overlay tmp)
            (setq point
              (if (minusp arg) (overlay-start tmp) (overlay-end tmp)))))))
    (when overlay
      (if (minusp arg)
          (goto-char (overlay-start overlay))
        (goto-char (overlay-end overlay)))
      (let ((url (overlay-get overlay 'link-url))
            (width (window-width))
            ;; Don't put the URLs in *Message* buffer
            message-log-max)
        (message "%s"
                 (if (and lynx-trim-urls-to-fit-window-width
                          (>= (length url) width))
                     (concat (subseq url 0 (- (window-width) 5))
                             "....")
                   url))))))

(defun lynx-show-buffer-url ()
  (interactive)
  (if lynx-buffer-url
      (message "%s" lynx-buffer-url)
    (message "Cannot determine the URL")))

(defun lynx-absolute-link (n)
  (interactive "p")
  (setq lynx-next-increment nil)
  (goto-char (point-min))
  (lynx-next-link n))

;;; Download these files to disk instead of displaying them in
;;; an emacs buffer.  Well query if the user wants to download it
;;; or see it in a buffer.

(defvar lynx-downloadable-files ; Can be a regexp or a cons `(NOT regexp)'
  '((not "\\.\\(shtml\\|html\\|htm\\)$")))
  
(defvar lynx-query-user-on-download t)
(defvar lynx-replace-current-window-buffer t)

(defvar lynx-try-auto-mode-alist t
  "If non-NIL, then try to set the mode of buffer using AUTO-MODE-ALIST.
If the page name matches any element of AUTO-MODE-ALIST, then
the buffer mode is set by the associated function.")

(defvar lynx-lynx-mode-names
  (list "\\.s?html?\\'" "\\.jsp$")
  "Any buffer name that matches elements in this list will
automatically be in LYNX-MODE.")

(defvar lynx-actions-list
  ;; This first regexp should be gotten from variable `image-types'.
  '(("\\.\\(png\\|gif\\|tiff\\|jpeg\\|jpg\\|xpm\\|pbm\\|xbm\\)$" .
     lynx-show-image)))

(defvar lynx-view-images-other-buffer t)

(defvar lynx-insert-images t
  "If non-NIL, then insert all image links in current buffer.")

(defun lynx-insert-images (&optional start end arg)
  (interactive "r\nP")
  (unless (eq major-mode 'lynx-mode)
    (error "Buffer should be in lynx-mode"))
  (if (not arg)
      (setq end (or end (point-max)))
    (setq start (point-min) end (point-max)))
  (let ((tfiles '())
        (tbuf (get-buffer-create " *lynx images*"))
        (buffer-read-only nil)
        (buffer-file-coding-system 'raw-text))
    (with-current-buffer tbuf
      (setq buffer-undo-list t))
    (goto-char (or start (point-min)))
    (unwind-protect
      (save-excursion
        ;; Should really get the URL from the overlay itself...
        (let ((i -1)
              (len (length lynx-buffer-references)))
          (save-excursion
            (setq i (1- (overlay-get (lynx-get-overlay 1 start) 'number))))
          (while (and (<= (point) end) (< i len))
            (goto-char (overlay-end (lynx-get-overlay 1 (point))))
            (let ((url (format "%s" (aref lynx-buffer-references i)))
                  (tfile (make-temp-file "lynx-")))
              (push tfile tfiles)
              (when (string-match
                     "\\.\\(png\\|gif\\|tiff\\|jpeg\\|jpg\\|xpm\\|pbm\\|xbm\\)$"
                     url)
                (if (looking-at (format "\\[%s\\]"(file-name-nondirectory url)))
                    (replace-match ""))
                (with-current-buffer tbuf
                  (delete-region (point-min) (point-max))
                  (call-process
                   "lynx" nil t nil "-dump" "-connect_timeout=10" url)
                  (write-region (point-min) (point-max) tfile nil 'no-message))
                ;; Don't bother the user with errors...
                (condition-case nil
                    (insert-image (create-image tfile) "<image>")
                  (error nil))))
            (incf i))))
      ;(mapc 'delete-file tfiles)
      )))

(defun lynx-fetch (url &optional arg)
  (interactive
    (let ((url (ffap-url-at-point)))
      (if url
          ;; Give the user a little warning.
          (prog1 (list url nil)
            (message "Fetching `%s'..." url)
            (sit-for 1.0)
            (message nil))
        (list (read-string "Url: ") current-prefix-arg))))
  ;; Allows user to navigate by keywords:
  (setq url (lynx-maybe-convert-keyword url))
  (with-current-buffer (get-buffer-create (funcall lynx-buffer-name url))
    (let ((lynx-default-options
           (cond (current-prefix-arg
                  (let ((def (mapconcat 'identity lynx-default-options " ")))
                    (split-string
                     (read-string "Lynx options: " def nil def))))
                 ((consp arg) arg)
                 (t lynx-default-options))))
      (cond ((string-match "^mailto:\\(.*\\)" url)
             (let ((addr (match-string 1 url)))
               (compose-mail addr)))
            ((not (let (buffer-read-only)
                    (erase-buffer)
                    (lynx-exec url)))
             (message "Unable to fetch `%s'" url))
            (t
             ;; Normal URL fetch:
             (let ((name (file-name-nondirectory url))
                   image-file-p)
               (cond ((and lynx-actions-list
                           (member* name lynx-actions-list
                                    :test (lambda (n e)
                                            (when (string-match (car e) n)
                                              (funcall (cdr e))
                                              t))))
                      ;; It is an image:
                      (setq image-file-p t))
                     ((and lynx-try-auto-mode-alist
                           (and (not (member* name lynx-lynx-mode-names
                                              :test (lambda (n r)
                                                      (string-match r n))))
                                (member* name auto-mode-alist
                                         :test (lambda (n e)
                                                 (when (string-match (car e) n)
                                                   (funcall (cdr e))
                                                   t)))))
                      ;; Not a normal HTML file:
                      (set-buffer-modified-p nil)
                      (view-mode)
                      (goto-char (point-min)))
                     (t
                      (lynx-mode)
                      ;; must be after LYNX-MODE, since LYNX-MODE
                      ;; kills all local vars
                      (setq lynx-buffer-url url)))
               (cond ((and image-file-p lynx-view-images-other-buffer)
                      (setq cursor-type nil)
                      ;; Show buffer, but don't switch to it.
                      (display-buffer (current-buffer)))
                     (lynx-replace-current-window-buffer
                      (switch-to-buffer (current-buffer)))
                     (t (pop-to-buffer (current-buffer))))
               ;;
               ;; Invoke any associated action.  Should do this
               ;; after the windows and buffers are setup properly.
               ;; This won't execute any actions, if no actions are
               ;; associated with this URL.
               ;;
               (when lynx-execute-actions-p
                 (lynx-invoke-action url))))))
    (current-buffer)))

(defun lynx-fetch-in-other-window (arg)
  (interactive "P")
  (let (buf)
    (save-window-excursion
      (setq buf (call-interactively 'lynx-fetch)))
    (switch-to-buffer-other-window buf)))

;;; Should go to DIR times forward before retrieving url.
(defun lynx-follow-url (dir)
  (interactive "p")
  (lynx-fetch (lynx-get-closest-url dir)))

(defun lynx-follow-url-in-other-window (dir)
  (interactive "p")
  (let ((buf (save-window-excursion (lynx-follow-url dir))))
    (switch-to-buffer-other-window buf)))

(defun lynx-backward-follow-url (dir)
  (interactive "p")
  (lynx-follow-url (- dir)))

(defun lynx-get-overlay-at (&optional point)
  (car (overlays-at (or point (point)))))

(defun lynx-next-overlay (&optional point)
  (lynx-get-overlay-at
   (next-overlay-change (or point (point)))))

(defun lynx-previous-overlay (&optional point)
  (setq point (or point (point)))
  (let ((last (previous-overlay-change point)))
    (or (lynx-get-overlay-at last)
        (lynx-get-overlay-at (- last 1)))))

(defun lynx-get-overlay (dir &optional point)
  (setq point (or point (point)))
  (let ((overlay
         (or (let ((overlay             
                    (lynx-get-overlay-at point)))
               (if (and (minusp dir)
                        overlay
                        (eq point (overlay-start overlay)))
                   nil
                 overlay))
             (if (minusp dir)
                 (lynx-previous-overlay point)
               (lynx-next-overlay point)))))
    (if (null overlay)
        (if (minusp dir)
            (lynx-next-overlay (point-min))
          (lynx-previous-overlay (point-max)))
      overlay)))

(defun lynx-get-closest-url (dir &optional move-point)
  (let ((overlay (lynx-get-overlay dir)))
    (when overlay
      (when move-point
        (if (minusp dir)
            (goto-char (overlay-start overlay))
          (goto-char (overlay-end overlay))))
      (overlay-get overlay 'link-url))))

(defun lynx-copy-current-link-as-kill (arg)
  (interactive "p")
  (let ((url (lynx-get-closest-url arg)))
    (when url
      (kill-new url)
      (message "Copied \"%s\"" url))))

;;; TODO: Fix LYNX-GET-OVERLAY to
;;;  return an overlay DIR times later...

(defun lynx-forward-follow (arg)
  (interactive "p")
  (let (url)
    (if (= (abs arg) 1)
        (setq url (lynx-get-closest-url arg t))
      (setq url (lynx-absolute-link arg)))
    (when url
      (let ((current-prefix-arg nil))
        (lynx-fetch url)))))    

(defun lynx-backward-follow (arg)
  (interactive "p")
  (lynx-forward-follow (- arg)))  


(defvar lynx-tmp-filenames '())

(add-hook 'kill-emacs-hook
  (lambda ()
    (mapcar (lambda (name) (ignore-errors (delete-file name)))
            lynx-tmp-filenames)))

;;; TMP can be deleted here itself....
(defun lynx-show-image ()
  (let ((string (buffer-string))
        (tmp (make-temp-file "lynx-")))
    (push tmp lynx-tmp-filenames)
    (condition-case error
        (let ((buffer-file-coding-system 'raw-text)) ;Is this right?
          (write-region (point-min) (point-max) tmp  nil 'no-message)
          (delete-region (point-min) (point-max))
          (insert-image (create-image tmp) "<image>"))
      (error
       (insert string)
       (goto-char (point-min))
       (message "Unable to display image: %S" error)))))

;;; FIXME:
(defun lynx-list-matching-urls (match)
  (interactive "sURLs matching: ")
  (let ((references lynx-buffer-references) ; Need refs of current buffer.
        (url lynx-buffer-url))
    (with-current-buffer (get-buffer-create "*URL Matches*")
      (erase-buffer)
      (if (string= match "")
          (let ((len (length references)))
            (insert (format "All the references from `%s':\n\n" url))
            (do ((i 0 (1+ i)))
                ((= i len))
              (insert (aref references i) "\n")))
        ;;
        ;; Otherwise, list all the matching URLs nicely....
        (let ((matches '()))
          (insert "URLs matching `" match "':\n\n")
          (do ((i (- (length references) 1) (- i 1)))
              ((<= i 0))
            (when (string-match match (aref references i))
              (push (aref references i) matches)))
          (setq matches (sort matches 'string<))
          (let ((new '())
                (max 0))
            (while matches
              (let ((cur (cons 1 (pop matches)))
                    (done nil))
                (while (and matches (not done))
                  (if (not (string= (car matches) (cdr cur)))
                      (setq done t)
                    (incf (car cur))
                    (pop matches)))
                (setq max (max (car cur) max))
                (push cur new)))
            (when (> max 0)
              (let ((control (format "%%0%dd " (+ 1 (floor (log max 10))))))
                (dolist (x (sort new (lambda (x y) (> (car x) (car y)))))
                  (insert (format control (car x)) (cdr x) "\n")))))))
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(defun lynx-up-directory (&optional arg)
  (interactive "p")
  (let ((url lynx-buffer-url))
    (dotimes (i (abs arg))
      (when url
        (when (eq ?\/ (aref url (1- (length url))))
          (setq url (subseq url 0 (1- (length url)))))
        (setq url (file-name-directory url))
        (when (member url '("http://" "ftp://" "file://"))
          (setq url nil))))
    (if (or (null url) (string= url lynx-buffer-url))
        (message "Can't go up any more levels")
      (lynx-fetch url))))


;;; Experimental stuff:

;;; Quick (intutive) access to websites:

;;; DEFINE-KEYS is a macro that I have locally.
;;; It is kinda ripped from GNUS-DEFINE-KEYS (from what I
;;; remember).

;;; For example, the key combination `/.' should fetch
;;; `http://www.slashdot.org'.  But `C-u /.' should
;;; maybe do a search on slashdot.org.

;(define-keys (lynx-hotkey-prefix-map "j" lynx-mode-map)
;  "\\." (lambda () (interactive) (lynx-fetch "http://www.slashdot.org")))

;(defvar lynx-hotlist+history-filename nil)

;(defun lynx-get-url ()
;  )


;;; Lynx Actions:
;;;
;;; Recording actions for URLs.  If an URL loads up, and it has an
;;; associated action, then the action is executed.  Convinent for moving
;;; to certain places (positions) in the buffer, highlighting headlines
;;; matching a regexp, or even inserting particular images....
;;; Actions can either be a function or a string (vector) which is
;;; interpreted as a keyboard macro.

(defvar lynx-command-actions nil)
(defvar lynx-record-action-url nil)
(defvar lynx-actions-save-filename nil)

(defun lynx-maybe-convert-keyword (url)
  (catch 'done
    (dolist (x lynx-command-actions)
      (if (string= (car x) url)
          (throw 'done (cadr x))))
    url))

(defun lynx-invoke-action (url &optional prefix)
  (let ((action
         (catch 'done
           (dolist (x lynx-command-actions)
             (when (string= (cadr x) url)  ;string-match
               (throw 'done (caddr x)))))))
    (when action
      (cond ((or (stringp action) (vectorp action))
             (let ((last-kbd-macro action))
               (call-last-kbd-macro prefix)))
            ((functionp action)
             (funcall action))
            (t
             (error "Unrecognized lynx action for `%s': %S"
                    url action))))))

;;; Two ways to define a lynx action.  Either using
;;; the macro DEFINE-LYNX-ACTION or by calling
;;; LYNX-RECORD-ACTION and actually doing some action.
;;;
;;; Watch out for recursive actions; will mess up if
;;; we use STRING-MATCH instead of STRING=.
;;; When we use STRING-MATCH, matching URL's can get you
;;; into trouble.

(defmacro def-lynx-action (arg &rest body)
  (let ((name (car arg))
        (url (cadr arg)))
    (unless (or (stringp name) (symbolp name))
      ;; allow numbers
      (setq name (format "%s" name)))
    `(progn
       (if (assq ',name lynx-command-actions)
           (setq lynx-command-actions
                 (delete* ',name lynx-command-actions :key #'car)))
       (push (list ',name ,url ,(if (null (cdr body))
                                    nil
                                  `(lambda ,(car body)
                                     ,@(cdr body)
                                     (recenter 0))))
             lynx-command-actions))))

(defun lynx-record-action ()
  (interactive)
  (when (null defining-kbd-macro)
    (let ((url (read-string "Record action for url: ")))
      (setq lynx-record-action-url url)
      (lynx-fetch url)
      (start-kbd-macro nil)
      (message "Recording actions...hit `%s' to finish"
               (key-description
                (car (where-is-internal 'lynx-finish-kbd-macro)))))))

(defun lynx-finish-kbd-macro ()
  (interactive)
  (when defining-kbd-macro
    (end-kbd-macro)
    (push (list nil lynx-record-action-url (copy-seq last-kbd-macro))
          lynx-command-actions)))


;;; Google services:

(defvar lynx-google-search-url
  "http://www.google.com/search?q=%s&start=0")

(defvar lynx-google-groups-url
  "http://groups.google.com/groups?q=%s&ie=UTF-8&hl=en")
  
(defun lynx-google (string)
  "Google search."
  (interactive "sGoogle Search: ")
  (when (> (length string) 0)
    (let ((all '((?\  . "+") (?\" . "%22"))))
      (lynx-fetch
        (format lynx-google-search-url
          (mapconcat (lambda (char)
                       (let ((new (cdr (assoc char all))))
                         (if (stringp new)
                             new
                           (string (or new char)))))
                     string ""))))))

(defun lynx-google-groups (string)
  "Google groups search."
  (interactive "sGoogle groups: ")
  (lynx-fetch (format lynx-google-groups-url
                (mapconcat 'identity (split-string string) "+"))))


;;; Completions:


(provide 'lynx)

;;; lynx.el ends here
