(require 'lynx)

;;; cnn.com headline begins with a non-space (the first line to do that):
(def-lynx-action (cnn "http://www.cnn.com") ()
  (if (re-search-forward "^\\[" nil t)
      (beginning-of-line)))

;(def-lynx-action (g "http://www.google.com") ()
;  (if (search-forward "seconds" nil t)
;      (lynx-next-link 1)))

;;; sl -> "http://www.slashdot.org"
;;; Highlight topics:  this doesn't work too well because
;;; the RSS feed doesn't too many topic titles.
;;; Also, the RSS feed doesn't get updated as often as
;;; the website; thus, the latest headlines at the beginning
;;; of the page will often (occasionally?) missed.

(def-lynx-action (sl "http://www.slashdot.org") ()
  (let ((story-topics '())
        (initial nil))
    ;;
    ;; Get all the topics from the RSS:
    ;;
    (with-temp-buffer
      (lynx-exec "http://www.slashdot.org/index.rss")
      (goto-char (point-min))
      (re-search-forward "^<item " nil t)
      (while (re-search-forward "^<title>\\(.*\\)</title>" nil t)
        (push (match-string 1) story-topics)))
    (setq story-topics (nreverse story-topics))
    ;;
    ;; We are in Slashdot's buffer:
    ;;
    (goto-char (point-min))
    (dolist (topic story-topics)
      ;;
      ;; Hack: long headlines are broken into 2 or more lines:
      (setq topic (mapconcat 'identity (split-string topic) "[ \t\n]*"))
      ;;
      ;; Highlight topics:
      (when (re-search-forward topic nil t)
        (let ((beg (progn (goto-char (match-beginning 0))
                          (point-at-bol)))
              (end (progn (goto-char (match-end 0))
                          (point-at-eol))))
          ;;
          ;; We want to position the first topic at the beginning
          ;; of the window.
          (if (null initial) (setq initial beg))
          (let ((ov (make-overlay beg end)))
            (overlay-put ov 'face 'slashdot-highlight-headline)))))
    (goto-char (or initial (point-min)))
    ;; We want to make sure that new topics (when the RSS isn't
    ;; updated yet) aren't missed.
    (recenter -1)
    (goto-char (window-start))))

(defface slashdot-highlight-headline
  '((t :background "black" :foreground "red"))
  "")
