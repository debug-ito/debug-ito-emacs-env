(defun debugito-load-if-any (file &optional dont-warn)
  "Frontend to `load'. It loads FILE and returns non-nil it it succeeds.
Otherwise, it returns nil and shows warning. If DONT-WARN is non-nil, the warning
is suppressed."
  (if (load file t)
      t
    (progn (unless dont-warn
             (warn "Cannot find and load %s. Skip." file))
           nil)))

(defun debugito-require-if-any (feature &optional filename dont-warn)
  "Frontend to `require'. If it loads FEATURE successfully, it returns non-nil.
Otherwise, it returns nil and shows warning. If DONT-WARN is non-nil, the warning
is suppressed."
  (if (require feature filename t)
      t
    (progn (unless dont-warn
             (warn "Cannot find and load %s. Skip." feature))
           nil)))

(defun debugito-open-block ()
  "Open a line for a new block."
  (interactive)
  (save-excursion (newline-and-indent))
  (newline-and-indent))

;; insert-pair
(defun debugito-insert-pair (arg begin-str end-str)
  "When you use without any prefix argument, this function inserts BEGIN-STR followed by END-STR to the point,
and moves the point between the two inserted strings.
With a prefix argument (C-u), BEGIN-STR and END-STR are inserted to the beginning and end of the current region, respectively,
and the point is moved right after the inserted END-STR."
  (if (= arg 1)
      (let ((med-point (debugito-insert-pair-pos begin-str end-str (point) (point))))
        (goto-char med-point))
    (debugito-insert-pair-pos begin-str end-str (region-beginning) (region-end))))

(defun debugito-insert-pair-pos (begin-str end-str begin-pos end-pos)
  (let ((end-pos-mk (make-marker)))
    (set-marker end-pos-mk end-pos)
    (goto-char begin-pos)
    (insert-before-markers begin-str)
    (goto-char (marker-position end-pos-mk))
    (insert end-str)
    (marker-position end-pos-mk)))



;;; sequential numbering
(defvar debugito-seq-format "%d"
  "Last format used by `debugito-sequence-rectangle'.")
(defvar debugito-seq-number 0 "Number inserted by `debugito-sequence-rectangle'")
(autoload 'apply-on-rectangle "rect")
(defun debugito-sequence-rectangle-line (startcol endcol incr format)
  (move-to-column startcol t)
  (delete-rectangle-line startcol endcol nil)
  (insert (format format debugito-seq-number))
  (setq debugito-seq-number (+ debugito-seq-number incr)))

(defun debugito-sequence-rectangle (start end first incr format)
  "Combining string-rectangle from rect.el and cua-sequence-rectangle from cua-rect.el,
debugito-sequence-rectangle does exactly the same job as cua-sequence-rectangle, although its UI is
much more like string-rectangle.
You may want to bind a key sequence `C-x r n' or something to this command."
  (interactive
   (list (region-beginning)
         (region-end)
         (if current-prefix-arg
             (prefix-numeric-value current-prefix-arg)
           (string-to-number
            (read-string "Start value: (1) " nil nil "1")))
         (string-to-number
          (read-string "Increment: (1) " nil nil "1"))
         (read-string (concat "Format: (" debugito-seq-format ") "))))
  (if (= (length format) 0)
      (setq format debugito-seq-format)
    (setq debugito-seq-format format))
  (setq debugito-seq-number first)
  (apply-on-rectangle 'debugito-sequence-rectangle-line start end incr format))

(require 'cl)  ;; for subseq function

;; rotational input
(defvar debugito-rot-dollar '(("$" . 1) ("\\" . 1) ("\\$" . 2) ("\\\\" . 2) ("$" . 0)))
(defvar debugito-rot-arrow '(("->" . 2) ("=>" . 2) ("<-" . 2)))
(defvar debugito-rot-othersigils '(("@" . 1) ("%" . 1) ("&" . 1)))

(defvar debugito-rot-current-list nil)
(defvar debugito-rot-previous-elem nil)
(defun debugito-rot-input (input-list &optional comm-name start-index)
  "Inserts different strings every time this function is executed repeatedly.

INPUT-LIST is a list of cons cells that describes rotational input you want to perform.
In a cons cell in INPUT-LIST, its car is the string you want to insert and its cdr is the
number of characters that are to be deleted before inserting the next string.
When you execute this function repeatedly from the same command (or with the same COMM-NAME
parameter), the string elements in INPUT-LIST are inserted and deleted in turn.

COMM-NAME will be set to `this-command' variable, and it is used to determine whether
this execution of the function is the first time or not. If it's not the first time,
`debugito-rot-input' will input the next element of the previously provided INPUT-LIST.
If COMM-NAME is omitted, `this-command' variable remains unchanged.

START-INDEX is the index of the element in INPUT-LIST that you want to insert at the
first execution of this function. It must be equal or greater than 0 and less than the
length of INPUT-LIST.

See Also: rotate-text.el http://www.emacswiki.org/RotateText
  rotate-text provides similar functionality. The important difference is, however,
  rotate-text rotates a string that is already on the buffer. This behavior is sometimes
  confusing, especially when my intention is to insert a new string instead of modify
  the existing one."
  (unless start-index
    (setq start-index 0))
  (when comm-name
    (setq this-command comm-name))
  (unless (equal this-command last-command)
    (setq debugito-rot-current-list input-list)
    (setq debugito-rot-previous-elem nil))
  (let ((cur-elem (or (cdr debugito-rot-previous-elem)
                      (subseq debugito-rot-current-list start-index))))
    (when debugito-rot-previous-elem
      (backward-delete-char (cdr (car debugito-rot-previous-elem))))
    (insert (car (car cur-elem)))
    (setq debugito-rot-previous-elem cur-elem)))


(defun debugito-anthy-kutouten ()
  "Change Hiragana map so that kutouten are zenkaku comma and period."
  (interactive)
  (anthy-change-hiragana-map "," "，")
  (anthy-change-hiragana-map "." "．"))


(defun debugito-wl-dup-message-buf ()
  "In Wanderlust, create a buffer which is a duplicate of the message on which the cursor is in the Summary."
  (interactive)
  (let (wbuf newbuffer)
    (wl-summary-redisplay)
    (setq wbuf wl-message-buffer)
    (setq newbuffer (generate-new-buffer (format "*WL Message %d*" (wl-summary-message-number))))
    (set-buffer newbuffer)
    (insert-buffer-substring wbuf)
    (goto-char (point-min))
    (message (format "Message was duplicated to %s." newbuffer))
    newbuffer))

(defun debugito-wl-dup-and-show-message ()
  "In Wanderlust, create a duplicate buffer of a message and show it in a new window."
  (interactive)
  (let ((newbuffer (debugito-wl-dup-message-buf))
        (newwindow nil))
    (set-buffer "Summary")
    (wl-summary-toggle-disp-msg 'off)
    (setq newwindow (split-window-horizontally))
    (switch-to-buffer newbuffer)
    (select-window newwindow)
    (set-buffer "Summary")
    (wl-summary-redisplay)))

