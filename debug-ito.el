(defun debug-ito-open-block()
  "Open a line for a new block."
  (interactive)
  (let*
      ((startpos (point)))
    (newline-and-indent)
    (goto-char startpos)
    (newline-and-indent)))

;; insert-pair
(defun debug-ito-insert-pair (arg begin-str end-str)
  "When you use without any prefix argument, this function inserts BEGIN-STR followed by END-STR to the point,
and moves the point between the two inserted strings.
With a prefix argument (C-u), BEGIN-STR and END-STR are inserted to the beginning and end of the current region, respectively,
and the point is moved right after the inserted END-STR."
  (if (= arg 1)
      (let ((med-point (debug-ito-insert-pair-pos begin-str end-str (point) (point))))
        (goto-char med-point))
    (debug-ito-insert-pair-pos begin-str end-str (region-beginning) (region-end))))

(defun debug-ito-insert-pair-pos (begin-str end-str begin-pos end-pos)
  (let ((end-pos-mk (make-marker)))
    (set-marker end-pos-mk end-pos)
    (goto-char begin-pos)
    (insert-before-markers begin-str)
    (goto-char (marker-position end-pos-mk))
    (insert end-str)
    (marker-position end-pos-mk)))



;;; sequential numbering
(defvar debug-ito-seq-format "%d"
  "Last format used by `debug-ito-sequence-rectangle'.")
(defvar debug-ito-seq-number 0 "Number inserted by `debug-ito-sequence-rectangle'")
(autoload 'apply-on-rectangle "rect")
(defun debug-ito-sequence-rectangle-line (startcol endcol incr format)
  (move-to-column startcol t)
  (delete-rectangle-line startcol endcol nil)
  (insert (format format debug-ito-seq-number))
  (setq debug-ito-seq-number (+ debug-ito-seq-number incr)))

(defun debug-ito-sequence-rectangle (start end first incr format)
  "Combining string-rectangle from rect.el and cua-sequence-rectangle from cua-rect.el,
debug-ito-sequence-rectangle does exactly the same job as cua-sequence-rectangle, although its UI is
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
         (read-string (concat "Format: (" debug-ito-seq-format ") "))))
  (if (= (length format) 0)
      (setq format debug-ito-seq-format)
    (setq debug-ito-seq-format format))
  (setq debug-ito-seq-number first)
  (apply-on-rectangle 'debug-ito-sequence-rectangle-line start end incr format))

(defun debug-ito-anthy-kutouten ()
  "Change Hiragana map so that kutouten are zenkaku comma and period."
  (interactive)
  (anthy-change-hiragana-map "," "，")
  (anthy-change-hiragana-map "." "．"))


(defun debug-ito-wl-dup-message-buf ()
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

(defun debug-ito-wl-dup-and-show-message ()
  "In Wanderlust, create a duplicate buffer of a message and show it in a new window."
  (interactive)
  (let ((newbuffer (debug-ito-wl-dup-message-buf))
        (newwindow nil))
    (set-buffer "Summary")
    (wl-summary-toggle-disp-msg 'off)
    (setq newwindow (split-window-horizontally))
    (switch-to-buffer newbuffer)
    (select-window newwindow)
    (set-buffer "Summary")
    (wl-summary-redisplay)))

