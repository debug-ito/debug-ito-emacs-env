;;; Count words in a region
;; http://www.math.s.chiba-u.ac.jp/~matsu/lisp/emacs-lisp-intro-jp_14.html
(unless (fboundp 'count-words-region)
  (defun count-words-region (beginning end)  
    "Print number of words in the region."
    (interactive "r")
    (message "Counting words in region ... ")
    (save-excursion
      (let ((count 0))
        (goto-char beginning)
        (while (and (< (point) end)
                    (re-search-forward "\\w+\\W*" end t))
          (setq count (1+ count)))
        (cond ((zerop count)
               (message
                "The region does NOT have any words."))
              ((= 1 count)
               (message
                "The region has 1 word."))
              (t
               (message
                "The region has %d words." count)))))))


;; Context-aware window switching
;; see http://d.hatena.ne.jp/rubikitch/20100210/emacs
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-sensibly (selected-window)))
  (other-window 1))

;; repeated align
;; http://www.emacswiki.org/emacs/AlignCommands#toc5
(defun align-repeat (start end regexp)
    "Repeat alignment with respect to 
     the given regular expression."
    (interactive "r\nsAlign regexp: ")
    (align-regexp start end 
        (concat "\\(\\s-*\\)" regexp) 1 0 t))

