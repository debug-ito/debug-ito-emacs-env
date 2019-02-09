;; Save this file in ISO-2022-JP because it is the coding system for Anthy!

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lib")
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(load-library "debugito")
(setq debugito-frame-width 128)
(setq debugito-frame-height 26)
(debugito-load-if-any "local" t)

(setq inhibit-startup-screen t)
(global-font-lock-mode t) ;; Syntax Highlighting
(setq line-number-mode t) ;; show Line Number
(auto-compression-mode t) ;; prevent mojibake of Japanese info
(setq make-backup-files nil) ;; dont create backup files
(show-paren-mode t) ;; Emphasize matching parentheses
(setq-default indent-tabs-mode nil) ;; Never use tabs for indentation
(setq-default column-number-mode t)
(transient-mark-mode 0)
(setq tags-revert-without-query t)
(setq-default bidi-display-reording nil)
(setq-default bidi-paragraph-direction 'left-to-right)
(electric-indent-mode 0)
(setq comment-style 'extra-line) ;; Comment marks (e.g. /*, */) should be only at the head of each line.
(server-start)

;;;;;;;;;;;;;; elscreen http://www.morishima.net/~naoto/elscreen-en/?lang=en
(when (debugito-require-if-any 'elscreen)
  (elscreen-start))


;;;;;;;;;;;;;; uniquify
(when (debugito-require-if-any 'uniquify)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;;;;;;;;;;;;;; yasnippet
(setq-default yas-snippet-dirs '("~/.emacs.d/snippets"))
(when (debugito-require-if-any 'yasnippet)
  (yas-global-mode 1)
  (setq debugito-yas-name "Toshio Ito")
  (setq debugito-yas-email "debug.ito@gmail.com"))

;;;;;;;;;;;;;; Setting for X mode
(cond (window-system
       ;; .emacsでのフォント設定でうまくいかないときは~/.Xresourcesで設定してみる
       (set-default-font "TakaoGothic-12")
       (set-scroll-bar-mode 'right) ;; show scroll bar on the right side
       (menu-bar-mode 0) ;; Disable menu-bar
       (tool-bar-mode 0) ;; Disable tool-bar
       (mouse-wheel-mode 1) ;; Enable wheel mouse
       (setq x-select-enable-clipboard t) ;; Share kill-ring and X clipboard
       (add-to-list 'default-frame-alist (cons 'height debugito-frame-height))
       (add-to-list 'default-frame-alist (cons 'width debugito-frame-width))
       (set-face-attribute 'region nil :foreground "gtk_selection_fg_color")))

;; Setting for sensible split
;; see Help "split-window-sensibly"
(setq split-height-threshold 30)
(setq split-width-threshold nil)

;; Set C++ mode for .h files
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Octave mode
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; Use aspell
; 辞書変更にはispell-change-dictionaryを使う
(setq-default ispell-program-name "aspell")

;;;;;;;;;;;;;;; mozc
(when (debugito-require-if-any 'mozc)
  (setq default-input-method "japanese-mozc"))

;; Table.el
(custom-set-faces
 '(table-cell ((t (:background "yellow2" :inverse-video nil)))))

;;;;;;;;;;;;; shell mode
;; ls --color works well in the shell mode
;; You also have to modify .bashrc so that it runs dircolors
(autoload 'ansi-color-for-comint-mode-on "ansi-color"
  "Set `ansi-color-for-comint-mode' to t." t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;; C-j as sending input, like bash
(add-hook 'shell-mode-hook (lambda () (local-set-key (kbd "C-j") 'comint-send-input)))
;; Hide password input prompted by sudo
(setq comint-password-prompt-regexp "\\(\\([Oo]ld \\|[Nn]ew \\|'s \\|login \\|Kerberos \\|CVS \\|UNIX \\| SMB \\|^\\|\\[sudo\\] \\)[Pp]assword\\( (again)\\)?\\|pass phrase\\|\\(Enter\\|Repeat\\|Bad\\) passphrase\\)\\(?:, try again\\)?\\(?: for [^:]+\\)?:\\s *\\'")
(setq shell-completion-fignore '(".svn/"))

;; shell command uses pop-to-buffer to show the shell buffer, which by
;; default prefers another window to show the buffer. To show the
;; shell buffer in the current window, we temporarily set
;; display-buffer-overriding-action to change the default behavior of
;; pop-to-buffer.
(defun debugito-advice-shell-window (orig-fun &rest args)
  (let ((orig-override display-buffer-overriding-action))
    (setq display-buffer-overriding-action display-buffer--same-window-action)
    (apply orig-fun args)
    (setq display-buffer-overriding-action orig-override)))
(advice-add 'shell :around 'debugito-advice-shell-window)

;; track current working directory based on /proc fs
;; http://www.emacswiki.org/emacs/ShellDirtrackByProcfs
(defun track-shell-directory/procfs ()
  (shell-dirtrack-mode 0)
  (add-hook 'comint-preoutput-filter-functions
            (lambda (str)
              (prog1 str
                (when (string-match comint-prompt-regexp str)
                  (cd (file-symlink-p
                       (format "/proc/%s/cwd" (process-id
                                               (get-buffer-process
                                                (current-buffer)))))))))
            nil t))
(add-hook 'shell-mode-hook 'track-shell-directory/procfs)

;;;;;;;;;;;;;;;; Add NesC mode autoload feature
;; You have to place nesc.el provided by nesc package in wherever emacs can find it.
(autoload 'nesc-mode "nesc.el")
(add-to-list 'auto-mode-alist '("\\.nc\\'" . nesc-mode))

;;;;;;;;;;;;;;;;; Makefile mode

(add-to-list 'auto-mode-alist '("/[mM]akefile[^/]*\\'" . makefile-gmake-mode))

;;;;;;;;;;;;;;;;; CSS mode
(setq cssm-mirror-mode nil) ;; Turn off mirror mode
(setq cssm-indent-function #'cssm-c-style-indenter) ;; C-style indentation

;;;;;;;;;;;;;;;; TeX mode
(setq latex-run-command "platex")
(setq tex-bibtex-command "jbibtex")
;; (setq latex-run-command "latex")
;; (setq tex-bibtex-command "bibtex")
(add-hook 'latex-mode-hook (lambda ()
                             (defun tex-display-shell ()
                               (display-buffer (tex-shell-buf)))
                             (local-set-key (kbd "C-j") 'newline-and-indent)
                             (local-set-key (kbd "C-c m")
                                            (lambda (arg) (interactive "p")
                                              (debugito-insert-pair arg "$" "$")))
                             (local-set-key (kbd "C-c i")
                                            (lambda (arg) (interactive "p")
                                              (debugito-insert-pair arg "\\color{colmodified}" "\\color{colstayed}")))
                             (local-set-key (kbd "C-c o") 'latex-insert-block)
                             (add-to-list 'tex-compile-commands '("jbibtex %r"))))

;;;;;;;;;;;;;;;;;; Skeleton read to enable C-h as delete in incremental search
;; http://d.hatena.ne.jp/emacsjjj/20060222/p1
(defadvice skeleton-read (around unbind-c-h activate compile)
  (let ((help-char nil))
    ad-do-it))

;;;;;;;;;;;;;;;;;; Wanderlust key customization
(add-hook 'wl-summary-mode-hook (lambda () (local-set-key (kbd "C-d") 'debugito-wl-dup-and-show-message)))
(add-hook 'wl-summary-mode-hook (lambda () (local-set-key (kbd "M-d") 'debugito-wl-dup-message-buf)))

;;;;;;;;;;;;;;;;;;; For Japanese printing
;; See also https://wiki.ubuntulinux.jp/UbuntuTips/Application/EmacsJapanesePrint
(debugito-require-if-any 'ps-mule)
(setq ps-multibyte-buffer 'non-latin-printer)
(defalias 'ps-mule-header-string-charsets 'ignore)

;;;;;;;;;;;;;;;;;;;; Magit
(defadvice magit-status (around magit-status-full-window activate)
  (let ((pop-up-windows nil))
    ad-do-it))
(add-hook 'git-commit-setup-hook 'turn-off-auto-fill) ;; auto-fill is enabled by default
(setq git-commit-finish-query-functions '())

;;;;;;;;;;;;;;;;;;;; markdown mode
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-hook 'markdown-mode-hook
          (lambda () (progn
                       (local-set-key (kbd "C-m") 'newline)
                       (setq word-wrap t)
                       (setq comment-continue "  --"))))

;;;;;;;;;;;;;; (C)Perl mode
;; http://www.emacswiki.org/emacs/CPerlMode
;; https://github.com/jrockway/cperl-mode
(defalias 'perl-mode 'cperl-mode)
(dolist (file '("\\.t\\'" "\\.psgi\\'" "cpanfile" "Daikufile"))
  (add-to-list 'auto-mode-alist (cons file 'perl-mode)))
(setq cperl-invalid-face nil
      cperl-close-paren-offset -4
      cperl-continued-statement-offset 4
      cperl-indent-level 4
      cperl-indent-parens-as-block t
      cperl-indent-region-fix-constructs nil
      cperl-electric-parens nil
      cperl-indent-subs-specially nil
      cperl-auto-newline nil
      cperl-electric-backspace-untabify nil)
(add-hook 'cperl-mode-hook
          (lambda ()
            (local-set-key (kbd "M-m") (lambda () (interactive) (insert "my ")))
            (local-set-key (kbd "<tab>") 'indent-for-tab-command)
            ;; Forbid ugly special color theme. Standard theme is the most beautiful.
            (copy-face 'font-lock-variable-name-face 'cperl-array-face)
            (copy-face 'font-lock-variable-name-face 'cperl-hash-face)
            (copy-face 'font-lock-keyword-face       'cperl-nonoverridable-face)))

;;;; It seems that CPerl mode messes up with font-lock-keywords variable when
;;;; you use font-lock-add-keywords to add custom patterns for font-lock.
;;;; And it seems very difficult to add a pattern safely unless you embed it inside
;;;; the code of CPerl mode.
;; (font-lock-add-keywords nil
;;                         '(("&[a-zA-Z_0-9]*" 0  font-lock-function-call-face))
;;                         nil)))


;;;;;;;;;;;;;;;;;;;;; Ruby mode
(add-hook 'ruby-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c n")
                           (lambda (arg) (interactive "p")
                             (debugito-insert-pair arg "" "end")))
            (local-set-key (kbd "C-c m")
                           (lambda (arg) (interactive "p")
                             (debugito-insert-pair arg "|" "|")))))

;;;;;;;;;;;;;;;;;;; ChangeLog mode
(debugito-remove-auto-mode 'change-log-mode)

;;;;;;;;;;;;;;;;;;; Javascript mode
(setq js-auto-indent-flag nil)

;;;;;;;;;;;;;;;;;;; Haskell mode
(setq haskell-mode-hook
      '(turn-on-haskell-indentation
        turn-on-haskell-doc-mode
        turn-on-haskell-decl-scan
        turn-on-font-lock
        (lambda ()
          (set (make-local-variable 'debugito-open-block-impl) 'debugito-open-block-aligned)

          ;; Quick fix for fill-paragraph missing haddock line for
          ;; haskell-mode 13.14.2-1 (from Ubuntu repository).
          ;; See: https://github.com/haskell/haskell-mode/issues/570
          (when (equal haskell-version "13.14")
            (set 'paragraph-separate (concat " *$\\| *\\({-\\|-}\\) *$\\|" page-delimiter)))
          
          (define-key haskell-indentation-mode-map (kbd "C-j") 'haskell-indentation-newline-and-indent)
          (define-key haskell-indentation-mode-map (kbd "RET") 'newline)
          (define-key haskell-indentation-mode-map (kbd "C-d") 'delete-char))))

;;;;;;;;;;;;;;;;;;; Groovy mode

;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))


;;;;;;;;;;;;;;;;;;; org mode
(setq org-startup-folded nil)

;;;;;;;;;;;;;;;;;; setting for japanese-zenkaku-region

(put-char-code-property ?- 'jisx0208 ?―) ;; U+2014 "em dash"
(put-char-code-property ?  'jisx0208 nil) ;; don't use Zenkaku Space.
(put-char-code-property ?. 'jisx0208 ?．) ;; Use Zenkaku-Period, not the ideographic period (。)
(put-char-code-property ?, 'jisx0208 ?，) ;; Use Zenkaku-Comma, not the ideographic comma (、)


;;;;;;;;;;;;; Key customization

;; C-h as backspace in i-search mode
;; http://www.hpcs.cs.tsukuba.ac.jp/~yonemoto/pukiwiki/index.php?Meadow%2F.emacs
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)

;; (setq parens-require-spaces nil)


(global-set-key (kbd "C-c h") (lambda (arg) (interactive "p") (debugito-insert-pair arg "[" "]")))
(global-set-key (kbd "C-c j") (lambda (arg) (interactive "p") (debugito-insert-pair arg "(" ")")))
(global-set-key (kbd "C-c k") (lambda (arg) (interactive "p") (debugito-insert-pair arg "{" "}")))
(global-set-key (kbd "C-c l") (lambda (arg) (interactive "p") (debugito-insert-pair arg "\"" "\"")))
(global-set-key (kbd "C-c ;") (lambda (arg) (interactive "p") (debugito-insert-pair arg "'" "'")))
(global-set-key (kbd "C-c n") (lambda (arg) (interactive "p") (debugito-insert-pair arg "<" ">")))
(global-set-key (kbd "C-c o") (lambda (arg) (interactive "p") (debugito-insert-pair arg "`" "`")))
(global-set-key (kbd "C-c p") 'popwin:display-last-buffer)
(global-set-key (kbd "C-c v") 'magit-status)
(global-set-key (kbd "C-x C-h") 'help)
(global-set-key (kbd "C-c C-j") 'debugito-open-block)
(global-set-key (kbd "C-x r n") 'debugito-sequence-rectangle)
(global-set-key (kbd "C-x C-n") 'next-multiframe-window)
(global-set-key (kbd "C-x C-p") 'previous-multiframe-window)
(global-set-key (kbd "C-x l"  ) 'font-lock-fontify-buffer)
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
(global-set-key (kbd "C-;") 'toggle-input-method)
(global-set-key (kbd "M-j") 'toggle-input-method)
(global-set-key (kbd "C-^") 'enlarge-window)
(global-set-key (kbd "C-~") 'shrink-window)
(global-set-key (kbd "C-]") 'enlarge-window-horizontally)
(global-set-key (kbd "C-}") 'shrink-window-horizontally)
(global-set-key (kbd "M-i") 'complete-symbol)
(global-set-key (kbd "M-s h l") 'debugito-highlight-lines)
(global-set-key (kbd "<C-tab>") 'dabbrev-expand)
(global-set-key (kbd "C-.") ">")
(global-set-key (kbd "C-,") "<")
(global-set-key (kbd "C--") "=")
(global-set-key (kbd "C-\\") "_")
(global-set-key (kbd "M-,") (lambda () (interactive) (find-tag "" t)))
(global-set-key (kbd "C-o") (lambda () (interactive) (debugito-rot-input debugito-rot-dollar 'debugito-rot-CO)))
(global-set-key (kbd "C-t") (lambda (arg) (interactive "p")
                  (if (= arg 1)
                      (debugito-rot-input debugito-rot-othersigils 'debugito-rot-CT)
                    (debugito-rot-input debugito-rot-arrow 'debugito-rot-CT 2))))

;; Always-enabled global key setting
;; http://pqrs.org/emacs/doc/keyjack-mode/index.html
;; http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
;;
;; kbd macro seems to accept constants only. We have to repeat writing it.
(setq my-keyjack-mode-map (make-sparse-keymap))
(mapcar (lambda (x)
          (define-key my-keyjack-mode-map (car x) (cdr x))
          (global-set-key (car x) (cdr x)))
        `(;; One-touch opening a block
          (,(kbd "C-c C-j") . debugito-open-block)
          ;; sequential numbering
          (,(kbd "C-c C-o") . (lambda () (interactive) (debugito-rot-input debugito-rot-arrow 'debugito-rot-CO)))))

(define-minor-mode my-keyjack-mode
  "A minor mode so that my key settings override annoying major modes."
  t nil 'my-keyjack-mode-map)
(my-keyjack-mode 1)


(debugito-load-if-any "local-after" t)
