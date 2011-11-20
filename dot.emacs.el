;; Save this file in ISO-2022-JP because it is the coding system for Anthy!

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/twittering-mode-dev/twittering-mode/")
(load-library "misc-func")
(load-library "debug-ito")


(setq inhibit-startup-screen t)
(mouse-wheel-mode 1) ;; Enable wheel mouse
(global-font-lock-mode t) ;; Syntax Highlighting
(setq line-number-mode t) ;; show Line Number
(auto-compression-mode t) ;; prevent mojibake of Japanese info
(setq make-backup-files nil) ;; dont create backup files
(show-paren-mode t) ;; Emphasize matching parentheses
(setq-default indent-tabs-mode nil) ;; Never use tabs for indentation
(setq-default column-number-mode t)
(transient-mark-mode nil)
(setq tags-revert-without-query t)
(server-start)


;;;;;;;;;;;;;; elscreen http://www.morishima.net/~naoto/elscreen-en/?lang=en
(require 'elscreen)

;;;;;;;;;;;;;; Setting for X mode
(cond (window-system
       ;; .emacs$B$G$N%U%)%s%H@_Dj$G$&$^$/$$$+$J$$$H$-$O(B~/.Xresources$B$G@_Dj$7$F$_$k(B
       (set-default-font "TakaoGothic-12")
       (set-scroll-bar-mode 'right) ;; show scroll bar on the right side
       (menu-bar-mode 0) ;; Disable menu-bar
       (tool-bar-mode 0) ;; Disable tool-bar
       (setq x-select-enable-clipboard t) ;; Share kill-ring and X clipboard
       (add-to-list 'default-frame-alist '(height . 26))
       (add-to-list 'default-frame-alist '(width .  128))))

;; Setting for sensible split
;; see Help "split-window-sensibly"
(setq split-height-threshold 30)
(setq split-width-threshold nil)

;; Set C++ mode for .h files
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Octave mode
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; Use aspell
; $B<-=qJQ99$K$O(Bispell-change-dictionary$B$r;H$&(B
(setq-default ispell-program-name "aspell")

;;;;;;;;;;;;;;; anthy
;; See http://forum.ubuntulinux.jp/viewtopic.php?pid=773
;; Apparently ISO-2022-JP should be used to encode the japanese characters below
(require 'anthy)
(when (>= emacs-major-version 22)
  (setq anthy-accept-timeout 1))
(setq default-input-method "japanese-anthy")
(setq anthy-wide-space " ")
(anthy-change-hiragana-map "la" "$B$!(B")
(anthy-change-hiragana-map "li" "$B$#(B")
(anthy-change-hiragana-map "lu" "$B$%(B")
(anthy-change-hiragana-map "le" "$B$'(B")
(anthy-change-hiragana-map "lo" "$B$)(B")
(anthy-change-hiragana-map "ltu" "$B$C(B")
(anthy-change-hiragana-map "lya" "$B$c(B")
(anthy-change-hiragana-map "lyu" "$B$e(B")
(anthy-change-hiragana-map "lyo" "$B$g(B")
(anthy-change-hiragana-map "cj" "$B!&(B")
(anthy-change-katakana-map "la" "$B%!(B")
(anthy-change-katakana-map "li" "$B%#(B")
(anthy-change-katakana-map "lu" "$B%%(B")
(anthy-change-katakana-map "le" "$B%'(B")
(anthy-change-katakana-map "lo" "$B%)(B")
(anthy-change-katakana-map "ltu" "$B%C(B")
(anthy-change-katakana-map "lya" "$B%c(B")
(anthy-change-katakana-map "lyu" "$B%e(B")
(anthy-change-katakana-map "lyo" "$B%g(B")
(anthy-change-katakana-map "cj" "$B!&(B")
;; Change keybind for chainging maps
;; http://ubulog.blogspot.com/2009/02/emacsanthyel.html
(setq anthy-rkmap-keybind
      '((("alphabet" . ?\C-j) . "hiragana")
        (("hiragana" . ?\C-j) . "alphabet")
        (("hiragana" . ?q) . "katakana")
        (("katakana" . ?q) . "hiragana")
        (("hiragana"     . ?\C-q) . "hankaku_kana")
        (("hankaku_kana" . ?\C-q) . "walphabet")
        (("walphabet" . ?\C-q) . "hiragana")))

;; Immediately return to Hiragana mode after commiting Katakanas.
(add-hook 'anthy-commit-hook (lambda ()
                               (when (string= "katakana" anthy-current-rkmap)
                                 (anthy-hiragana-map))))


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


;;;;;;;;;;;;;;;; Add NesC mode autoload feature
;; You have to place nesc.el provided by nesc package in wherever emacs can find it.
(autoload 'nesc-mode "nesc.el")
(add-to-list 'auto-mode-alist '("\\.nc\\'" . nesc-mode))

;;;;;;;;;;;;;;;;; CSS mode
(setq cssm-mirror-mode nil) ;; Turn off mirror mode
(setq cssm-indent-function #'cssm-c-style-indenter) ;; C-style indentation

;;;;;;;;;;;;;;;; TeX mode
(setq latex-run-command "platex")
(setq tex-bibtex-command "jbibtex")
;; (setq latex-run-command "latex")
;; (setq tex-bibtex-command "bibtex")
(add-hook 'latex-mode-hook (lambda () (local-set-key (kbd "C-j") 'newline-and-indent)))
(add-hook 'latex-mode-hook (lambda () (local-set-key (kbd "C-c m") (lambda (arg) (interactive "p")
                                                                     (debug-ito-insert-pair arg "$" "$")))))
(add-hook 'latex-mode-hook (lambda () (local-set-key (kbd "C-c i")
                                                     (lambda (arg) (interactive "p")
                                                       (debug-ito-insert-pair arg "\\color{colmodified}" "\\color{colstayed}")))))
(add-hook 'latex-mode-hook (lambda () (add-to-list 'tex-compile-commands '("jbibtex %r"))))

;;;;;;;;;;;;;;;;;; Wanderlust key customization
(add-hook 'wl-summary-mode-hook (lambda () (local-set-key (kbd "C-d") 'debug-ito-wl-dup-and-show-message)))
(add-hook 'wl-summary-mode-hook (lambda () (local-set-key (kbd "M-d") 'debug-ito-wl-dup-message-buf)))

;;;;;;;;;;;;;;;;;; MHC
(autoload 'mhc-wl-setup "mhc-wl")
(add-hook 'wl-init-hook 'mhc-wl-setup)
(setq mhc-base-folder "+schedule")
(setq mhc-mail-path   (expand-file-name "~/Mail"))

;;;;;;;;;;;;;;;;;;; For Japanese printing
;; See also https://wiki.ubuntulinux.jp/UbuntuTips/Application/EmacsJapanesePrint
(setq ps-multibyte-buffer 'non-latin-printer)
(require 'ps-mule)
(defalias 'ps-mule-header-string-charsets 'ignore)

;;;;;;;;;;;;;;;;;;;; twittering mode  http://twmode.sourceforge.net/index.html
(require 'twittering-mode)
(setq twittering-icon-mode t)
(setq twittering-use-master-password t)
(setq twittering-status-format "%i %s,  %C{%Y/%m/%d %H:%M:%S}:\n%FILL[  ]{%T // %r%R}\n ")

;;;;;;;;;;;;;; (C)Perl mode
;; http://www.emacswiki.org/emacs/CPerlMode
;; (defalias 'perl-mode 'cperl-mode)
(setq cperl-invalid-face 'off
      cperl-electric-lbrace-space t
      cperl-close-paren-offset -4
      cperl-continued-statement-offset 4
      cperl-indent-level 4
      cperl-indent-parens-as-block t
      cperl-highlight-variables-indiscriminately t)
;; Forbid ugly special color theme. Standard theme is the most beautiful.
(copy-face font-lock-variable-name-face 'cperl-array-face)
(copy-face font-lock-variable-name-face 'cperl-hash-face)
(copy-face font-lock-keyword-face       'cperl-nonoverridable-face)
(add-hook 'cperl-mode-hook
          (lambda ()
            (local-set-key (kbd "M-m") (lambda () (interactive) (insert "my ")))
            (local-set-key (kbd "<tab>") 'indent-for-tab-command)))


;;;;;;;;;;;;;;;;;;; Rotate text
;; https://github.com/debug-ito/rotate-text.el
(require 'rotate-text)
(defvar debug-ito-rotate-symbolics
  '(("->" "=>" ">=" "<=" ">" "<") ("$" "&" "$$" "\\\\" "%" "@")))
(defun debug-ito-rotate-symbolic-characters (original arg)
  (let ((temp-symbolics debug-ito-rotate-symbolics)
        replacement)
    (dolist (symbols debug-ito-rotate-symbolics)
      (or replacement
          (setq replacement (rotate-text-replacement symbols original arg))))
    replacement))
(add-to-list 'rotate-text-patterns '("[<>]=\\|[-=]?>\\|[\\$%&@<]" debug-ito-rotate-symbolic-characters))


;;;;;;;;;;;;;;;;;;; Other settings
;; http://d.hatena.ne.jp/rubikitch/20090219/sequential_command
(require 'sequential-command-config)
(sequential-command-setup-keys)

;; http://www.emacswiki.org/emacs/sticky.el
(require 'sticky)
(use-sticky-key 'muhenkan sticky-alist:ja)


;;;;;;;;;;;;; Key customization

;; C-h as backspace in i-search mode
;; http://www.hpcs.cs.tsukuba.ac.jp/~yonemoto/pukiwiki/index.php?Meadow%2F.emacs
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)

;; (setq parens-require-spaces nil)

;; Always-enabled global key setting
;; http://pqrs.org/emacs/doc/keyjack-mode/index.html
;; http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
;;
;; kbd macro seems to accept constants only. We have to repeat writing it.
(setq my-keyjack-mode-map (make-sparse-keymap))
(mapcar (lambda (x)
          (message "arg: %s" x)
          (define-key my-keyjack-mode-map (car x) (cdr x))
          (global-set-key (car x) (cdr x)))
        `((,(kbd "C-h") . backward-delete-char-untabify)
          (,(kbd "C-x C-h") . help)
          ;; One-touch parentheses, quotes and others
          (,(kbd "C-c h") . (lambda (arg) (interactive "p") (debug-ito-insert-pair arg "[" "]")))
          (,(kbd "C-c j") . (lambda (arg) (interactive "p") (debug-ito-insert-pair arg "(" ")")))
          (,(kbd "C-c k") . (lambda (arg) (interactive "p") (debug-ito-insert-pair arg "{" "}")))
          (,(kbd "C-c l") . (lambda (arg) (interactive "p") (debug-ito-insert-pair arg "\"" "\"")))
          (,(kbd "C-c ;") . (lambda (arg) (interactive "p") (debug-ito-insert-pair arg "'" "'")))
          (,(kbd "C-c n") . (lambda (arg) (interactive "p") (debug-ito-insert-pair arg "<" ">")))
          ;; One-touch opening a block
          (,(kbd "C-c C-j") . debug-ito-open-block)
          ;; sequential numbering
          (,(kbd "C-x r n" ) . debug-ito-sequence-rectangle)
          ;; some other settings
          (,(kbd "C-;") . toggle-input-method)
          (,(kbd "C-\\") . "_")
          (,(kbd "C-^") . enlarge-window)
          (,(kbd "C-~") . shrink-window)
          (,(kbd "C-]") . enlarge-window-horizontally)
          (,(kbd "C-}") . shrink-window-horizontally)
          (,(kbd "C-x C-n") . next-multiframe-window)
          (,(kbd "C-x C-p") . previous-multiframe-window)
          (,(kbd "M-i") . complete-symbol)
          (,(kbd "C-.") . (lambda () (interactive) (tags-apropos " ")))
          (,(kbd "M-,") . (lambda () (interactive) (find-tag "" t)))
          (,(kbd "C-o") . (lambda () (interactive) (rotate-text 1  "$")))
          (,(kbd "C-t") . (lambda () (interactive) (rotate-text-backward 1 "@")))
          (,(kbd "C-c C-o") . (lambda () (interactive) (rotate-text 1  "->")))
          (,(kbd "<C-tab>") . dabbrev-expand)))

(define-minor-mode my-keyjack-mode
  "A minor mode so that my key settings override annoying major modes."
  t nil 'my-keyjack-mode-map)

(my-keyjack-mode 1)