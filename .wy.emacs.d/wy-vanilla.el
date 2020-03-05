;;; wy-vanilla.el --- WY's emacs config
;;; Commentary:
;;; configuration based on Purcell's work

;;; Code:

(require 'init-elpa)

;; Themes
(require-package 'zenburn-theme)
(load-theme 'zenburn t)

;;(add-to-list 'default-frame-alist '(font . FONT ))
(set-face-attribute 'default nil :family "Courier New" :height 160)


;; Search
(setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy)))


;; Binding Map
;; (setq keys '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))
;; (progn
;;   (setq keys '("<tab>" "`" "-" "=" "[" "]" "{" "}" "\\" "|" ";" "'" ":" "," "." "<" ">" "/" "?"))
;;   (let (value)
;;     (dolist (element keys value)
;;       (setq value
;;             (cons (describe-key-briefly (kbd (concat "M-" (upcase element)))) value)
;;             )
;;       )
;;     )
;;   )

;;  C-a    move-beginning-of-line
;;  C-b    backward-char
;;  C-c    prefix
;;  C-d    paredit-forward-delete
;;  C-e    move-end-of-line
;;  C-f    forward-char
;;  C-g    keyboard-quit
;;  C-h    help-commandTAB runs the command ielm-tab
;;  C-j    paredit-newline
;;  C-k    paredit-kill
;;  C-l    recenter-top-bottomRET runs the command ielm-return
;;  C-n    next-line
;;  C-o    sanityinc/open-line-with-reindent
;;  C-p    previous-line
;;  C-q    quoted-insert
;;  C-r    isearch-backward
;;  C-s    isearch-forward
;;  C-t    transpose-chars
;;  C-u    universal-argument
;;  C-v    cua-scroll-up
;;  C-w    whole-line-or-region-kill-region
;;  C-x    Control-X-prefix
;;  C-y    whole-line-or-region-yank
;;  C-z    sanityinc/maybe-suspend-frame

;;  C-TAB  undefined
;;  C-`    undefined
;;  C--    negative-argument
;;  C-=    expand-region
;;  C-]    abort-recursive-edit
;;  C-{    paredit-backward-barf-sexp
;;  C-}    paredit-forward-barf-sexp
;;  C-\    toggle-input-method
;;  C-|    undefined
;;  C-;    avy-goto-char-timer
;;  C-'    undefined
;;  C-:    undefined
;;  C-,    undefined
;;  C-.    cua-set-mark
;;  C-<    mc/mark-previous-like-this
;;  C->    mc/mark-next-like-this
;;  C-/    undo
;;  C-?    undefined
;;  C-S-Backspace kill-whole-line

;;  M-a    backward-sentence
;;  M-b    backward-word
;;  M-c    capitalize-word
;;  M-d    paredit-forward-kill-word
;;  M-e    forward-sentence
;;  M-f    forward-word
;;  M-g    go-to
;;  M-h    ns-do-hide-emacs
;;  M-i    tab-to-tab-stop
;;  M-j    indent-new-comment-line
;;  M-k    kill-sentence
;;  M-l    downcase-word
;;  M-m    back-to-indentation
;;  M-n    comint-next-input
;;  M-o    facemenu-keymap
;;  M-p    comint-previous-input
;;  M-q    paredit-reindent-defun
;;  M-r    paredit-raise-sexp
;;  M-s    search
;;  M-t    transpose-words
;;  M-u    upcase-word
;;  M-v    cua-scroll-down
;;  M-w    whole-line-or-region-kill-ring-save
;;  M-x    M-x
;;  M-y    cua-paste-pop
;;  M-z    zap-to-ch

;;  M-TAB  is undefined
;;  M-`    ns-next-frame
;;  M--    negative-argument
;;  M-=    count-words-region
;;  M-[]   undefined
;;  M-{    backward-paragraph
;;  M-}    forward-paragraph
;;  M-\    delete-horizontal-space
;;  M-|    shell-command-on-region
;;  M-;    paredit-comment-dwim
;;  M-'    abbrev-prefix-mark
;;  M-:    pp-eval-expression
;;  M-,    pop-tag-mark
;;  M-.    elisp-slime-nav-find-elisp-thing-at-point
;;  M-<    beginning-of-buffer
;;  M->    end-of-buffer
;;  M-/    company-complete
;;  M-?    sanityinc/counsel-search-project



;;  M-A    is undefined
;;  M-B    is undefined
;;  M-C    is undefined
;;  M-D    is undefined
;;  M-E    is undefined
;;  M-F    is undefined
;;  M-G    is undefined
;;  M-H    is undefined
;;  M-I    is undefined
;;  M-J    paredit-join-sexps
;;  M-K    is undefined
;;  M-L    is undefined
;;  M-M    is undefined
;;  M-N    is undefined
;;  M-O    is undefined
;;  M-P    is undefined
;;  M-Q    is undefined
;;  M-R    is undefined
;;  M-S    paredit-split-sexp
;;  M-T    is undefined
;;  M-U    is undefined
;;  M-V    is undefined
;;  M-W    is undefined
;;  M-X    is undefined
;;  M-Y    browse-kill-ring
;;  M-Z    zap-up-to-char

(provide 'wy-vanilla)

;;; wy-vanilla.el ends here
