;;; wy-key-bindings.el --- Key bindings
;;; Commentary:
;;; Code:

;;; Binding Map
;;; Use the snippet below to list existing key bindings
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

;;  C-a    move-beginning-of-line            M-a    backward-sentence
(global-set-key (kbd "M-a") 'backward-paragraph)
;;  C-b    backward-char                     M-b    backward-word
;;  C-c    prefix                            M-c    capitalize-word -> org-orgcapture
;;  C-d    paredit-forward-delete            M-d    paredit-forward-kill-word
;;  C-e    move-end-of-line                  M-e    forward-sentence
(global-set-key (kbd "M-e") 'forward-paragraph)
;;  C-f    forward-char                      M-f    forward-word
;;  C-g    keyboard-quit                     M-g    go-to
(global-set-key (kbd "M-g l") 'goto-line)
(global-set-key (kbd "M-g b") 'dumb-jump-back)
(global-set-key (kbd "M-g g") 'dumb-jump-go)
(global-set-key (kbd "M-g o") 'org-open-at-point)
;;  C-h    help-command                      M-h    ns-do-hide-emacs
;;  C-i    tab                               M-i    tab-to-tab-stop
;;  C-j    paredit-newline                   M-j    indent-new-comment-line
;;  C-k    paredit-kill                      M-k    kill-sentence
;;  C-l    recenter-top-bottom               M-l    downcase-word
;;  C-m    enter                             M-m    back-to-indentation
(global-set-key (kbd "C-\\") 'session-jump-to-last-change)
;;  C-n    next-line                         M-n    comint-next-input/symbol-jump
;;  C-o    sanityinc/open-line-with-reindent M-o    facemenu-keymap
(define-prefix-command 'window-operations)
(global-set-key (kbd "C-o") 'window-operations)
(global-set-key (kbd "C-o o") 'sanityinc/toggle-delete-other-windows)
(global-set-key (kbd "C-o i") 'delete-window)
(global-set-key (kbd "M-o") 'next-window-any-frame)
;;  C-p    previous-line                     M-p    comint-previous-input/symbol-jump
;;  C-q    quoted-insert                     M-q    paredit-reindent-defun
(global-set-key (kbd "M-q") 'kill-current-buffer)
;;  C-r    isearch-backward                  M-r    paredit-raise-sexp
;;  C-s    isearch-forward                   M-s    search
(global-set-key (kbd "M-s g") 'rgrep)
(global-set-key (kbd "M-s s") 'ispell-word)
;;  C-t    transpose-chars                   M-t    transpose-words
(global-set-key (kbd "C-t") 'move-to-window-line-top-bottom)
(global-set-key (kbd "M-t") 'cycle-spacing)
;;  C-u    universal-argument                M-u    upcase-word
;;  C-v    cua-scroll-up                     M-v    cua-scroll-down
;;  C-w    whole-line-or-region-kill-region  M-w    whole-line-or-region-kill-ring-save
;;  C-x    Control-X-prefix                  M-x    M-x
;;  C-y    whole-line-or-region-yank         M-y    cua-paste-pop
;;  C-z    sanityinc/maybe-suspend-frame     M-z    zap-to-ch

;;  C-TAB  undefined                         M-TAB  is undefined
(global-set-key (kbd "C-<tab>") 'consult-buffer)
;;  C-`    undefined                         M-`    ns-next-frame
;;  C--    negative-argument                 M--    negative-argument
(global-set-key (kbd "C--") 'dash-at-point)
;;  C-=    expand-region                     M-=    count-words-region
;;  C-[]   esc;abort-recursive-edit          M-[]   undefined
;;  C-{    paredit-backward-barf-sexp        M-{    backward-paragraph
;;  C-}    paredit-forward-barf-sexp         M-}    forward-paragraph
;;  C-\    toggle-input-method               M-\    delete-horizontal-space (switch input method)
;;  C-|    undefined                         M-|    shell-command-on-region
;;  C-;    avy-goto-char-timer               M-;    paredit-comment-dwim
;;  C-'    undefined                         M-'    abbrev-prefix-mark
;;  C-:    undefined                         M-:    pp-eval-expression
;;  C-,    undefined                         M-,    pop-tag-mark
;;  C-.    cua-set-mark                      M-.    elisp-slime-nav-find-elisp-thing-at-point
(global-set-key (kbd "C-,") 'xref-find-definitions)
(global-set-key (kbd "C-.") 'xref-find-references)
;;  C-<    mc/mark-previous-like-this        M-<    beginning-of-buffer
;;  C->    mc/mark-next-like-this            M->    end-of-buffer
;;  C-/    undo                              M-/    company-complete
;;  C-?    undefined                         M-?    sanityinc/counsel-search-project

;;  C-S-Backspace kill-whole-line

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

;; (global-set-key (kbd "RET") 'newline-and-indent)

;;; wy-key-bindings.el ends here
