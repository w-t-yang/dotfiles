;;; wy-vanilla.el --- WY's emacs config
;;; Commentary:
;;; configuration based on Purcell's work

;;; Code:

(require 'init-elpa)
(load-file "~/.wy.emacs.d/wy-functions.el")

;;; Themes
(require-package 'zenburn-theme)
(load-theme 'zenburn t)

;;(add-to-list 'default-frame-alist '(font . FONT ))
(set-face-attribute 'default nil :family "Courier New" :height 160)


;;; Search
;; For fuzzy search, manually enable it everytime
;; by calling ivy-toggle-fuzzy
;; (setq ivy-re-builders-alist
;;       '((t . ivy--regex-fuzzy)))

;;; Remote
;; (setq tramp-verbose 6)
(setq ivy-rich-parse-remote-buffer nil)
(require 'tramp)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
(setq tramp-default-method "ssh")
(setq tramp-completion-reread-directory-timeout nil)
(setq tramp-auto-save-directory "~/tmp/tramp/")
(setq tramp-chunksize 2000)
(setq remote-file-name-inhibit-cache nil)


;;; Project


;;; Binding Map
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
;;  C-c    prefix                            M-c    capitalize-word
;;  C-d    paredit-forward-delete            M-d    paredit-forward-kill-word
;;  C-e    move-end-of-line                  M-e    forward-sentence
(global-set-key (kbd "M-e") 'forward-paragraph)
;;  C-f    forward-char                      M-f    forward-word
;;  C-g    keyboard-quit                     M-g    go-to

;;  C-h    help-command                      M-h    ns-do-hide-emacs

;;  C-i    tab                               M-i    tab-to-tab-stop
;;  C-j    paredit-newline                   M-j    indent-new-comment-line
;;  C-k    paredit-kill                      M-k    kill-sentence
;;  C-l    recenter-top-bottom               M-l    downcase-word
;;  C-m    enter                             M-m    back-to-indentation
;;  C-n    next-line                         M-n    comint-next-input
;;  C-o    sanityinc/open-line-with-reindent M-o    facemenu-keymap

;;  C-p    previous-line                     M-p    comint-previous-input
;;  C-q    quoted-insert                     M-q    paredit-reindent-defun
;;  C-r    isearch-backward                  M-r    paredit-raise-sexp
;;  C-s    isearch-forward                   M-s    search

;;  C-t    transpose-chars                   M-t    transpose-words
;;  C-u    universal-argument                M-u    upcase-word
;;  C-v    cua-scroll-up                     M-v    cua-scroll-down
(global-set-key (kbd "C-v") 'scroll-up-half)
(global-set-key (kbd "M-v") 'scroll-down-half)
;;  C-w    whole-line-or-region-kill-region  M-w    whole-line-or-region-kill-ring-save
;;  C-x    Control-X-prefix                  M-x    M-x
;;  C-y    whole-line-or-region-yank         M-y    cua-paste-pop
;;  C-z    sanityinc/maybe-suspend-frame     M-z    zap-to-ch



;;  C-TAB  undefined                         M-TAB  is undefined
(global-set-key (kbd "C-<tab>") 'projectile-switch-to-buffer)
;;  C-`    undefined                         M-`    ns-next-frame
;;  C--    negative-argument                 M--    negative-argument
;;  C-=    expand-region                     M-=    count-words-region
;;  C-[]   esc;abort-recursive-edit          M-[]   undefined
;;  C-{    paredit-backward-barf-sexp        M-{    backward-paragraph
;;  C-}    paredit-forward-barf-sexp         M-}    forward-paragraph
;;  C-\    toggle-input-method               M-\    delete-horizontal-space (switch input method)
(global-set-key (kbd "C-\\") 'wy-open-remote-root)
;;  C-|    undefined                         M-|    shell-command-on-region
;;  C-;    avy-goto-char-timer               M-;    paredit-comment-dwim
;;  C-'    undefined                         M-'    abbrev-prefix-mark
;;  C-:    undefined                         M-:    pp-eval-expression
;;  C-,    undefined                         M-,    pop-tag-mark
;;  C-.    cua-set-mark                      M-.    elisp-slime-nav-find-elisp-thing-at-point
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

(global-set-key (kbd "RET") #'newline-and-indent)

;;; lsp

;; when lsp is not enable/available, use dumb-jump for "jump to definition"
(require-package 'dumb-jump)

;;(setq lsp-keymap-prefix "M-l")
(require-package 'lsp-mode)
(require-package 'lsp-ui)
(setq lsp-ui-doc-enable nil)
(require-package 'company-lsp)
;;(require-package 'lsp-treemacs)
(require-package 'lsp-ivy)
;;(require-package 'dap-mode)
(require-package 'yasnippet)

;;(require 'lsp-mode)

;; Swift
;;(require-package 'lsp-sourcekit)

;; Java
;; (require-package 'lsp-java)
;; (add-hook 'java-mode-hook #'lsp)
(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 2
                                  tab-width 2
                                  )))

;; (require-package 'meghanada)
;; (add-hook 'java-mode-hook
;;           (lambda ()
;;             ;; meghanada-mode on
;;             (meghanada-mode t)
;;             ;; enable telemetry
;;             (meghanada-telemetry-enable t)
;;             (flycheck-mode +1)
;;             (setq c-basic-offset 2)
;;             ;; use code format
;;             (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))
;; (cond
;;  ((eq system-type 'windows-nt)
;;   (setq meghanada-java-path (expand-file-name "bin/java.exe" (getenv "JAVA_HOME")))
;;   (setq meghanada-maven-path "mvn.cmd"))
;;  (t
;;   (setq meghanada-java-path "java")
;;   (setq meghanada-maven-path "mvn")))


(setq company-minimum-prefix-length 1 company-idle-delay 0.0) ;; default is 0.2
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq lsp-idle-delay 0.500)
(setq lsp-inhibit-message t)

;;; Performance
(setq desktop-restore-eager 7)

(provide 'wy-vanilla)

;;; wy-vanilla.el ends here
