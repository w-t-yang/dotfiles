;;; wy-key-bindings.el --- Key bindings
;;; Commentary:
;;; Code:

(require 'init-elpa "~/.emacs.d/lisp/init-elpa.el")

;; (require-package 'evil)
;; (evil-mode 1)

(global-set-key (kbd "RET") 'newline)

(global-set-key (kbd "C-x f") 'find-file)
(global-set-key (kbd "C-x s") 'save-buffer)

(global-set-key (kbd "M-a") 'backward-paragraph)
(global-set-key (kbd "M-e") 'forward-paragraph)

;; M-c for consult
(define-prefix-command 'consult-operations)
(global-set-key (kbd "M-c") 'consult-operations)
(global-set-key (kbd "M-c e") 'consult-flymake)
(global-set-key (kbd "M-c f") 'consult-find)
(global-set-key (kbd "M-c g") 'consult-ripgrep)
(global-set-key (kbd "M-c h") 'consult-history)
(global-set-key (kbd "M-c i") 'consult-imenu)
(global-set-key (kbd "M-c l") 'consult-line)
(global-set-key (kbd "M-c o") 'consult-outline)
(global-set-key (kbd "M-c s") 'consult-eglot-symbols)
(global-set-key (kbd "M-c t") 'consult-theme)

;; M-m for mark
(require-package 'expand-region)
(require 'expand-region)
(define-prefix-command 'mark-operations)
(global-set-key (kbd "M-m") 'mark-operations)
(global-set-key (kbd "M-m '") 'er/mark-inside-quotes)
(global-set-key (kbd "M-m f") 'er/mark-defun)
(global-set-key (kbd "M-m i") 'er/mark-inside-pairs)
(global-set-key (kbd "M-m o") 'er/mark-outside-pairs)
(global-set-key (kbd "M-m p") 'er/mark-paragraph)
(global-set-key (kbd "M-m s") 'er/mark-symbol)
(global-set-key (kbd "M-m w") 'er/mark-subword)

;; M-g for go
(global-set-key (kbd "M-g b") 'dumb-jump-back)
(global-set-key (kbd "M-g g") 'dumb-jump-go)
(global-set-key (kbd "M-g l") 'goto-line)
(global-set-key (kbd "M-g o") 'org-open-at-point)

(global-set-key (kbd "C-o") 'next-window-any-frame)

(define-prefix-command 'open-operations)
(global-set-key (kbd "M-o") 'open-operations)
(global-set-key (kbd "M-o d") 'eldoc)
(global-set-key (kbd "M-o e") 'embark-act)
(global-set-key (kbd "M-o m s") 'wy-set-content-margin)
(global-set-key (kbd "M-o m m") 'wy-toggle-content-margin)

(require-package 'git-link)
(require 'git-link)
(setq git-link-open-in-browser t)
(global-set-key (kbd "M-o g") 'git-link)


(global-set-key (kbd "M-q") 'kill-current-buffer)

;; M-s for select(mark)/search
(require-package 'rg)
(global-set-key (kbd "M-s g") 'rg)
(global-set-key (kbd "M-s s") 'ispell-word)

(global-set-key (kbd "C-t") 'cycle-spacing)
;; (global-set-key (kbd "M-t") 'cycle-spacing)

(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'sanityinc/toggle-delete-other-windows)

;; C-M-d down-list conflicts with Mac system shortcut "look up in dictionary"

;; Don't set C-M-g to any function, otherwise C-g won't work in control-mode
;; (global-set-key (kbd "C-M-g") 'conflict-with-control-mode)

(global-set-key (kbd "C-M-m") 'mark-sexp)
(global-set-key (kbd "C-M-,") 'er/expand-region)
(global-set-key (kbd "C-M-.") 'er/contract-region)

(global-set-key (kbd "C-<tab>") 'consult-buffer)
(global-set-key (kbd "C-x <tab>") 'wy-consult-buffer-other-window)
(global-set-key (kbd "C-x C-<tab>") 'wy-switch-buffers)
(global-set-key (kbd "C-\\") 'session-jump-to-last-change)
(global-set-key (kbd "C-|") 'split-window-right)
;; Conflict with undo-tree
;; (global-set-key (kbd "C-_") 'split-window-below)
(global-set-key (kbd "M--") 'shrink-window-horizontally)
(global-set-key (kbd "M-=") 'enlarge-window-horizontally)
(global-set-key (kbd "M-_") 'shrink-window)
(global-set-key (kbd "M-+") 'enlarge-window)
(global-set-key (kbd "C-,") 'xref-find-definitions)
(global-set-key (kbd "C-.") 'xref-find-references)
(global-set-key (kbd "M-,") 'xref-go-back)
(global-set-key (kbd "M-.") 'xref-go-forward)
(global-set-key (kbd "C-M-o") 'wy-duplicate-line)
(global-set-key (kbd "C-M-r") 'repeat)
(global-set-key (kbd "C-M-v") 'scroll-other-window)
(global-set-key (kbd "M-V") 'scroll-other-window-down)
(global-set-key (kbd "C-M-<left>") 'flymake-goto-prev-error)
(global-set-key (kbd "C-M-<right>") 'flymake-goto-next-error)

(add-hook 'magit-mode-hook
          (lambda ()
            (local-unset-key (kbd "C-<tab>"))
            (local-unset-key (kbd "M-0"))
            (local-unset-key (kbd "M-1"))
            (local-unset-key (kbd "M-2"))
            (local-unset-key (kbd "M-3"))
            (local-unset-key (kbd "M-4"))
            ))

(add-hook 'dired-mode-hook
          (lambda ()
            (local-unset-key (kbd "C-o"))))

(add-hook 'rg-mode-hook
          (lambda ()
            (local-unset-key (kbd "C-o"))))

(add-hook 'eshell-mode-hook
          (lambda ()
            (local-set-key (kbd "M-k") 'eshell/clear)
            ))

(defun wy-get-key-binding-func (key)
  "Get binding function for given KEY."
  (let ((des (describe-key-briefly (kbd key)))
        (res nil)
        )
    (if (string-match-p "undefined" des)
        (setq res `("undefined"))
      (setq res (last (split-string des " ")))
      )
    (car res)
    )
  )

;; Tips to use the following function
;; 1. Open an org file, and switch to the major mode that you want to generate
;;    the mapping for
;; 2. Call this function to generate the table
;; 3. Switch to org-mode, and format the table
;; 4. Done.
(defvar wy-keys '())
(defun wy-generate-key-binding-map ()
  "Generate key binding map."
  (interactive)
  (setq wy-keys '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))
  (dolist (element wy-keys)
    (let* ((ckey (concat "C-" element))
           (mkey (concat "M-" element))
           (cmkey (concat "C-M-" element))
           (Mkey (concat "M-" (upcase element)))
           (cres (wy-get-key-binding-func ckey))
           (mres (wy-get-key-binding-func mkey))
           (cmres (wy-get-key-binding-func cmkey))
           (Mres (wy-get-key-binding-func Mkey))
           )
      (insert "|-\n")
      (insert "|" ckey "|" mkey "|" cmkey "|" Mkey "|\n")
      (insert "|" cres "|" mres "|" cmres "|" Mres "|\n")
      )
    )
  (insert "|-\n")

  (insert "\n")
  (setq wy-keys '("<tab>" "`" "-" "=" "[" "]" "{" "}" "\\" ";" "'" ":" "," "." "<" ">" "/" "?"))
  ;; Missing char '|', because it conflicts with org-mode table
  (dolist (element wy-keys)
    (let* ((ckey (concat "C-" element))
           (mkey (concat "M-" element))
           (cmkey (concat "C-M-" element))
           (cres (wy-get-key-binding-func ckey))
           (mres (wy-get-key-binding-func mkey))
           (cmres (wy-get-key-binding-func cmkey))
           )
      (insert "|-\n")
      (insert "|" ckey "|" mkey "|" cmkey "|\n")
      (insert "|" cres "|" mres "|" cmres "|\n")
      )
    )
  (insert "|-\n")
  )
;;; wy-key-bindings.el ends here
