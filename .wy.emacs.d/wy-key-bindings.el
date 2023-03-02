;;; wy-key-bindings.el --- Key bindings
;;; Commentary:
;;; Code:

(global-set-key (kbd "RET") 'newline)

(global-set-key (kbd "M-a") 'backward-paragraph)
(global-set-key (kbd "M-e") 'forward-paragraph)
(global-set-key (kbd "M-g b") 'dumb-jump-back)
(global-set-key (kbd "M-g e") 'embark-act)
(global-set-key (kbd "M-g g") 'dumb-jump-go)
(global-set-key (kbd "M-g i") 'consult-imenu)
(global-set-key (kbd "M-g l") 'goto-line)
(global-set-key (kbd "M-g o") 'org-open-at-point)

(define-prefix-command 'window-operations)
(global-set-key (kbd "C-o") 'window-operations)
(global-set-key (kbd "C-o o") 'sanityinc/toggle-delete-other-windows)
(global-set-key (kbd "M-1") 'sanityinc/toggle-delete-other-windows)
(global-set-key (kbd "C-o i") 'delete-window)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "C-o r") 'split-window-right)
(global-set-key (kbd "C-o b") 'split-window-below)
(global-set-key (kbd "C-o m s") 'wy-set-content-margin)
(global-set-key (kbd "C-o m t") 'wy-toggle-content-margin)

(global-set-key (kbd "M-o") 'next-window-any-frame)
(global-set-key (kbd "M-q") 'kill-current-buffer)
(global-set-key (kbd "M-s g") 'rg)
(global-set-key (kbd "M-s s") 'ispell-word)
;;(global-set-key (kbd "C-t") 'embark-act)
(global-set-key (kbd "M-t") 'cycle-spacing)

(global-set-key (kbd "C-<tab>") 'consult-buffer)
(global-set-key (kbd "C-\\") 'session-jump-to-last-change)
(global-set-key (kbd "C-|") 'split-window-right)
(global-set-key (kbd "C-_") 'split-window-below)
(global-set-key (kbd "M--") 'shrink-window-horizontally)
(global-set-key (kbd "M-=") 'enlarge-window-horizontally)
(global-set-key (kbd "M-_") 'shrink-window)
(global-set-key (kbd "M-+") 'enlarge-window)
(global-set-key (kbd "C-,") 'xref-find-definitions)
(global-set-key (kbd "C-.") 'xref-find-references)
(global-set-key (kbd "C-M-v") 'scroll-other-window)
(global-set-key (kbd "M-V") 'scroll-other-window-down)

(add-hook 'magit-mode-hook
          (lambda ()
            (local-unset-key (kbd "C-<tab>"))
            (local-unset-key (kbd "M-0"))
            (local-unset-key (kbd "M-1"))
            (local-unset-key (kbd "M-2"))
            (local-unset-key (kbd "M-3"))
            (local-unset-key (kbd "M-4"))
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
