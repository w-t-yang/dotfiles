;;; wy-key-bindings.el --- Key bindings
;;; Commentary:
;;; Code:

(global-set-key (kbd "M-a") 'backward-paragraph)
(global-set-key (kbd "M-e") 'forward-paragraph)
(global-set-key (kbd "M-g l") 'goto-line)
(global-set-key (kbd "M-g b") 'dumb-jump-back)
(global-set-key (kbd "M-g g") 'dumb-jump-go)
(global-set-key (kbd "M-g o") 'org-open-at-point)
(global-set-key (kbd "C-i") 'consult-imenu)

(define-prefix-command 'window-operations)
(global-set-key (kbd "C-o") 'window-operations)
(global-set-key (kbd "C-o o") 'sanityinc/toggle-delete-other-windows)
(global-set-key (kbd "C-o i") 'delete-window)
(global-set-key (kbd "C-o r") 'split-window-right)
(global-set-key (kbd "C-o b") 'split-window-below)

(global-set-key (kbd "M-o") 'next-window-any-frame)
(global-set-key (kbd "M-q") 'kill-current-buffer)
(global-set-key (kbd "M-s g") 'rgrep)
(global-set-key (kbd "M-s s") 'ispell-word)
(global-set-key (kbd "C-t") 'move-to-window-line-top-bottom)
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

(defun wy-get-key-binding-func (key)
  "Get binding function for given KEY."
  (setq-local des (describe-key-briefly (kbd key)))
  (if (string-match-p "undefined" des)
      (setq res `("undefined"))
    (setq res (last (split-string des " ")))
    )
  (car res)
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
    (setq ckey (concat "C-" element))
    ;;(setq Ckey (concat "C-" (upcase element)))
    (setq mkey (concat "M-" element))
    (setq cmkey (concat "C-M-" element))
    (setq Mkey (concat "M-" (upcase element)))

    (setq cres (wy-get-key-binding-func ckey))
    ;;(setq Cres (wy-get-key-binding-func Ckey))
    (setq mres (wy-get-key-binding-func mkey))
    (setq cmres (wy-get-key-binding-func cmkey))
    (setq Mres (wy-get-key-binding-func Mkey))

    (insert "|-\n")
    (insert "|" ckey "|" mkey "|" cmkey "|" Mkey "|\n")
    (insert "|" cres "|" mres "|" cmres "|" Mres "|\n")
    )
  (insert "|-\n")

  (insert "\n")
  (setq wy-keys '("<tab>" "`" "-" "=" "[" "]" "{" "}" "\\" ";" "'" ":" "," "." "<" ">" "/" "?"))
  ;; Missing char '|', because it conflicts with org-mode table
  (dolist (element wy-keys)
    (setq ckey (concat "C-" element))
    (setq mkey (concat "M-" element))
    (setq cmkey (concat "C-M-" element))

    (setq cres (wy-get-key-binding-func ckey))
    (setq mres (wy-get-key-binding-func mkey))
    (setq cmres (wy-get-key-binding-func cmkey))

    (insert "|-\n")
    (insert "|" ckey "|" mkey "|" cmkey "|\n")
    (insert "|" cres "|" mres "|" cmres "|\n")
    )
  (insert "|-\n")
  )
;;; wy-key-bindings.el ends here
