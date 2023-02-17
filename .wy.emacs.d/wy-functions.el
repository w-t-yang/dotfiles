;;; wy-functions.el --- Helper functions
;;; Commentary:
;;; Code:

;; (defun wy/open-remote-root ( server )
;;   "Open the root folder on a remote SERVER."
;;   (interactive "sEnter the server name: ")
;;   (find-file (format "/ssh:%s:/usr/local/git_tree" server))
;;   )
;; (global-set-key (kbd "C-\\") 'wy/open-remote-root)

;; (defun window-half-height ()
;;   "Half height of 'selected-window'."
;;   (max 1
;;        (/ (window-height (selected-window)) 2)
;;        )
;;   )

;; (defun scroll-up-half ()
;;   "Scroll half up."
;;   (interactive)
;;   (scroll-up (window-half-height)))

;; (defun scroll-down-half ()
;;   "Scroll half down."
;;   (interactive)
;;   (scroll-down (window-half-height)))

;; (global-set-key (kbd "C-v") 'scroll-up-half)
;; (global-set-key (kbd "M-v") 'scroll-down-half)

;; (defun wy/toggle-highlighting-word-at-point ()
;;   "Highlight or unhighlight the word where cursor lies."
;;   (interactive)
;;   (if hi-lock-interactive-patterns
;;       (unhighlight-regexp (car (car hi-lock-interactive-patterns)))
;;     (highlight-symbol-at-point))
;;   )

(defun wy-make-shell (name)
  "Create a shell buffer named NAME."
  (interactive "sName: ")
  (setq name (concat "$" name))
  (eshell)
  (rename-buffer name))

(defun wy-split-4-windows ()
  "Split frame into 4 windows."
  (interactive)
  (dired "~/Projects")
  (sanityinc/toggle-delete-other-windows)
  (split-window-below)
  (split-window-right)
  (next-window-any-frame)
  (next-window-any-frame)
  (split-window-right)
  (next-window-any-frame)
  (next-window-any-frame))

(defun wy-split-3-windows ()
  "Split frame into 4 windows."
  (interactive)
  (dired "~/Projects")
  (sanityinc/toggle-delete-other-windows)
  (split-window-right)
  (split-window-below)
  (shrink-window-horizontally 20))

;;; wy-functions.el ends here
