;;; wy-functions.el --- Helper functions
;;; Commentary:
;;; Code:

(defun wy/open-remote-root ( server )
  "Open the root folder on a remote SERVER."
  (interactive "sEnter the server name: ")
  (find-file (format "/ssh:%s:/usr/local/git_tree" server))
  )
;; (global-set-key (kbd "C-\\") 'wy/open-remote-root)

(defun window-half-height ()
  "Half height of 'selected-window'."
  (max 1
       (/ (window-height (selected-window)) 2)
       )
  )

(defun scroll-up-half ()
  "Scroll half up."
  (interactive)
  (scroll-up (window-half-height)))

(defun scroll-down-half ()
  "Scroll half down."
  (interactive)
  (scroll-down (window-half-height)))

(global-set-key (kbd "C-v") 'scroll-up-half)
(global-set-key (kbd "M-v") 'scroll-down-half)

;; (defun wy/toggle-highlighting-word-at-point ()
;;   "Highlight or unhighlight the word where cursor lies."
;;   (interactive)
;;   (if hi-lock-interactive-patterns
;;       (unhighlight-regexp (car (car hi-lock-interactive-patterns)))
;;     (highlight-symbol-at-point))
;;   )

(defun make-shell (name)
  "Create a shell buffer named NAME."
  (interactive "sName: ")
  (setq name (concat "$" name))
  (eshell)
  (rename-buffer name))

;;; wy-functions.el ends here
