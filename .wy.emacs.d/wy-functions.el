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
  (if (get-buffer name)
      (switch-to-buffer name)
    (progn
      (eshell)
      (rename-buffer name)
      )
    )
  )

(defvar wy-project-root "~/Projects")

(defun wy-split-2-windows ()
  "Split frame into 2 windows."
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (shrink-window-horizontally 20)
  (dired wy-project-root)
  (next-window-any-frame)
  )

(defun wy-split-3-windows ()
  "Split frame into 3 windows."
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (shrink-window-horizontally 20)
  (dired wy-project-root)
  (split-window-below)
  (next-window-any-frame)
  (next-window-any-frame)
  )

(defun wy-split-4-windows ()
  "Split frame into 4 windows."
  (interactive)
  (delete-other-windows)
  (dired wy-project-root)
  (split-window-below)
  (split-window-right)
  (wy-make-shell "1")
  (next-window-any-frame)
  (wy-make-shell "2")
  (next-window-any-frame)
  (wy-make-shell "3")
  (split-window-right)
  (next-window-any-frame)
  (wy-make-shell "4")
  (next-window-any-frame))

(global-set-key (kbd "M-1") 'sanityinc/toggle-delete-other-windows)
(global-set-key (kbd "M-2") 'wy-split-2-windows)
(global-set-key (kbd "M-3") 'wy-split-3-windows)
(global-set-key (kbd "M-4") 'wy-split-4-windows)

(defvar wy-other-window-background (face-attribute 'fringe :background))

(defun wy-highlight-selected-window ()
  "Highlight selected window."
  (walk-windows (lambda (w)
                  (unless (eq w (selected-window))
                    (with-current-buffer (window-buffer w)
                      (buffer-face-set `(:background ,wy-other-window-background))))))
  (buffer-face-set 'default))

(add-hook 'buffer-list-update-hook 'wy-highlight-selected-window)

;;; wy-functions.el ends here
