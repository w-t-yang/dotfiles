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

(defvar wy-content-margin 0)
(defun wy-set-content-margin (margin)
  "Set MARGIN of content."
  (interactive "nMargin: ")
  (setq wy-content-margin margin)
  )

(defvar wy-content-margin-toggled nil)
(defun wy-toggle-content-margin ()
  "Apply MARGIN to current window."
  (interactive)
  (if (eq wy-content-margin-toggled nil)
      (progn
        (fringe-mode 0) ;; No fringes
        ;; Set default content margin if it's 0
        (if (eq wy-content-margin 0)
            (setq wy-content-margin 64)
          )
        (set-window-margins nil wy-content-margin wy-content-margin)
        (setq left-margin-width wy-content-margin)
        (setq right-margin-width wy-content-margin)
        (setq-local wy-content-margin-toggled t)
        )
    (progn
      (fringe-mode nil) ;; Default fringes
      (set-window-margins nil 0 0)
      (setq left-margin-width 0)
      (setq right-margin-width 0)
      (setq-local wy-content-margin-toggled nil)
      )
    )
  )

(defun wy-make-shell (name)
  "Create a shell buffer named NAME."
  (interactive "sName: ")
  (setq name (format "[%s]$%s" (projectile-project-name) name))
  (if (get-buffer name)
      (switch-to-buffer name)
    (progn
      (eshell)
      (rename-buffer name)
      )
    )
  )

(defvar wy-shrink-window-steps 32)

(defun wy-split-2-windows ()
  "Split frame into 2 windows."
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (shrink-window-horizontally wy-shrink-window-steps)
  (dired (projectile-project-root))
  (wy-make-shell "cmd")
  (next-window-any-frame)
  )

(defun wy-split-3-windows ()
  "Split frame into 3 windows."
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (shrink-window-horizontally wy-shrink-window-steps)
  (dired (projectile-project-root))
  (split-window-below)
  (next-window-any-frame)
  (wy-make-shell "cmd")
  (next-window-any-frame)
  )

(defun wy-split-4-windows ()
  "Split frame into 4 windows."
  (interactive)
  (delete-other-windows)
  (dired (projectile-project-root))
  (split-window-right)
  (split-window-below)
  (wy-make-shell "1")
  (next-window-any-frame)
  (wy-make-shell "2")
  (next-window-any-frame)
  (wy-make-shell "3")
  (split-window-below)
  (next-window-any-frame)
  (wy-make-shell "4")
  (next-window-any-frame))

(global-set-key (kbd "M-2") 'wy-split-2-windows)
(global-set-key (kbd "M-3") 'wy-split-3-windows)
(global-set-key (kbd "M-4") 'wy-split-4-windows)

(defun wy-highlight-selected-window ()
  "Highlight selected window."
  (walk-windows (lambda (w)
                  (unless (eq w (selected-window))
                    (with-current-buffer (window-buffer w)
                      (buffer-face-set `(:background ,(face-attribute 'fringe :background)))))))
  (buffer-face-set 'default))

(add-hook 'buffer-list-update-hook 'wy-highlight-selected-window)

;; https://whatacold.io/blog/2022-05-23-emacs-duplicate-line/
(defun wy-duplicate-line(comment-first)
  "Duplicate the current line, comment line if COMMENT-FIRST is set."
  (interactive "P")
  (let ((line-text (buffer-substring-no-properties
                    (line-beginning-position)
                    (line-end-position))))
    (save-excursion
      (if comment-first
          (progn
            (comment-line 1)
            (move-beginning-of-line 1)
            (open-line 1))
        (move-end-of-line 1)
        (open-line 1)
        (forward-char))
      (insert line-text))
    (forward-line)))

(defun wy-consult-buffer-other-window ()
  "Assume 2 frames are open, working in the main buffer.
Switch to the other buffer, and run 'consult-buffer'."
  (interactive)
  (next-window-any-frame)
  (consult-buffer)
  (next-window-any-frame)
  )

(defun wy-switch-buffers ()
  "Assume 2 frames are open, switch content in 2 buffers."
  (interactive)
  (let (
        (main-buffer (buffer-name))
        (main-point (point))
        (other-buffer (buffer-name))
        (other-point (point))
        )
    (progn
      (next-window-any-frame)
      (setq other-buffer (buffer-name))
      (setq other-point (point))
      (switch-to-buffer main-buffer)
      (goto-char main-point)
      (next-window-any-frame)
      (switch-to-buffer other-buffer)
      (goto-char other-point)
      )
    )
  )

;;; wy-functions.el ends here
