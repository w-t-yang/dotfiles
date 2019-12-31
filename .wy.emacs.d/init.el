(defun wy-split-window-below ()
  "Splite window below"
  (interactive)
  (split-window-below)
  (evil-window-increase-height 24)
  )

(defun wy-increase-window-height ()
  "Increase window height"
  (interactive)
  (evil-window-increase-height 10)
  )

(defun wy-decrease-window-height ()
  "Decrease window height"
  (interactive)
  (evil-window-increase-height -10)
  )

(defun wy-increase-window-width ()
  "Increase window width"
  (interactive)
  (evil-window-increase-width 5)
  )

(defun wy-decrease-window-width ()
  "Decrease window width"
  (interactive)
  (evil-window-increase-width -5)
  )

(global-set-key (kbd "M-<up>") 'wy-increase-window-height)
(global-set-key (kbd "M-<down>") 'wy-decrease-window-height)
(global-set-key (kbd "M-<right>") 'wy-increase-window-width)
(global-set-key (kbd "M-<left>") 'wy-decrease-window-width)

(defun wy-open-eshell ()
  "Open a new eshell, and add to current layout"
  (interactive)
  (eshell 'N)
  )

(defun wy-clear-eshell-buffer ()
  "Clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    )
  )

(load-file "~/.wy.emacs.d/wy-explorer.el")

;; Disable eshell-prompt-extras epe-git-p, if eshell gets extremely slow
