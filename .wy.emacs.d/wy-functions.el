;;; wy-functions.el --- helper functions
;;; wy-vanilla.el --- WY's emacs config
;;; Commentary:
;;; Helper functions

;;; Code:

(defun wy-open-remote-root ( server )
  "Open the root folder on a remote SERVER."
  (interactive "sEnter the server name: ")
  (find-file (format "/ssh:%s:/usr/local/git_tree" server))
  )

(defun window-half-height ()
  "Half height of 'selected-window'."
  (max 1
       (/
        (window-height (selected-window))
        2)
       )
  )

(defun scroll-up-half ()
  "Scroll half up."
  (interactive)
  (cua-scroll-up (window-half-height)))

(defun scroll-down-half ()
  "Scroll half down."
  (interactive)
  (cua-scroll-down (window-half-height)))

;;; wy-functions.el ends here
