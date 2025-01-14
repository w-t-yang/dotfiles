;;; wy-explorer.el --- Explorer
;;; Commentary:
;;; A simple implementation of sidebar explorer

;;; Code:
(defconst wy-explorer-buffer-name " *WYExplorer*"
  "Name of wy-explorer buffer")
(defvar wy-explorer-buffer nil)
(defvar wy-explorer-buffers-not-in-any-project nil)
(defvar wy-explorer-working-buffer nil)
(defvar wy-explorer-buffer-hash (make-hash-table :test 'eq))


(defvar wy-explorer-lines ())
(defvar wy-explorer-current-line nil)


;; Interactive Functions
(defun wy-explorer-switch-window ()
  "Switch WYExplorer window."
  (interactive)
  (if (equal wy-explorer-buffer-name (buffer-name (current-buffer)))
      (other-window -1)
    (progn
      (setq window (get-buffer-window wy-explorer-buffer))
      (when window
        (winum-select-window-1)
        )
      )
    )
  )

(defun wy-explorer-toggle ()
  "Toggle WYExplorer."
  (interactive)

  (setq window (get-buffer-window wy-explorer-buffer))
  (if (and wy-explorer-buffer window)
      (delete-window window)
    (progn
      (setq wy-explorer-working-buffer (buffer-name (current-buffer)))
      (wy-explorer--buffer--get-or-create)
      (display-buffer-in-side-window wy-explorer-buffer `((side . left)))
      )
    )
  )

;; Private Functions
(defun wy-explorer--buffer--get-or-create ()
  "Get or create to wy-explorer buffer, and switch to it."
  (setq wy-explorer-buffer (get-buffer wy-explorer-buffer-name))
  (when (null wy-explorer-buffer)
    (setq wy-explorer-buffer (generate-new-buffer wy-explorer-buffer-name))
    )

  (with-current-buffer wy-explorer-buffer
    (wy-explorer--buffer--init)
    )
  )

(defun wy-explorer--buffer--init ()
  (when buffer-read-only (toggle-read-only))

  (erase-buffer)

  (setq wy-explorer-lines ())
  "Initialize global buffer for wy-explorer"
  (setq projects projectile-known-projects)
  (setq wy-explorer-buffers-not-in-any-project
        (sort
         (remove-if
          (lambda (b)
            (and
             (string-match-p "*" b)
             (not (string-match-p "^*eshell" b))
             )
            )
          (mapcar 'buffer-name (buffer-list))
          )
         'string<)
        )

  (dolist (b (mapcar 'buffer-name (buffer-list)))
    (puthash b t wy-explorer-buffer-hash)
    )

  (mapcar 'wy-explorer--buffer--display-project projects)

  ;;Insert buffers not in any project
  (when (> (length wy-explorer-buffers-not-in-any-project) 0)
    (push (list (line-number-at-pos) "p" nil) wy-explorer-lines)
    (insert "Buffers not in any project")
    (newline)
    (beginning-of-line)
    (dolist (b wy-explorer-buffers-not-in-any-project)
      ;;(unless (string-match-p "*$" b)
      (push (list (line-number-at-pos) "b" b nil) wy-explorer-lines)
      (when (equal b wy-explorer-working-buffer)
        (setq wy-explorer-current-line (line-number-at-pos)))
      (insert "  ")
      (insert b)
      (newline)
      (beginning-of-line)
      ;;)
      )
    )

  (goto-line wy-explorer-current-line)
  (read-only-mode)

  (wy-explorer-mode)
  )

(defun wy-explorer--buffer--display-project(p)
  "Display project name and it's open buffers in WYExplorer"

  (setq project-root (projectile-project-root p))
  (when (and project-root
             (not (string-equal project-root (expand-file-name "~/")))
             )

    ;;(setq buffers (sort (projectile-project-buffers project-root)) 'string<)
    (setq buffers
          (remove-if
           (lambda (b)
             (and
              (string-match-p "*$" b)
              (not (string-match-p "^*eshell" b))
              )
             )
           (mapcar 'buffer-name (projectile-project-buffers project-root))
           )
          )

    (when (> (length buffers) 0)
      ;;Insert project name
      (push (list (line-number-at-pos) "p" project-root) wy-explorer-lines)
      (setq host (nth 1 (split-string project-root ":")))
      (when host
        (insert "[")
        (insert host)
        (insert "]")
        )
      (insert (projectile-project-name project-root))
      (newline)
      (beginning-of-line)

      (dolist (b buffers)
        ;;(when (not (string-match-p "^*" b))
          (push (list (line-number-at-pos) "b" b project-root) wy-explorer-lines)
          (when (equal b wy-explorer-working-buffer)
            (setq wy-explorer-current-line (line-number-at-pos)))
          (insert "  ")
          (insert b)
          (newline)
          (beginning-of-line)
          (setq wy-explorer-buffers-not-in-any-project
                (delete b wy-explorer-buffers-not-in-any-project))
          ;;)
        )
      )
    )
  )

(defun wy-explorer-open-file-or-dir ()
  "Open a file or a director"
  (interactive)
  (setq line (nth (- (length wy-explorer-lines) (line-number-at-pos)) wy-explorer-lines))
  (if (equal (nth 1 line) "b")
      (progn
        (other-window -1)
        ;;(projectile-switch-project-by-name (nth 3 line))
        ;;(projectile-ensure-project (nth 3 line))
        ;;(switch-to-buffer (nth 2 line))
        (find-file (buffer-file-name (get-buffer (nth 2 line))))
        )
    (progn
      )
    )
  )

;;Hooks
(defun wy-explorer-buffer-list-update-hook ()
  (setq b (buffer-name(current-buffer)))
  (unless (gethash b wy-explorer-buffer-hash nil)
    (puthash b t wy-explorer-buffer-hash)
    (unless (string-match-p "*$" b)
      (wy-explorer--buffer--get-or-create)
      )
    )
  )
;;(add-hook 'buffer-list-update-hook 'wy-explorer-buffer-list-update-hook)

(defvar wy-explorer-mode-map nil)
(setq wy-explorer-mode-map (make-sparse-keymap))
(define-key wy-explorer-mode-map (kbd "RET")  'wy-explorer-open-file-or-dir)

(define-derived-mode wy-explorer-mode special-mode "wy-explorer"
  "A major mode for displaying the buffers grouped by projects."
  (setq cursor-in-non-selected-windows nil)
  (hl-line-mode +1)
  )


(global-set-key (kbd "C-\\") 'wy-explorer-switch-window)
(global-set-key (kbd "C-]") 'wy-explorer-toggle)

;; (with-eval-after-load 'projectile
;;   (require 'f)
;;   (defun wy-explorer-ignore-project (project-root)
;;     (print project-root)
;;     (print (expand-file-name "~/"))
;;     (string-match-p (expand-file-name "~/") project-root)
;;     )
;;   (setq projectile-ignored-project-function #'wy-explorer-ignore-project)
;;   )

;;(global-set-key (kbd "C-]") 'wy-explorer-toggle)

(provide 'wy-explorer)
;;; wy-explorer.el ends here
