;;; wy-org-settings.el --- WY's org-mode
;;; Commentary:
;;; Configs for org-mode

;;; Code:

(require 'init-elpa "~/.emacs.d/lisp/init-elpa.el")

(require 'org)
(require-package 'org-bullets)
(add-hook 'org-mode-hook 'org-bullets-mode)
(add-hook 'org-mode-hook 'org-indent-mode)
(setq-default
 ;;org-bullets-bullet-list '("❄" "❄" "❄" "❄" "☢")
 org-bullets-bullet-list '("⦿" "◉" "●" "○" "・")
 org-list-demote-modify-bullet '(("-" . "+") ("+" . "-") ("-" . "+"))
 org-list-allow-alphabetical t
 )

(defvar org-project-root "~/Projects/orgfiles")
(defvar org-capture-file (format "%s/%s.org" org-project-root "captured"))

(setq org-agenda-files
      (directory-files-recursively
       org-project-root
       "\\.org$"
       t
       (lambda (k)
         (not (string-match "\\(blog\\|archived\\)" k)))
       )
      )

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame."
  (if (equal "org capture" (frame-parameter nil 'name))
      (delete-frame)))

(defadvice org-capture-destroy
    (after delete-capture-frame activate)
  "Advise capture-destroy to close the frame."
  (if (equal "org capture" (frame-parameter nil 'name))
      (delete-frame)))

(defvar wy-org-capture-categories ())
(setq-default wy-org-capture-categories
              `(
                ("1" "WZServer" ,(format "%s/captured/%s.org" org-project-root "wenzhai-server"))
                ("2" "WZiOS" ,(format "%s/captured/%s.org" org-project-root "wenzhai-ios"))
                ("e" "emacs" ,org-capture-file)
                ("i" "idea" ,org-capture-file)
                ("n" "note" ,org-capture-file)
                ("t" "todo" ,org-capture-file)
                ("w" "work" ,org-capture-file)
                )
              )

(defvar wy-generated-org-capture-templates ())

(defun wy-generate-org-capture-templates ()
  "Generate org templates."
  (setq wy-generated-org-capture-templates ())
  (dolist (item wy-org-capture-categories)
    (let ((key (car item))
          (tag (car (cdr item)))
          (file (car (cdr (cdr item))))
          )
      (push `(,key ,tag entry (file+olp+datetree ,file)
                   ,(concat "\n* %? :" tag ":\n%U\nLink: %a")
                   :clock-resume t
                   :prepend t)
            wy-generated-org-capture-templates)
      )
    )
  )
(wy-generate-org-capture-templates)
(setq-default org-capture-templates (reverse wy-generated-org-capture-templates))

;; (require-package 'noflet)

(defun make-capture-frame ()
  "Create a new frame and run 'org-capture'."
  (interactive)
  (make-frame '((name . "org capture")
                (width . 80)
                (height . 16)
                (top . 400)
                (left . 300)))
  (select-frame-by-name "org capture")
  ;; (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
  ;;         (org-capture))
  (org-capture)
  (delete-other-windows)
  )

;; (global-set-key (kbd "M-c") 'org-capture)
;; (global-set-key (kbd "C-*") 'make-capture-frame)

;; org-reveal for presentation in Emacs
(require-package 'ox-reveal)

;; org-roam
;; (require-package 'org-roam)
;; (setq-default org-roam-directory (format "%s/%s" org-project-root "roam"))

(provide 'wy-org-settings)
;;; wy-org-settings.el ends here
