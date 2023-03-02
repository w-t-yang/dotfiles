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
 org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+"))
 org-list-allow-alphabetical t
 )

(defvar org-project-root "~/Projects/orgfiles")
(defvar org-capture-file (format "%s/%s.org" org-project-root "captured"))

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

(defvar wy-org-capture-categories
  `(("t" "todo")
    ("w" "work")
    ("e" "emacs")
    ("n" "note")
    ("i" "idea"))
  )

(defvar wy-generated-org-capture-templates ())

(defun wy-generate-org-capture-templates ()
  "Generate org templates."
  (setq wy-generated-org-capture-templates ())
  (dolist (item wy-org-capture-categories)
    (let ((key (car item))
          (tag (car (cdr item)))
          )
      (push `(,key ,tag entry (file+olp+datetree ,org-capture-file)
                   ,(concat "\n* %? :" tag ":\n%U\nLink: %a")
                   :clock-resume t)
            wy-generated-org-capture-templates)
      )
    )
  )
(wy-generate-org-capture-templates)

(setq-default org-capture-templates wy-generated-org-capture-templates)

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

(provide 'wy-org-settings)
;;; wy-org-settings.el ends here
