;;; wy-org-settings.el --- WY's org-mode
;;; Commentary:
;;; Configs for org-mode

;;; Code:

(require 'init-elpa "~/.emacs.d/lisp/init-elpa.el")

(require 'org)
(require-package 'org-bullets)
(add-hook 'org-mode-hook 'org-bullets-mode)
(add-hook 'org-mode-hook 'org-indent-mode)
(setq-default org-bullets-bullet-list '("❄" "❄" "❄" "❄" "☢")
              org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+"))
              org-list-allow-alphabetical t
              )

(defvar org-project-root "~/Projects/orgfiles")
(defvar org-todos-file (format "%s/%s" org-project-root "todos"))
(defvar org-notes-file (format "%s/%s" org-project-root "notes"))
(defvar org-work-file (format "%s/%s" org-project-root "work"))

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

(setq-default org-capture-templates
              `(
                ("t" "todo" entry
                 (file+headline ,org-todos-file "Upcomming Tasks")
                 "
* TODO %? :UNCATTED:
  %U
  Link: %a"
                 :clock-resume t)

                ("e" "emacs todo" entry
                 (file+headline ,org-todos-file)
                 "
* TODO %? :EMACS:
  %U
  Link: %a"
                 :clock-resume t)

                ("w" "work todo" entry
                 (file+olp+datetree ,org-work-file)
                 "
* TODO %? :WORK:
  %U"
                 :clock-resume t)

                ("n" "note" entry
                 (file+olp+datetree ,org-notes-file)
                 "
* %? :NOTE:
  %U
  Link: %a"
                 :clock-resume t)
                )
              )
(require-package 'noflet)

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
(global-set-key (kbd "C-*") 'make-capture-frame)

;; org-reveal for presentation in Emacs
(require-package 'ox-reveal)

(provide 'wy-org-settings)
;;; wy-org-settings.el ends here
