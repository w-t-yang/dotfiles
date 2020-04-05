;;; wy-org-settings.el --- WY's org-mode settings
;;; Commentary:
;;; configuration based on Purcell's work

;;; Code:

(require 'init-elpa)
(require-package 'org-bullets)

(add-hook 'org-mode-hook 'org-bullets-mode)
(setq org-bullets-bullet-list '("❄" "❄" "❄" "❄" "☢"))
(setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+")))
(setq org-list-allow-alphabetical t)
(setq org-export-with-toc nil)

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

(setq org-capture-templates
      '(
        ("t" "todo" entry
         (file+headline "~/org/todo.org" "Upcomming Tasks")
         "
* TODO %?
  %U
  Link: %a"
         :clock-resume t)

        ("n" "note" entry
         (file+datetree "~/org/note.org")
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


(global-set-key (kbd "C-*") 'make-capture-frame)

(provide 'wy-org-settings)
;;; wy-org-settings.el ends here
