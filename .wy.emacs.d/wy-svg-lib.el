;;; wy-svg-lib.el --- Svg-Lib
;;; Commentary:
;;; Configurations for svg-lib
;;; https://github.com/rougier/svg-lib
;;; Code:

(require 'init-elpa "~/.emacs.d/lisp/init-elpa.el")

(require-package 'svg-lib)

(defvar svg-font-lock-keywords
  `(("TODO"
     (0 (list 'face nil 'display (svg-font-lock-todo))))
    ("\\:\\([0-9a-zA-Z]+\\)\\:"
     (0 (list 'face nil 'display (svg-font-lock-tag (match-string 1)))))
    ("DONE"
     (0 (list 'face nil 'display (svg-font-lock-done))))
    ("\\[\\([0-9]\\{1,3\\}\\)%\\]"
     (0 (list 'face nil 'display (svg-font-lock-progress_percent (match-string 1)))))
    ("\\[\\([0-9]+/[0-9]+\\)\\]"
     (0 (list 'face nil 'display (svg-font-lock-progress_count (match-string 1)))))))

(defun svg-font-lock-tag (label)
  "Font lock for tag LABEL."
  (svg-lib-tag label nil :margin 0))

(defun svg-font-lock-todo ()
  (svg-lib-tag "TODO" nil :margin 0
               :font-family "Roboto Mono" :font-weight 500
               :foreground "#FFFFFF" :background "#673AB7"))

(defun svg-font-lock-done ()
  (svg-lib-tag "DONE" nil :margin 0
               :font-family "Roboto Mono" :font-weight 400
               :foreground "#B0BEC5" :background "white"))

(defun svg-font-lock-progress_percent (value)
  (svg-image (svg-lib-concat
              (svg-lib-progress-bar (/ (string-to-number value) 100.0)
                                    nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 12)
              (svg-lib-tag (concat value "%")
                           nil :stroke 0 :margin 0)) :ascent 'center))

(defun svg-font-lock-progress_count (value)
  (let* ((seq (mapcar #'string-to-number (split-string value "/")))
         (count (float (car seq)))
         (total (float (cadr seq))))
    (svg-image (svg-lib-concat
                (svg-lib-progress-bar (/ count total) nil
                                      :margin 0 :stroke 2 :radius 3 :padding 2 :width 12)
                (svg-lib-tag value nil
                             :stroke 0 :margin 0)) :ascent 'center)))

(defun wy-setup-svg-lib ()
  "Set up svg-lib in buffer."
  (interactive)
  ;;(push 'display font-lock-extra-managed-props)
  (font-lock-add-keywords nil svg-font-lock-keywords)
  ;;(font-lock-flush (point-min) (point-max))
  )

;;(add-hook 'after-change-major-mode-hook 'wy-setup-svg-lib)

(provide 'wy-svg-lib)
;;; wy-svg-lib.el ends here
