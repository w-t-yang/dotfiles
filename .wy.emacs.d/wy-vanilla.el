;;; wy-vanilla.el --- WY's emacs config
;;; Commentary:
;;; Configs based on Purcell's emacs.d

;;; Code:

(setq mac-option-modifier 'meta)

(require 'init-elpa "~/.emacs.d/lisp/init-elpa.el")

;;(load-file "~/.wy.emacs.d/wy-explorer.el")

;;; Themes
(load-theme 'sanityinc-tomorrow-day)
;;(load-theme 'sanityinc-tomorrow-night)

;;(require-package 'zenburn-theme)
;;(load-theme 'zenburn)

;;(require-package 'material-theme)
;;(load-theme 'material)
;;(load-theme 'material-light)

;; doom-themes are not compatible with line number coloum background
;;(require-package 'doom-themes)

(set-face-attribute 'default nil :family "Source Code Pro" :height 160)
(setq-default fill-column 80)
(global-display-fill-column-indicator-mode)

;; (require-package 'vscode-dark-plus-theme)
;; (load-theme 'vscode-dark-plus)
;; (require-package 'solaire-mode)
;; (solaire-mode +1)

;; (require-package 'nyan-mode)
;; (nyan-mode t)

(require-package 'mini-frame)
(custom-set-variables
 '(mini-frame-advice-functions
   `(read-from-minibuffer
     read-char-from-minibuffer
     read-string
     yes-or-no-p )))
(custom-set-variables
 '(mini-frame-show-parameters
   '((top . 0.05)
     (width . 0.7)
     ;; (height . 0.2)
     (left . 0.5)
     (alpha . 0.9)
     ;; https://github.com/minad/vertico/issues/115
     (no-accept-focus . t))))
(custom-set-variables
 '(mini-frame-resize 'resize)) ;; resize or grow-only
(mini-frame-mode t)

(winner-mode t)

;; (setq-default left-margin-width 0 right-margin-width 0)
;; (set-window-buffer nil (current-buffer))

;;; Search
;; For fuzzy search, manually uncomment and reload the file
;; (setq ivy-re-builders-alist
;;       '((t . ivy--regex-fuzzy)))
;; (setq ivy-re-builders-alist
;;       '((t . ivy--regex-plus)))

;; After omitting ./ and ../, use 'C-j' when trying to open a directory
;; (setq ivy-extra-directories ())
;; (setq ivy-extra-directories '("../" "./"))



;;; Remote
;; (setq tramp-verbose 6)
;; (defvar ivy-rich-parse-remote-buffer nil)
;; (require 'tramp)
;; (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
;; (setq tramp-default-method "ssh")
;; (setq tramp-completion-reread-directory-timeout nil)
;; (setq tramp-auto-save-directory "~/tmp/tramp/")
;; (setq tramp-chunksize 2000)
;; (setq remote-file-name-inhibit-cache nil)


;;; Treemacs
(require-package 'treemacs)
(require-package 'treemacs-persp)
(require-package 'treemacs-projectile)
(defvar treemacs-width 36)
(defvar treemacs-indentation 1)
(defvar treemacs-no-png-images t)
(defvar treemacs-user-mode-line-format
  '(:eval (format " Workspace: %s"
                  (treemacs-workspace->name (treemacs-current-workspace)))))
(global-set-key (kbd "C-]") 'treemacs)

;;; Dictionary
(require-package 'osx-dictionary)
(global-set-key (kbd "M-s d") 'osx-dictionary-search-word-at-point)

;;; ESS
(require-package 'ess)

;;; Kubernetes
(require-package 'kubernetes)

;;; Docker
(require-package 'docker)

;;; Httpd
(require-package 'simple-httpd)

;;; Other packages
;;(require-package 'origami) ;; Code block folding, already in PurcellEmacs

;;; Performance
(setq desktop-restore-eager 1)

;;; Program language settings
(load-file "~/.wy.emacs.d/wy-language-settings.el")

;;; Key Bindings
(load-file "~/.wy.emacs.d/wy-key-bindings.el")

;;; Org-mode settings
(load-file "~/.wy.emacs.d/wy-org-settings.el")

;;; Custom functions
(load-file "~/.wy.emacs.d/wy-functions.el")

(provide 'wy-vanilla)
;;; wy-vanilla.el ends here
