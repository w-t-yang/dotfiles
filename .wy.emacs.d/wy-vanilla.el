;;; wy-vanilla.el --- WY's emacs config
;;; Commentary:
;;; Configs based on Purcell's emacs.d

;;; Code:

(setq mac-option-modifier 'meta)

(require 'init-elpa "~/.emacs.d/lisp/init-elpa.el")

;;(load-file "~/.wy.emacs.d/wy-explorer.el")
;;(load-file "~/.wy.emacs.d/wy-functions.el")
;;(load-file "~/.wy.emacs.d/wy-org-settings.el")

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
(setq fill-column 80)
(global-display-fill-column-indicator-mode)

(require-package 'nyan-mode)
(nyan-mode t)

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
(defvar treemacs-width 48)
(defvar treemacs-indentation 1)
(defvar treemacs-no-png-images t)
(global-set-key (kbd "C-]") 'treemacs)

;;; Dictionary
(require-package 'osx-dictionary)
(global-set-key (kbd "M-s d") 'osx-dictionary-search-pointer)

;;; ESS
(require-package 'ess)

;;; Performance
(setq desktop-restore-eager 1)

;;; Program language settings
(load-file "~/.wy.emacs.d/wy-language-settings.el")

;;; Key Bindings
(load-file "~/.wy.emacs.d/wy-key-bindings.el")

(provide 'wy-vanilla)
;;; wy-vanilla.el ends here
