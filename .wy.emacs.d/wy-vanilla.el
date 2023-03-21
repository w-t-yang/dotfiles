;;; wy-vanilla.el --- WY's emacs config
;;; Commentary:
;;; Configs based on Purcell's emacs.d

;;; Code:

(setq mac-option-modifier 'meta)

(require 'init-elpa "~/.emacs.d/lisp/init-elpa.el")

;;(load-file "~/.wy.emacs.d/wy-explorer.el")

;;; Themes
(require 'init-themes "~/.emacs.d/lisp/init-themes.el")

;; doom-themes & material-theme are not compatible with line number coloum background
;; (require-package 'doom-themes)
;; (require-package 'material-theme)
;; (require-package 'zenburn-theme)

(defvar wy-custom-theme)
;; (setq wy-custom-theme 'doom-dracula)
;; (setq wy-custom-theme 'doom-one)
;; (setq wy-custom-theme 'doom-xcode)
;; (setq wy-custom-theme 'material)
;; (setq wy-custom-theme 'material-light)
(setq wy-custom-theme 'sanityinc-solarized-light)
;; (setq wy-custom-theme 'sanityinc-tomorrow-bright)
;; (setq wy-custom-theme 'sanityinc-tomorrow-day)
;; (setq wy-custom-theme 'sanityinc-tomorrow-eighties)
;; (setq wy-custom-theme 'sanityinc-tomorrow-night)
;; (setq wy-custom-theme 'zenburn)

;; Instead of calling 'load-theme', it's better to set custom-enabled-themes
;; and use 'reapply-themes' from 'init-themes' to forcibly load themes
;; This help to keep a clean list of enabled themes
(setq custom-enabled-themes `(,wy-custom-theme))
(reapply-themes)

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

;;; Control mode - https://github.com/stephendavidmarsh/control-mode
(require-package 'control-mode)
(control-mode-localized-setup)

;;; Mode line
(setcdr (assq 'vc-mode mode-line-format)
        '((:eval (replace-regexp-in-string "^ Git" (projectile-project-name) vc-mode))))

;; Original value of 'flymake-mode-line-format'
;; (" " flymake-mode-line-title flymake-mode-line-exception flymake-mode-line-counters)
(setq-default flymake-mode-line-format '("" "" flymake-mode-line-exception flymake-mode-line-counters))

;; Whitelist minor modes
;; When not sure what values to set in the list
;; print minor-mode-alist to get a full list of minor modes
;; (print minor-mode-alist)
(defun wy-purge-minor-modes ()
  "Purge minor modes."
  (setq minor-mode-alist '(
                           (flymake-mode flymake-mode-line-format)
                           (control-mode " <C>")
                           ))
  )
(add-hook 'after-change-major-mode-hook 'wy-purge-minor-modes)

;; Copy & past from 'binding.el'
;; Removed "(" and ")"
(setq mode-line-modes
      (let ((recursive-edit-help-echo
             "Recursive edit, type M-C-c to get out"))
        (list (propertize "%[" 'help-echo recursive-edit-help-echo)
              `(:propertize ("" mode-name)
                            help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
                            mouse-face mode-line-highlight
                            local-map ,mode-line-major-mode-keymap)
              '("" mode-line-process)
              `(:propertize ("" minor-mode-alist)
                            mouse-face mode-line-highlight
                            help-echo "Minor mode\n\
mouse-1: Display minor mode menu\n\
mouse-2: Show help for minor mode\n\
mouse-3: Toggle minor modes"
                            local-map ,mode-line-minor-mode-keymap)
              (propertize "%n" 'help-echo "mouse-2: Remove narrowing from buffer"
                          'mouse-face 'mode-line-highlight
                          'local-map (make-mode-line-mouse-map
                                      'mouse-2 #'mode-line-widen))
              (propertize "%]" 'help-echo recursive-edit-help-echo)
              " "))
      )

;; (require-package 'delight)
;; (delight '(
;;            (elisp-slime-nav-mode nil elisp-slime-nav)
;;            (paredit-mode nil paredit)
;;            (aggressive-indent-mode nil aggressive-indent)
;;            (projectile-mode nil projectile)
;;            (buffer-face-mode nil buffer-face)
;;            (undo-tree-mode nil undo-tree)
;;            (eldoc-mode nil eldoc)
;;            ;; To re-enable minor mode
;;            ;; (eldoc-mode " Eldoc" eldoc)
;;            (robe-mode nil robe)
;;            (projectile-rails-mode nil projectile-rails)
;;            ))

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

;;; Restclient
(require-package 'restclient)
(add-to-list 'auto-mode-alist '("\\.http$" . restclient-mode))

;;; Undo Tree
(require-package 'undo-tree)
(global-undo-tree-mode)
(setq-default undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

;;; Smooth scroll
(require-package 'smooth-scrolling)
(smooth-scrolling-mode 1)
;; (scroll-lock-mode 1)
;; (require-package 'smooth-scroll)

;;; Eshell
;; alias ~/.emacs.d/eshell/alias
(require-package 'esh-autosuggest)
(add-hook 'eshell-mode-hook #'esh-autosuggest-mode)

;;; Other packages
(require-package 'origami) ;; Code block folding, already in PurcellEmacs
(global-origami-mode 1)

;;; Performance
(setq-default desktop-restore-eager 1)

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
