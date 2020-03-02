;;; init.el --- Personal init file
;;; Commentary: just for the sake of flycheck --- no idea why it expects this
;;; Code: mhh
(defun wy-split-window-below ()
  "Splite window below."
  (interactive)
  (split-window-below)
  (evil-window-increase-height 24)
  )

(defun wy-increase-window-height ()
  "Increase window height."
  (interactive)
  (evil-window-increase-height 10)
  )

(defun wy-decrease-window-height ()
  "Decrease window height."
  (interactive)
  (evil-window-increase-height -10)
  )

(defun wy-increase-window-width ()
  "Increase window width."
  (interactive)
  (evil-window-increase-width 5)
  )

(defun wy-decrease-window-width ()
  "Decrease window width."
  (interactive)
  (evil-window-increase-width -5)
  )

(defun wy-open-eshell ()
  "Open a new eshell, and add to current layout."
  (interactive)
  (eshell 'N)
  )

(defun wy-clear-eshell-buffer ()
  "Clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    )
  )

(defun wy-open-remote-root ( server )
  "Open the root folder on a remote SERVER."
  (interactive "sEnter the server name: ")
  (find-file (format "/ssh:%s:/usr/local/git_tree/main" server))
  )


;; -- Bindings --
(load-file "~/.wy.emacs.d/emacs-bindings.el")
(global-set-key (kbd "<f13>") 'toggle-frame-fullscreen)
(global-set-key (kbd "M-<up>") 'wy-increase-window-height)
(global-set-key (kbd "M-<down>") 'wy-decrease-window-height)
(global-set-key (kbd "M-<right>") 'wy-increase-window-width)
(global-set-key (kbd "M-<left>") 'wy-decrease-window-width)
(global-set-key (kbd "C-]") 'neotree-toggle)
(global-set-key (kbd "C-.") 'neotree-find)
;;(global-set-key (kbd "C-<tab>") 'centaur-tabs-forward-tab)
;;(global-set-key (kbd "C-S-<tab>") 'centaur-tabs-backward-tab)
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "C-\\") 'wy-open-remote-root)
;;(global-set-key (kbd "C-]") 'treemacs)
;;(global-set-key (kbd "C-<escape>") 'eshell)
;;(global-set-key (kbd "C-j TAB") 'helm-projectile-switch-to-buffer)


;; -- Hooks --
;;(add-hook 'java-mode-hook (lambda ()
;;                            (setq c-basic-offset 2
;;                                  ;tab-width 2
;;                                  )))
;;(add-hook 'after-init-hook #'neotree-show)
;;(add-hook 'kill-emacs-hook 'neotree-hide)
;;
;;(advice-add 'helm-ff-filter-candidate-one-by-one
;;            :around (lambda (fcn file)
;;                      (unless (string-match "\\(?:/\\|\\`\\)\\.\\{1,2\\}\\'" file)
;;                        (funcall fcn file))))

;; -- Variables --
;;(setq-default cursor-in-non-selected-windows nil)
(setq doom-theme 'doom-city-lights)
;;(setq doom-theme 'doom-material)
;;(setq doom-theme 'doom-moonlight)

;;(load-file "~/.wy.emacs.d/wy-explorer.el")
;;(global-set-key (kbd "C-]") 'wy-explorer-toggle)
;; Disable eshell-prompt-extras epe-git-p, if eshell gets extremely slow
