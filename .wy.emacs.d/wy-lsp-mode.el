;;; wy-lsp-mode.el --- lsp-mode configs
;;; Commentary:
;;; Configs for lsp-mode
;;; !!! Deprecated !!!
;;; !!! Fully migrated to eglot from lsp !!!

;;; Code:

;;(require 'init-elpa "~/.emacs.d/lisp/init-elpa.el")

;; lsp-mode
;; when lsp is not enable/available, use dumb-jump for "jump to definition"
(require-package 'dumb-jump)
(setq lsp-keymap-prefix "M-l")

(require-package 'lsp-mode)
;; (require-package 'lsp-ui)
;; (require-package 'company-lsp)
(require-package 'lsp-treemacs)
;; (require-package 'lsp-ivy)
;; (require-package 'dap-mode)
;; (require-package 'yasnippet)
;; (setq lsp-ui-doc-enable nil)
;; (require 'lsp-mode)

;; Swift
;;(require-package 'lsp-sourcekit)

;; Java
;; - Install via melpa
;; (require-package 'lsp-java)
;; - end of install via melpa

;; - Install manually
;;  (load-file "~/.emacs.d/elpa-26.3/lsp-java/lsp-java.el")
;;  (require 'lsp-java)
;; - end of install manually

;; (add-hook 'java-mode-hook #'lsp)
;; (add-hook 'java-mode-hook (lambda ()
;;                             (setq c-basic-offset 2
;;                                   tab-width 2
;;                                   )))

;; Ruby
(add-hook 'ruby-mode-hook #'lsp)

;; C
(require-package 'ccls)
(add-hook 'c-mode-hook #'lsp)

;; Javascript
(require-package 'web-mode)
;; auto-enable for .js/.jsx files
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
(defun web-mode-init-hook ()
  "Hooks for Web mode.  Adjust indent."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  )
(add-hook 'web-mode-hook  'web-mode-init-hook)
(setq js-indent-level 2)
(setq js2-basic-offset 2)

;; Typescript
(add-hook 'typescript-mode-hook #'lsp)
(setq typescript-indent-level 2)

(setq company-minimum-prefix-length 1 company-idle-delay 0.0) ;; default is 0.2
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq lsp-idle-delay 0.500)
(setq lsp-inhibit-message t)

(global-set-key (kbd "M-g d") 'lsp-find-definition)
(global-set-key (kbd "M-g i") 'lsp-find-implementation)


;;; wy-lsp-mode.el ends here
