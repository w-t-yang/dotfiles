;;; wy-language-settings.el --- Language server settings
;;; Commentary:
;;; Code:

(require 'init-elpa "~/.emacs.d/lisp/init-elpa.el")

;;; Ruby
;; Solargraph [https://github.com/castwide/solargraph]
;; Add server to Gemfile
;;   gem 'solargraph', group: :development
;; Start the language server with local dev server

;;; Javascript & Typescript & tsx
;; Typescript language server
;; [https://github.com/typescript-language-server/typescript-language-server]
;; Install
;;   npm install -g typescript-language-server typescript
;; Run
;;   typescript-language-server --stdio

(require 'typescript-mode)
(add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode))
(setq typescript-indent-level 2)

(require-package 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx$" . web-mode))
(defun web-mode-init-hook ()
  "Hooks for Web mode.  Adjust indent."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  )
(add-hook 'web-mode-hook  'web-mode-init-hook)


;;; Swift
(require-package 'swift-mode)

(defun xcode-build()
  (interactive)
  (shell-command-to-string
   "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'build targetProject' -e 'end tell'"))

(defun xcode-run()
  (interactive)
  (shell-command-to-string
   "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'stop targetProject' -e 'run targetProject' -e 'end tell'"))

(defun xcode-test()
  (interactive)
  (shell-command-to-string
   "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'stop targetProject' -e 'test targetProject' -e 'end tell'"))

(defun xcode-open-current-file()
  (interactive)
  (shell-command-to-string
   (concat "open -a \"/Applications/Xcode.app\" " (buffer-file-name))))

(defun swift-print-var-at-point()
  (interactive)
  (if (string-match-p (string (preceding-char)) "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_")
      (backward-sexp)
    nil)
  (kill-sexp)
  (yank)
  (move-end-of-line nil)
  (newline)
  (insert "print(\"")
  (yank)
  (insert ": \\(")
  (yank)
  (insert ")\")")
  (indent-for-tab-command))

(global-set-key (kbd "C-c x b") 'xcode-build)
(global-set-key (kbd "C-c x r") 'xcode-run)
(global-set-key (kbd "C-c x t") 'xcode-test)
(global-set-key (kbd "C-c x o") 'xcode-open-current-file)
(global-set-key (kbd "C-c x p") 'swift-print-var-at-point)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(swift-mode . ("xcrun" "sourcekit-lsp"))))

;;(add-hook 'swift-mode-hook 'eglot-ensure)

;;; wy-language-settings.el ends here
