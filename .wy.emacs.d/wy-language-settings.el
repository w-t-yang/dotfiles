;;; wy-language-settings.el --- Language server settings
;;; Commentary:
;;; Code:

(require 'init-elpa "~/.emacs.d/lisp/init-elpa.el")

;;; Ruby
;; Solargraph [https://github.com/castwide/solargraph] Add server to Gemfile gem
;;   gem 'solargraph', group: :development
;; Or install it with
;;   gem install 'solargraph'
;; [Optional] Other configurations
;;   solargraph download-core
;;   solargraph bundle
;;   solargraph config
;;   solargraph scan -v
(defun wy-run-rubocop ()
  "Run rubocop autofix in current project."
  (interactive)
  (message "Running Rubocup autofix...")
  (projectile-run-shell-command-in-root "rubocop -A")
  )
(global-set-key (kbd "C-c r r") 'wy-run-rubocop)

;;; Javascript & Typescript & tsx
;; Typescript language server
;; [https://github.com/typescript-language-server/typescript-language-server]
;; Install
;;   npm install -g typescript-language-server typescript
;; [Optional] Run language server manually
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

(defun wy-run-swiftlint ()
  "Run swiftlint autofix in current project."
  (interactive)
  (message "Running SwiftLint autofix...")
  (projectile-run-shell-command-in-root "swiftlint --fix")
  )
(global-set-key (kbd "C-c r s") 'wy-run-swiftlint)

;;(add-hook 'swift-mode-hook 'eglot-ensure)

;;; XML
(setq nxml-child-indent 4 nxml-attribute-indent 4)

;;; Java
(defvar lombok-path (substitute-in-file-name "$HOME/.m2/repository/org/projectlombok/lombok/1.18.30/lombok-1.18.30.jar"))
(defvar jvm-arg1 (format "--jvm-arg=-javaagent:%s" lombok-path))
(defvar jvm-arg2 (format "--jvm-arg=-Xbootclasspath/a:%s" lombok-path))
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `(java-mode . ("jdtls" ,jvm-arg1 ,jvm-arg2))
               ))

(defun on-java-loaded ()
  "Override key bindings in java mode."
  (define-key java-mode-map (kbd "M-q") 'kill-current-buffer))

(add-hook 'java-mode-hook 'on-java-loaded)


;;; Shell
(require-package 'flymake-shell)
(require-package 'flymake-shellcheck)

;;; wy-language-settings.el ends here
