;;; wy-ai.el --- Integration with AI
;;; Commentary:
;;; Code:

(defvar wy-ai-buffer-name "*AI*")

(defvar command-prefix "cody chat -m")
(defvar prompt-til "Tell me about something interesting about programming for today")

;; Function created by Cody
(defun async-shell-command-to-buffer (command buffer)
  "Execute async shell COMMAND and send output to BUFFER.
If BUFFER doesn't exist, it will be created."
  (interactive)
  (let ((proc (start-process "async-shell-command" buffer
                             shell-file-name shell-command-switch
                             command)))
    (set-process-sentinel proc #'shell-command-sentinel)
    proc))

(defun wy-ai-buffer (command-prefix prompt)
  "Open AI buffer, and execute given COMMAND-PREFIX and PROMPT."
  (interactive)
  (setq wy-ai-buffer-name (format "*AI/%s*" (projectile-project-name)))
  (switch-to-buffer wy-ai-buffer-name)
  (goto-char (point-max))

  (insert "\n----------------------------------------------\n")
  (insert (format "  Date: %s" (shell-command-to-string "date")) )
  (insert (format "Prompt: %s\n\n" prompt))

  (insert "Generating response...\n")
  (async-shell-command-to-buffer
   (format "%s '%s'" command-prefix prompt)
   wy-ai-buffer-name)

  ;; (insert "\nEnd of response.\n")
  (markdown-mode)
  )

(defun wy-ai-til ()
  "Function that ask AI to teach something for today."
  (interactive)
  (wy-ai-buffer command-prefix prompt-til)
  )

(defun wy-ai-prompt ()
  "Ask for USER-INPUT to be used as AI prompt."
  (interactive)
  (wy-ai-buffer command-prefix (read-string "Prompt: "))
  )

(define-prefix-command 'ai-operations)
(global-set-key (kbd "M-[") 'ai-operations)
(global-set-key (kbd "M-[ p") 'wy-ai-prompt)

(global-set-key (kbd "M-]") 'wy-ai-prompt)
;;; wy-ai.el ends here
