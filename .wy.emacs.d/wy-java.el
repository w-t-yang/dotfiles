;;; wy-java.el --- Settings for Java development
;;; Commentary:
;;; Code:

(defun wy/java/mark-imported-class ()
  "Mark the imported java class name with its package name."
  (interactive)
  (move-beginning-of-line nil)
  (forward-word)
  (forward-char)
  (set-mark (point))
  (move-end-of-line nil)
  (backward-char)
  )

(global-set-key (kbd "M-m j i") 'wy/java/mark-imported-class)

(defun wy/java/current-class ()
  "Return the java class name of the current file."
  (interactive)
  (string-replace ".java" "" (file-relative-name (buffer-file-name)))
  )

(defvar wy/java/single-test-name "")

(defun wy/java/set-single-test-name ()
  "Mark the first java test name before cursor."
  (interactive)
  (let ((pos (point)))
    (search-backward "@Test")
    (search-forward "{")
    (move-beginning-of-line nil)
    (forward-word)
    (forward-char)
    (set-mark (point))
    (forward-word nil)
    (setq wy/java/single-test-name (buffer-substring (mark) (point)))
    (deactivate-mark)
    (goto-char pos)
    )
  )

(defun wy/java/run-single-test ()
  "Run the marked java test."
  (interactive)
  (wy/java/set-single-test-name)
  (let ((marked-test
         (concat
          (wy/java/current-class)
          "#"
          wy/java/single-test-name)
         ))
    (wy-split-2-windows)
    (next-window-any-frame)
    (delete-other-windows)
    (end-of-buffer)
    (insert (concat "mvn test -Dtest=" marked-test)))
  )

(defun flymake-checkstyle-init (report-fn &rest _args)
  "Initialize flymake for Checkstyle.
REPORT-FN is the callback function to report diagnostics."
  (let* ((checkstyle-jar "~/checkstyle/checkstyle.jar")
         (checkstyle-config "~/checkstyle/checkstyle.xml")
         (source (buffer-file-name))
         (command (list "java" "-jar" checkstyle-jar "-c" checkstyle-config)))
    (list :command command
          :error-patterns
          '(("^[ERROR] \\(.*\\):\\([0-9]+\\):[0-9]*[:]* \\(.*\\)$" 1 2 nil 3))
          :source source
          :report-fn report-fn)))

(defun flymake-checkstyle-load ()
  "Configure flymake mode to check the current buffer's Java syntax."
  (interactive)
  (add-hook 'flymake-diagnostic-functions #'flymake-checkstyle-init nil t)
  (flymake-mode t))

(setq flymake-proc-allowed-file-name-masks nil)
(setq flymake-diagnostic-functions (delq 'flymake-proc-legacy-flymake flymake-diagnostic-functions))
(add-hook 'java-mode-hook 'flymake-checkstyle-load)
;; (setq flymake-log-level 3)

;;; wy-java.el ends here
