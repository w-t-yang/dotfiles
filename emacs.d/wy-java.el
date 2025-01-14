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

;; (require-package 'google-c-style)
;; (autoload 'google-set-c-style "google-c-style")
;; (autoload 'google-make-newline-indent "google-c-style")
;; (add-hook 'java-mode-hook 'google-set-c-style)
;; (add-hook 'java-mode-hook 'google-make-newline-indent)


;; Set up checkstyle flymake backend
;; Reference: https://www.gnu.org/software/emacs/manual/html_node/flymake/An-annotated-example-backend.html
(defvar-local checkstyle--flymake-proc nil)
(defvar-local checkstyle-jar "~/checkstyle/checkstyle.jar")
(defvar-local checkstyle-xml "~/checkstyle/google_checks.xml")
(defvar-local checkstyle-regex "^\\(\\[.*\\]\\) \\(.*.java\\):\\([0-9]+\\):[0-9]*[:]* \\(.*\\)$")

;; (defun checkstyle-flymake (report-fn &rest _args)
;;   "Initialize flymake for Checkstyle.
;; REPORT-FN is the callback function to report diagnostics."
;;   (let* ((source (buffer-file-name))
;;          (command (list "java" "-jar" checkstyle-jar "-c" checkstyle-xml)))
;;     (list :command command
;;           :error-patterns
;;           ;; '(("^\\[WARN\\] \\(.*\\):\\([0-9]+\\):[0-9]*[:]* \\(.*\\)$" 1 2 nil 3))
;;           '(("^\\(.*.java\\):\\([0-9]+\\):[0-9]*[:]* \\(.*\\)$" 1 2 nil 3))
;;           :source source
;;           :report-fn report-fn)))

(defun flymake-checkstyle-backend (report-fn &rest _args)
  "Flymake backend for Checkstyle.

Calls REPORT-FN with a list of Flymake diagnostics for the current buffer."

  (unless (executable-find "java")
    (error "Cannot find java"))

  (when (process-live-p checkstyle--flymake-proc)
    (kill-process checkstyle--flymake-proc))

  (let* ((source (current-buffer))
         (filename (buffer-file-name source)))
    (save-restriction
      (widen)
      (setq checkstyle--flymake-proc
            (make-process
             :name "flymake-checkstyle" :noquery t :connection-type 'pipe
             :buffer (generate-new-buffer " *flymake-checkstyle*")
             :command (list "java" "-jar" checkstyle-jar "-c" checkstyle-xml (buffer-file-name))
             :sentinel
             (lambda (proc _event)
               (when (memq (process-status proc) '(exit signal))
                 (unwind-protect
                     (if (with-current-buffer (current-buffer) (eq proc checkstyle--flymake-proc))
                         (with-current-buffer (process-buffer proc)
                           (goto-char (point-min))
                           (cl-loop
                            while (search-forward-regexp
                                   checkstyle-regex
                                   nil t)
                            for msg = (match-string 4)
                            for (beg . end) = (flymake-diag-region
                                               (current-buffer)
                                               (string-to-number (match-string 3)))
                            for type = (if (string-match "WARN" (match-string 1))
                                           :warning
                                         :error)
                            when (and beg end)
                            collect (flymake-make-diagnostic source beg end type msg)
                            into diags
                            finally (funcall report-fn diags)))
                       (flymake-log :warning "Canceling obsolete check %s" proc))
                   ;;(kill-buffer (process-buffer proc))
                   )))))
      (process-send-region checkstyle--flymake-proc (point-min) (point-max))
      (process-send-eof checkstyle--flymake-proc))))

;; (defun flymake-checkstyle-load ()
;;   "Configure flymake mode to check the current buffer with checkstyle."
;;   (add-hook 'flymake-diagnostic-functions #'flymake-checkstyle-backend nil t))
;; (add-hook 'java-mode-hook 'flymake-checkstyle-load)

;; (require-package 'flycheck)
;; (flycheck-define-checker checkstyle-checker
;;   "Java checkstyle checker."
;;   :command ("java" "-jar" "~/checkstyle/checkstyle.jar" "-c" "~/checkstyle/google_checks.xml" "-f" "xml" source)
;;   :error-parser flycheck-parse-checkstyle
;;   :enable t
;;   :modes (java-mode))

;;; wy-java.el ends here
