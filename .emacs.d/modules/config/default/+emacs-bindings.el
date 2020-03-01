;;; config/default/+emacs-bindings.el -*- lexical-binding: t; -*-

;; Sensible deafult key bindings for non-evil users
(setq doom-leader-alt-key "C-j"
      doom-localleader-alt-key "C-c l")

;; persp-mode and projectile in different prefixes
(setq persp-keymap-prefix (kbd "C-c w"))
(after! projectile
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))


;;
;;; Autoloads

(autoload 'org-capture-goto-target "org-capture" nil t)


;;
;;; Leader keys

(map! :leader
      :desc "Evaluate line/region"        "e"   #'+eval/line-or-region

      (:prefix ("l" . "<localleader>")) ; bound locally
      (:prefix ("!" . "checkers"))      ; bound by flycheck

      :desc "Comment line"       ";"    #'comment-line
      :desc "Eshell"                   ":"    #'eshell
      ;;:desc "help"                  "h"    help-map
      ;;:desc "Find file"             "."    #'find-file
      :desc "Switch buffer"         ","    #'projectile-switch-to-buffer
      ;;:desc "Switch to last buffer" "`"    #'evil-switch-to-windows-last-buffer
      ;; :desc "Resume last search"    "'"    (cond ((featurep! :completion ivy)   #'ivy-resume)
      ;;                                            ((featurep! :completion helm)  #'helm-resume))
      :desc "Search for symbol in project" "*" #'+default/search-project-for-symbol-at-point
      :desc "Find file in project"  "SPC"  #'projectile-find-file
      :desc "Jump to bookmark"      "RET"  #'bookmark-jump
      :desc "Pop up scratch buffer" "x"    #'doom/open-scratch-buffer
      :desc "Org Capture"           "X"    #'org-capture

      ;; (:when (featurep! :ui workspaces)
      ;;   :desc "Switch workspace buffer" "," #'persp-switch-to-buffer
      ;;   :desc "Switch all buffer"       "<" #'switch-to-buffer)

      (:when (featurep! :ui popup)
        :desc "Toggle last popup"     "`"    #'+popup/toggle)

      ;;; <leader> & --- snippets
      (:prefix-map ("&" . "snippets")
        :desc "New snippet"           "n" #'yas-new-snippet
        :desc "Insert snippet"        "i" #'yas-insert-snippet
        :desc "Find global snippet"   "/" #'yas-visit-snippet-file
        :desc "Reload snippets"       "r" #'yas-reload-all
        :desc "Create Temp Template"  "c" #'aya-create
        :desc "Use Temp Template"     "e" #'aya-expand)

      ;;; <leader> b --- buffer
      (:prefix-map ("b" . "buffer")
        :desc "Toggle narrowing"            "-"   #'doom/toggle-narrow-buffer
        :desc "Previous buffer"             "["   #'previous-buffer
        :desc "Next buffer"                 "]"   #'next-buffer
        (:when (featurep! :ui workspaces)
          :desc "Switch workspace buffer" "b" #'persp-switch-to-buffer
          :desc "Switch buffer"           "B" #'switch-to-buffer)
        (:unless (featurep! :ui workspaces)
          :desc "Switch buffer"           "b" #'switch-to-buffer)
        :desc "Kill buffer"                 "d"   #'kill-current-buffer
        :desc "ibuffer"                     "i"   #'ibuffer
        :desc "Kill buffer"                 "k"   #'kill-current-buffer
        :desc "Kill all buffers"            "K"   #'doom/kill-all-buffers
        :desc "Switch to last buffer"       "l"   #'evil-switch-to-windows-last-buffer
        :desc "Set bookmark"                "m"   #'bookmark-set
        :desc "Delete bookmark"             "M"   #'bookmark-delete
        :desc "Next buffer"                 "n"   #'next-buffer
        :desc "New empty buffer"            "N"   #'evil-buffer-new
        :desc "Kill other buffers"          "O"   #'doom/kill-other-buffers
        :desc "Previous buffer"             "p"   #'previous-buffer
        :desc "Revert buffer"               "r"   #'revert-buffer
        :desc "Save buffer"                 "s"   #'basic-save-buffer
        :desc "Save all buffers"            "S"   #'evil-write-all
        :desc "Save buffer as root"         "u"   #'doom/sudo-save-buffer
        :desc "Pop up scratch buffer"       "x"   #'doom/open-scratch-buffer
        :desc "Switch to scratch buffer"    "X"   #'doom/switch-to-scratch-buffer
        :desc "Bury buffer"                 "z"   #'bury-buffer
        :desc "Kill buried buffers"         "Z"   #'doom/kill-buried-buffers)

      ;;; <leader> c --- code
      (:prefix-map ("c" . "code")
        :desc "LSP Execute code action"               "a"   #'lsp-execute-code-action
        :desc "Compile"                               "c"   #'compile
        :desc "Recompile"                             "C"   #'recompile
        :desc "Jump to definition"                    "d"   #'+lookup/definition
        :desc "Jump to references"                    "D"   #'+lookup/references
        :desc "Evaluate buffer/region"                "e"   #'+eval/buffer-or-region
        :desc "Evaluate & replace region"             "E"   #'+eval:replace-region
        :desc "Format buffer/region"                  "f"   #'+format/region-or-buffer
        :desc "LSP Format buffer/region"              "F"   #'+default/lsp-format-region-or-buffer
        :desc "LSP Organize imports"                  "i"   #'lsp-organize-imports
        (:when (featurep! :completion ivy)
          :desc "Jump to symbol in current workspace" "j"   #'lsp-ivy-workspace-symbol
          :desc "Jump to symbol in any workspace"     "J"   #'lsp-ivy-global-workspace-symbol)
        (:when (featurep! :completion helm)
          :desc "Jump to symbol in current workspace" "j"   #'helm-lsp-workspace-symbol
          :desc "Jump to symbol in any workspace"     "J"   #'helm-lsp-global-workspace-symbol)
        :desc "Jump to documentation"                 "k"   #'+lookup/documentation
        :desc "LSP Rename"                            "r"   #'lsp-rename
        :desc "Send to repl"                          "s"   #'+eval/send-region-to-repl
        :desc "Delete trailing whitespace"            "w"   #'delete-trailing-whitespace
        :desc "Delete trailing newlines"              "W"   #'doom/delete-trailing-newlines
        :desc "List errors"                           "x"   #'flymake-show-diagnostics-buffer
        (:when (featurep! :checkers syntax)
          :desc "List errors"                         "x"   #'flycheck-list-errors))
      
      ;;; <leader> f --- file
      (:prefix-map ("f" . "file")
        :desc "Open project editorconfig"   "c"   #'editorconfig-find-current-editorconfig
        :desc "Copy this file"              "C"   #'doom/copy-this-file
        :desc "Find directory"              "d"   #'dired
        :desc "Delete this file"            "D"   #'doom/delete-this-file
        :desc "Find file in emacs.d"        "e"   #'+default/find-in-emacsd
        :desc "Browse emacs.d"              "E"   #'+default/browse-emacsd
        :desc "Find file"                   "f"   #'find-file
        :desc "Find file from here"         "F"   #'+default/find-file-under-here
        :desc "Locate file"                 "l"   #'locate
        :desc "Find file in private config" "p"   #'doom/find-file-in-private-config
        :desc "Browse private config"       "P"   #'doom/open-private-config
        :desc "Recent files"                "r"   #'recentf-open-files
        :desc "Rename/move file"            "R"   #'doom/move-this-file
        :desc "Save file"                   "s"   #'save-buffer
        :desc "Save file as..."             "S"   #'write-file
        :desc "Sudo find file"              "u"   #'doom/sudo-find-file
        :desc "Sudo this file"              "U"   #'doom/sudo-this-file
        :desc "Yank filename"               "y"   #'+default/yank-buffer-filename)

      ;;; <leader> r --- remote
      (:when (featurep! :tools upload)
        (:prefix-map ("r" . "remote")
          :desc "Upload local"               "u" #'ssh-deploy-upload-handler
          :desc "Upload local (force)"       "U" #'ssh-deploy-upload-handler-forced
          :desc "Download remote"            "d" #'ssh-deploy-download-handler
          :desc "Diff local & remote"        "D" #'ssh-deploy-diff-handler
          :desc "Browse remote files"        "." #'ssh-deploy-browse-remote-handler
          :desc "Detect remote changes"      ">" #'ssh-deploy-remote-changes-handler))

      ;;; <leader> g --- git
      (:prefix-map ("g" . "git")
        :desc "Git revert file"             "R"   #'vc-revert
        :desc "Copy link to remote"         "y"   #'+vc/browse-at-remote-kill-file-or-region
        :desc "Copy link to homepage"       "Y"   #'+vc/browse-at-remote-kill-homepage
        (:when (featurep! :ui hydra)
          :desc "SMerge"                    "m"   #'+vc/smerge-hydra/body)
        (:when (featurep! :ui vc-gutter)
          :desc "Git revert hunk"           "r"   #'git-gutter:revert-hunk
          :desc "Git stage hunk"            "s"   #'git-gutter:stage-hunk
          :desc "Git time machine"          "t"   #'git-timemachine-toggle
          :desc "Jump to next hunk"         "]"   #'git-gutter:next-hunk
          :desc "Jump to previous hunk"     "["   #'git-gutter:previous-hunk)
        (:when (featurep! :tools magit)
          :desc "Magit dispatch"            "/"   #'magit-dispatch
          :desc "Forge dispatch"            "'"   #'forge-dispatch
          :desc "Magit switch branch"       "b"   #'magit-branch-checkout
          :desc "Magit status"              "g"   #'magit-status
          :desc "Magit file delete"         "D"   #'magit-file-delete
          :desc "Magit blame"               "B"   #'magit-blame-addition
          :desc "Magit clone"               "C"   #'magit-clone
          :desc "Magit fetch"               "F"   #'magit-fetch
          :desc "Magit buffer log"          "L"   #'magit-log
          :desc "Git stage file"            "S"   #'magit-stage-file
          :desc "Git unstage file"          "U"   #'magit-unstage-file
          (:prefix ("f" . "find")
            :desc "Find file"                 "f"   #'magit-find-file
            :desc "Find gitconfig file"       "g"   #'magit-find-git-config-file
            :desc "Find commit"               "c"   #'magit-show-commit
            :desc "Find issue"                "i"   #'forge-visit-issue
            :desc "Find pull request"         "p"   #'forge-visit-pullreq)
          (:prefix ("o" . "open in browser")
            :desc "Browse file or region"     "o"   #'browse-at-remote
            :desc "Browse homepage"           "h"   #'+vc/browse-at-remote-homepage
            :desc "Browse remote"             "r"   #'forge-browse-remote
            :desc "Browse commit"             "c"   #'forge-browse-commit
            :desc "Browse an issue"           "i"   #'forge-browse-issue
            :desc "Browse a pull request"     "p"   #'forge-browse-pullreq
            :desc "Browse issues"             "I"   #'forge-browse-issues
            :desc "Browse pull requests"      "P"   #'forge-browse-pullreqs)
          (:prefix ("l" . "list")
            (:when (featurep! :tools gist)
              :desc "List gists"              "g"   #'+gist:list)
            :desc "List repositories"         "r"   #'magit-list-repositories
            :desc "List submodules"           "s"   #'magit-list-submodules
            :desc "List issues"               "i"   #'forge-list-issues
            :desc "List pull requests"        "p"   #'forge-list-pullreqs
            :desc "List notifications"        "n"   #'forge-list-notifications)
          (:prefix ("c" . "create")
            :desc "Initialize repo"           "r"   #'magit-init
            :desc "Clone repo"                "R"   #'magit-clone
            :desc "Commit"                    "c"   #'magit-commit-create
            :desc "Fixup"                     "f"   #'magit-commit-fixup
            :desc "Branch"                    "b"   #'magit-branch-and-checkout
            :desc "Issue"                     "i"   #'forge-create-issue
            :desc "Pull request"              "p"   #'forge-create-pullreq)))

      ;;; <leader> i --- insert
      (:prefix-map ("i" . "insert")
        :desc "Current file name"             "f"   #'+default/insert-file-path
        :desc "Current file path"             "F"   (λ!! #'+default/insert-file-path t)
        :desc "Evil ex path"                  "p"   (λ! (evil-ex "R!echo "))
        :desc "From evil register"            "r"   #'evil-ex-registers
        :desc "Snippet"                       "s"   #'yas-insert-snippet
        :desc "Unicode"                       "u"   #'unicode-chars-list-chars
        :desc "From clipboard"                "y"   #'+default/yank-pop)

      ;;; <leader> l --- lookup
      (:when (featurep! :tools lookup)
        (:prefix-map ("l" . "lookup")
          "k" #'+lookup/documentation
          "d" #'+lookup/definition
          "D" #'+lookup/references
          "f" #'+lookup/file
          "o" #'+lookup/online-select
          "i" #'+lookup/in-docsets
          "I" #'+lookup/in-all-docsets))

      ;;; <leader> m --- multiple cursors
      (:when (featurep! :editor multiple-cursors)
        (:prefix-map ("m" . "multiple cursors")
          :desc "Edit lines"         "l"         #'mc/edit-lines
          :desc "Mark next"          "n"         #'mc/mark-next-like-this
          :desc "Unmark next"        "N"         #'mc/unmark-next-like-this
          :desc "Mark previous"      "p"         #'mc/mark-previous-like-this
          :desc "Unmark previous"    "P"         #'mc/unmark-previous-like-this
          :desc "Mark all"           "t"         #'mc/mark-all-like-this
          :desc "Mark all DWIM"      "m"         #'mc/mark-all-like-this-dwim
          :desc "Edit line endings"  "e"         #'mc/edit-ends-of-lines
          :desc "Edit line starts"   "a"         #'mc/edit-beginnings-of-lines
          :desc "Mark tag"           "s"         #'mc/mark-sgml-tag-pair
          :desc "Mark in defun"      "d"         #'mc/mark-all-like-this-in-defun
          :desc "Add cursor w/mouse" "<mouse-1>" #'mc/add-cursor-on-click))

      ;;; <leader> n --- notes
      (:prefix-map ("n" . "notes")
        :desc "Search notes for symbol"      "*" #'+default/search-notes-for-symbol-at-point
        :desc "Org agenda"                   "a" #'org-agenda
        :desc "Toggle org-clock"             "c" #'+org/toggle-clock
        :desc "Cancel org-clock"             "C" #'org-clock-cancel
        :desc "Open deft"                    "d" #'deft
        :desc "Find file in notes"           "f" #'+default/find-in-notes
        :desc "Browse notes"                 "F" #'+default/browse-notes
        :desc "Org store link"               "l" #'org-store-link
        :desc "Tags search"                  "m" #'org-tags-view
        :desc "Org capture"                  "n" #'org-capture
        :desc "Active org-clock"             "o" #'org-clock-goto
        :desc "Todo list"                    "t" #'org-todo-list
        :desc "Search notes"                 "s" #'+default/org-notes-search
        :desc "Search org agenda headlines"  "S" #'+default/org-notes-headlines
        :desc "View search"                  "v" #'org-search-view
        :desc "Org export to clipboard"        "y" #'+org/export-to-clipboard
        :desc "Org export to clipboard as RTF" "Y" #'+org/export-to-clipboard-as-rich-text
        (:when (featurep! :lang org +journal)
          (:prefix ("j" . "journal")
            :desc "New Entry"      "j" #'org-journal-new-entry
            :desc "Search Forever" "s" #'org-journal-search-forever)))

      ;;; <leader> o --- open
      (:prefix-map ("o" . "open")
        :desc "Org agenda"       "A"  #'org-agenda
        (:prefix ("a" . "org agenda")
          :desc "Agenda"         "a"  #'org-agenda
          :desc "Todo list"      "t"  #'org-todo-list
          :desc "Tags search"    "m"  #'org-tags-view
          :desc "View search"    "v"  #'org-search-view)
        :desc "Default browser"    "b"  #'browse-url-of-file
        :desc "Start debugger"     "d"  #'+debugger/start
        :desc "New frame"          "f"  #'make-frame
        :desc "REPL"               "r"  #'+eval/open-repl-other-window
        :desc "REPL (same window)" "R"  #'+eval/open-repl-same-window
        :desc "Dired"              "-"  #'dired-jump
        (:when (featurep! :ui neotree)
          :desc "Project sidebar"              "p" #'+neotree/open
          :desc "Find file in project sidebar" "P" #'+neotree/find-this-file)
        (:when (featurep! :ui treemacs)
          :desc "Project sidebar" "p" #'+treemacs/toggle
          :desc "Find file in project sidebar" "P" #'+treemacs/find-file)
        (:when (featurep! :term shell)
          :desc "Toggle shell popup"    "t" #'+shell/toggle
          :desc "Open shell here"       "T" #'+shell/here)
        (:when (featurep! :term term)
          :desc "Toggle terminal popup" "t" #'+term/toggle
          :desc "Open terminal here"    "T" #'+term/here)
        (:when (featurep! :term vterm)
          :desc "Toggle vterm popup"    "t" #'+vterm/toggle
          :desc "Open vterm here"       "T" #'+vterm/here)
        (:when (featurep! :term eshell)
          :desc "Toggle eshell popup"   "e" #'+eshell/toggle
          :desc "Open eshell here"      "E" #'+eshell/here)
        (:when (featurep! :tools macos)
          :desc "Reveal in Finder"           "o" #'+macos/reveal-in-finder
          :desc "Reveal project in Finder"   "O" #'+macos/reveal-project-in-finder
          :desc "Send to Transmit"           "u" #'+macos/send-to-transmit
          :desc "Send project to Transmit"   "U" #'+macos/send-project-to-transmit
          :desc "Send to Launchbar"          "l" #'+macos/send-to-launchbar
          :desc "Send project to Launchbar"  "L" #'+macos/send-project-to-launchbar)
        (:when (featurep! :tools docker)
          :desc "Docker" "D" #'docker))

      ;;; <leader> p --- project
      (:prefix-map ("p" . "project")
        :desc "Browse project"               "." #'+default/browse-project
        :desc "Browse other project"         ">" #'doom/browse-in-other-project
        :desc "Run cmd in project root"      "!" #'projectile-run-shell-command-in-root
        :desc "Add new project"              "a" #'projectile-add-known-project
        :desc "Switch to project buffer"     "b" #'projectile-switch-to-buffer
        :desc "Compile in project"           "c" #'projectile-compile-project
        :desc "Repeat last command"          "C" #'projectile-repeat-last-command
        :desc "Remove known project"         "d" #'projectile-remove-known-project
        :desc "Discover projects in folder"  "D" #'+default/discover-projects
        :desc "Edit project .dir-locals"     "e" #'projectile-edit-dir-locals
        :desc "Find file in project"         "f" #'projectile-find-file
        :desc "Find file in other project"   "F" #'doom/find-file-in-other-project
        :desc "Configure project"            "g" #'projectile-configure-project
        :desc "Invalidate project cache"     "i" #'projectile-invalidate-cache
        :desc "Kill project buffers"         "k" #'projectile-kill-buffers
        :desc "Find other file"              "o" #'projectile-find-other-file
        :desc "Switch project"               "p" #'projectile-switch-project
        :desc "Find recent project files"    "r" #'projectile-recentf
        :desc "Run project"                  "R" #'projectile-run-project
        :desc "Save project files"           "s" #'projectile-save-project-buffers
        :desc "Pop up scratch buffer"        "x" #'doom/open-project-scratch-buffer
        :desc "Switch to scratch buffer"     "X" #'doom/switch-to-project-scratch-buffer
        :desc "List project tasks"           "t" #'magit-todos-list
        :desc "Test project"                 "T" #'projectile-test-project)

      ;;; <leader> q --- quit/restart
      (:prefix-map ("q" . "quit/restart")
        :desc "Restart emacs server"         "d" #'+default/restart-server
        :desc "Delete frame"                 "f" #'delete-frame
        :desc "Clear current frame"          "F" #'doom/kill-all-buffers
        :desc "Kill Emacs (and daemon)"      "K" #'save-buffers-kill-emacs
        :desc "Quit Emacs"                   "q" #'kill-emacs
        :desc "Save and quit Emacs"          "Q" #'save-buffers-kill-terminal
        :desc "Quick save current session"   "s" #'doom/quicksave-session
        :desc "Restore last session"         "l" #'doom/quickload-session
        :desc "Save session to file"         "S" #'doom/save-session
        :desc "Restore session from file"    "L" #'doom/load-session
        :desc "Restart & restore Emacs"      "r" #'doom/restart-and-restore
        :desc "Restart Emacs"                "R" #'doom/restart)

      ;;; <leader> r --- remote
      (:when (featurep! :tools upload)
        (:prefix-map ("r" . "remote")
          :desc "Upload local"               "u" #'ssh-deploy-upload-handler
          :desc "Upload local (force)"       "U" #'ssh-deploy-upload-handler-forced
          :desc "Download remote"            "d" #'ssh-deploy-download-handler
          :desc "Diff local & remote"        "D" #'ssh-deploy-diff-handler
          :desc "Browse remote files"        "." #'ssh-deploy-browse-remote-handler
          :desc "Detect remote changes"      ">" #'ssh-deploy-remote-changes-handler))

      ;;; <leader> s --- search
      (:prefix-map ("s" . "search")
        :desc "Search buffer"                "b" #'swiper
        :desc "Search current directory"     "d" #'+default/search-cwd
        :desc "Search other directory"       "D" #'+default/search-other-cwd
        :desc "Locate file"                  "f" #'locate
        :desc "Jump to symbol"               "i" #'imenu
        :desc "Jump to visible link"         "l" #'link-hint-open-link
        :desc "Jump to link"                 "L" #'ffap-menu
        :desc "Jump list"                    "j" #'evil-show-jumps
        :desc "Jump to bookmark"             "m" #'bookmark-jump
        :desc "Look up online"               "o" #'+lookup/online
        :desc "Look up online (w/ prompt)"   "O" #'+lookup/online-select
        :desc "Look up in local docsets"     "k" #'+lookup/in-docsets
        :desc "Look up in all docsets"       "K" #'+lookup/in-all-docsets
        :desc "Search project"               "p" #'+default/search-project
        :desc "Search other project"         "P" #'+default/search-other-project
        :desc "Jump to mark"                 "r" #'evil-show-marks
        :desc "Search buffer"                "s" #'swiper-isearch
        :desc "Search buffer for thing at point" "S" #'swiper-isearch-thing-at-point
        :desc "Dictionary"                   "t" #'+lookup/dictionary-definition
        :desc "Thesaurus"                    "T" #'+lookup/synonyms)

      ;;; <leader> t --- toggle
      (:prefix-map ("t" . "toggle")
        :desc "Big mode"                     "b" #'doom-big-font-mode
        :desc "Flymake"                      "f" #'flymake-mode
        (:when (featurep! :checkers syntax)
          :desc "Flycheck"                   "f" #'flycheck-mode)
        :desc "Frame fullscreen"             "F" #'toggle-frame-fullscreen
        :desc "Evil goggles"                 "g" #'evil-goggles-mode
        (:when (featurep! :ui indent-guides)
          :desc "Indent guides"              "i" #'highlight-indent-guides-mode)
        :desc "Indent style"                 "I" #'doom/toggle-indent-style
        :desc "Line numbers"                 "l" #'doom/toggle-line-numbers
        (:when (featurep! :lang org +present)
          :desc "org-tree-slide mode"        "p" #'org-tree-slide-mode)
        :desc "Read-only mode"               "r" #'read-only-mode
        (:when (featurep! :checkers spell)
          :desc "Flyspell"                   "s" #'flyspell-mode)
        (:when (featurep! :lang org +pomodoro)
          :desc "Pomodoro timer"             "t" #'org-pomodoro)
        :desc "Soft line wrapping"           "w" #'visual-line-mode
        (:when (featurep! :ui word-wrap)
          :desc "Soft line wrapping"         "w" #'+word-wrap-mode)
        :desc "Zen mode"                     "z" #'writeroom-mode)

      ;;; <leader> w --- workspace
      (:when (featurep! :ui workspaces)
        (:prefix-map ("w" . "workspace")
          :desc "Display workspaces"        "TAB" #'+workspace/display
          :desc "Switch workspace"          "."   #'+workspace/switch-to
          :desc "Switch to last workspace"  "`"   #'+workspace/other
          :desc "New workspace"             "n"   #'+workspace/new
          :desc "Load workspace from file"  "l"   #'+workspace/load
          :desc "Save workspace to file"    "s"   #'+workspace/save
          :desc "Delete session"            "x"   #'+workspace/kill-session
          :desc "Delete this workspace"     "d"   #'+workspace/delete
          :desc "Rename workspace"          "r"   #'+workspace/rename
          :desc "Restore last session"      "R"   #'+workspace/restore-last-session
          :desc "Next workspace"            "]"   #'+workspace/switch-right
          :desc "Previous workspace"        "["   #'+workspace/switch-left
          :desc "Switch to 1st workspace"   "1"   #'+workspace/switch-to-0
          :desc "Switch to 2nd workspace"   "2"   #'+workspace/switch-to-1
          :desc "Switch to 3rd workspace"   "3"   #'+workspace/switch-to-2
          :desc "Switch to 4th workspace"   "4"   #'+workspace/switch-to-3
          :desc "Switch to 5th workspace"   "5"   #'+workspace/switch-to-4
          :desc "Switch to 6th workspace"   "6"   #'+workspace/switch-to-5
          :desc "Switch to 7th workspace"   "7"   #'+workspace/switch-to-6
          :desc "Switch to 8th workspace"   "8"   #'+workspace/switch-to-7
          :desc "Switch to 9th workspace"   "9"   #'+workspace/switch-to-8
          :desc "Switch to final workspace" "0"   #'+workspace/switch-to-final))

      ;; APPs
      ;;; <leader> M --- mu4e
      (:when (featurep! :email mu4e)
        (:prefix-map ("M" . "mu4e")
          :desc "Open email app" "M" #'=mu4e
          :desc "Compose email"  "c" #'+mu4e/compose))

      ;;; <leader> I --- IRC
      (:when (featurep! :app irc)
        (:prefix-map ("I" . "irc")
          :desc "Open irc app"       "I" #'=irc
          :desc "Next unread buffer" "a" #'tracking-next-buffer
          :desc "Quit irc"           "q" #'+irc/quit
          :desc "Reconnect all"      "r" #'circe-reconnect-all
          :desc "Send message"       "s" #'+irc/send-message
          (:when (featurep! :completion ivy)
            :desc "Jump to channel"  "j" #'irc/ivy-jump-to-channel)))

      ;;; <leader> T --- twitter
      (:when (featurep! :app twitter)
        (:prefix-map ("T" . "twitter")
          :desc "Open twitter app" "T" #'=twitter
          :desc "Quit twitter"     "q" #'+twitter/quit
          :desc "Rerender twits"   "r" #'+twitter/rerender-all
          :desc "Ace link"         "l" #'+twitter/ace-link)))


;;
;;; Global & plugin keybinds

(map! "C-'" #'imenu

      ;;; Text scaling
      [C-mouse-4] #'text-scale-increase
      [C-mouse-5] #'text-scale-decrease
      [C-down-mouse-2] (λ! (text-scale-set 0))
      "M-+" #'doom/reset-font-size
      "M-=" #'doom/increase-font-size
      "M--" #'doom/decrease-font-size

      ;;; newlines
      [remap newline]  #'newline-and-indent
      "C-j"            #'+default/newline

      ;;; search
      (:when (featurep! :completion ivy)
        "C-S-s"        #'swiper
        "C-S-r"        #'ivy-resume)
      (:when (featurep! :completion helm)
        "C-S-s"        #'swiper-helm
        "C-S-r"        #'helm-resume)

      ;;; objed
      (:when (featurep! :editor objed +manual)
        "M-SPC"     #'objed-activate)

      ;;; buffer management
      "C-x b"       #'switch-to-buffer
      "C-x 4 b"     #'switch-to-buffer-other-window
      (:when (featurep! :ui workspaces)
        "C-x b"       #'persp-switch-to-buffer
        "C-x B"       #'switch-to-buffer
        "C-x 4 B"     #'switch-to-buffer-other-window
        (:when (featurep! :completion ivy)
          "C-x 4 b"   #'+ivy/switch-workspace-buffer-other-window))
      "C-x C-b"     #'ibuffer-list-buffers
      "C-x K"       #'doom/kill-this-buffer-in-all-windows

      ;;; company-mode
      "C-;" #'+company/complete
      (:after company
        :map company-active-map
        "C-o"        #'company-search-kill-others
        "C-n"        #'company-select-next
        "C-p"        #'company-select-previous
        "C-h"        #'company-quickhelp-manual-begin
        "C-S-h"      #'company-show-doc-buffer
        "C-s"        #'company-search-candidates
        "M-s"        #'company-filter-candidates
        [C-tab]      #'company-complete-common-or-cycle
        [tab]        #'company-complete-common-or-cycle
        [backtab]    #'company-select-previous
        "C-RET"      #'counsel-company
        :map company-search-map
        "C-n"        #'company-search-repeat-forward
        "C-p"        #'company-search-repeat-backward
        "C-s"        (λ! (company-search-abort) (company-filter-candidates)))

      ;;; ein notebooks
      (:after ein:notebook-multilang
        :map ein:notebook-multilang-mode-map
        "C-c h" #'+ein/hydra/body)

      ;;; expand-region
      "C-="  #'er/expand-region
      "C--"  #'er/contract-region

      ;;; flycheck
      (:after flycheck
        :map flycheck-error-list-mode-map
        "C-n" #'flycheck-error-list-next-error
        "C-p" #'flycheck-error-list-previous-error
        "RET" #'flycheck-error-list-goto-error)

      ;;; help and info
      (:after help-mode
        :map help-mode-map
        "o" #'link-hint-open-link
        ">" #'help-go-forward
        "<" #'help-go-back
        "n" #'forward-button
        "p" #'backward-button)
      (:after helpful
        :map helpful-mode-map
        "o" #'link-hint-open-link)
      (:after apropos
        :map apropos-mode-map
        "o" #'link-hint-open-link
        "n" #'forward-button
        "p" #'backward-button)
      (:after info
        :map Info-mode-map
        "o" #'link-hint-open-link)

      ;;; ivy & counsel
      (:when (featurep! :completion ivy)
        (:after ivy
          :map ivy-minibuffer-map
          "TAB"   #'ivy-alt-done
          "C-g"   #'keyboard-escape-quit)
        (:after counsel
          :map counsel-ag-map
          "C-SPC" #'ivy-call-and-recenter ; preview
          "M-RET" #'+ivy/git-grep-other-window-action)
        "C-M-y"   #'counsel-yank-pop)

      ;;; neotree
      (:when (featurep! :ui neotree)
        "<f9>"    #'+neotree/open
        "<C-f9>"  #'+neotree/find-this-file
        (:after neotree
          :map neotree-mode-map
          "q"     #'neotree-hide
          "RET"   #'neotree-enter
          "SPC"   #'neotree-quick-look
          "v"     #'neotree-enter-vertical-split
          "s"     #'neotree-enter-horizontal-split
          "c"     #'neotree-create-node
          "D"     #'neotree-delete-node
          "g"     #'neotree-refresh
          "r"     #'neotree-rename-node
          "R"     #'neotree-refresh
          "h"     #'+neotree/collapse-or-up
          "l"     #'+neotree/expand-or-open
          "n"     #'neotree-next-line
          "p"     #'neotree-previous-line
          "N"     #'neotree-select-next-sibling-node
          "P"     #'neotree-select-previous-sibling-node))

      ;;; popups
      (:when (featurep! :ui popup)
        "C-x p"   #'+popup/other
        "C-`"     #'+popup/toggle
        "C-~"     #'+popup/raise)

      ;;; smartparens
      (:after smartparens
        :map smartparens-mode-map
        "C-M-a"     #'sp-beginning-of-sexp
        "C-M-e"     #'sp-end-of-sexp
        "C-M-f"     #'sp-forward-sexp
        "C-M-b"     #'sp-backward-sexp
        "C-M-d"     #'sp-splice-sexp
        "C-M-k"     #'sp-kill-sexp
        "C-M-t"     #'sp-transpose-sexp
        "C-<right>" #'sp-forward-slurp-sexp
        "M-<right>" #'sp-forward-barf-sexp
        "C-<left>"  #'sp-backward-slurp-sexp
        "M-<left>"  #'sp-backward-barf-sexp)

      ;;; treemacs
      (:when (featurep! :ui treemacs)
        "<f9>"   #'+treemacs/toggle
        "<C-f9>" #'+treemacs/find-file)

      ;;; yasnippet
      (:after yasnippet
        :map yas-keymap  ; keymap while editing an inserted snippet
        "C-e"           #'+snippets/goto-end-of-field
        "C-a"           #'+snippets/goto-start-of-field
        "<S-tab>"       #'yas-prev-field
        "<M-backspace>" #'+snippets/delete-to-start-of-field
        [backspace]     #'+snippets/delete-backward-char
        [delete]        #'+snippets/delete-forward-char-or-field))
