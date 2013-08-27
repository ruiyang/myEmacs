(let ((default-directory "~/.emacs_includes"))
  (normal-top-level-add-subdirs-to-load-path))

;; add marlalade package repro (added because of slime-js)
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; sudo related
;; C-x C-f /sudo::/etc/hosts
(require 'tramp)

;; "(" ")" to hide and show details
(require 'dired+)
(require 'dired-details)
(dired-details-install)

(require 'ido)
(setq ido-enable-last-directory-history nil)
(setq ido-enable-flex-matching t)
(setq ido-auto-merge-work-directories-length -1) ;; disable auto-merge
;; (ido-everywhere t)
(setq ido-create-new-buffer 'always)
(setq ido-default-file-method 'selected-window)
(setq ido-use-filename-at-point 'guess)
(ido-mode t)
(add-hook 'ido-setup-hook 
          (lambda () 
            (define-key ido-completion-map [tab] 'ido-complete)))

(let ((default-directory "~/liveaccounts/frontend/"))
  (shell "ruby"))
(let ((default-directory "~/liveaccounts/"))
  (shell "1"))

;; Setup for org
;; Set to the location of your Org files on your local system
(setq org-directory "~/Dropbox/Personal/org")
(setq org-todo-keywords
      '((type "TODO(t)" "DOING(d!)" "PAUSED(p@!)" "|" "DONE(o!)" "CANCELED(c@!)")))

(setq org-agenda-files (list "~/Dropbox/Personal/org"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-split-window-function (quote split-window-horizontally))
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-mobile-agendas (quote default)))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline (concat org-directory "/capture.org") "Tasks")
             "* TODO %?\t\t%u\n")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
             "* %?\nEntered on %U\n  %i\n  %a")))

(setq org-default-notes-file (concat org-directory "/capture.org"))
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-ca" 'org-agenda)

;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/Dropbox/Personal/org/mobile-inbox.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
(setq org-mobile-files '("~/Dropbox/Personal/org"))
(setq org-mobile-force-id-on-agenda-items nil)

(org-agenda-to-appt)
(appt-activate)

(fset 'org-help
      "echo \"EOL
----Headline---------
(S-)M-Left/Right     change line heading line (tree) level
M-Up/Down            move tree up or down
C-c C-c              toggle checkbox
----TODO Operations--
C-c .                insert a timestamp
C-c C-s              add scheduled time
C-c C-t              jump to a state
----Capture----------
C-c c                capture
C-c C-c              save capture
----Agenda View------
C-c a a              agenda view of the current week
C-c a t              all todo items
C-u r                search the agenda matching a tag
---------------------
EOL
\"\C-m")

;; Ring navigation:
;; M-g ]         Go to next search results buffer, restore its current search context
;; M-g [         Ditto, but selects previous buffer.
;;               Navigation is cyclic.
;;
;; Stack navigation:
;; M-g -         Pop to previous search results buffer (kills top search results buffer)
;; M-g _         Clear the search results stack (kills all grep-a-lot buffers!)
;;
;; Other:
;; M-g =         Restore buffer and position where current search started
(require 'grep-a-lot)
(grep-a-lot-setup-keys)

;; Split windows in Emacs 22 compatible way
(setq split-height-threshold nil)
(setq split-width-threshold most-positive-fixnum)
;; no need to try to split window vertically based on the width
(setq split-width-threshold nil)

;; turn off the tool bar and get rid of the splash screen
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-splash-screen t)

;; window frame operations, maximize window with C-c (right|left)
(windmove-default-keybindings)
(when (fboundp 'winner-mode)
  (winner-mode 1))

(setq x-select-enable-clipboard t)

(iswitchb-mode)
(line-number-mode)
(column-number-mode)

;; create an invisible backup directory and make the backups also invisable
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; (defun make-backup-file-name (filename)
;;   (defvar backups-dir "~/.backups/")
;;   (make-directory backups-dir t)
;;   (expand-file-name
;;    (concat backups-dir "." (file-name-nondirectory filename) "~")
;;    (file-name-directory filename)))

;; Disable all version control
;; (setq vc-handled-backends nil)


;; set indent size
;;(customize-variable (quote tab-stop-list))
(setq-default indent-tabs-mode nil) ; always replace tabs with spaces
(setq-default indent-line-function 'insert-tab)
(setq-default tab-stop-list (number-sequence 2 200 2))
(setq-default tab-width 2)

;; javascript settings. used a fork version of js2-mode, which has indent fix.
;; source: git://github.com/mooz/js2-mode.git
(autoload 'javascript-mode "javascript" nil t)
(setq js-indent-level 2)
(setq c-basic-offset tab-width)

(autoload 'js2-mode "js2-mode" nil t)
(setq js2-bounce-indent-p t)
(setq js2-indent-on-enter-key nil)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.hbs$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.ftl$" . html-mode))
(add-to-list 'auto-mode-alist '("buildfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))

;; customize find-grep
(setq grep-find-command "find . -name 'target' -prune -o -name 'webapp*assets' -prune -o -name '.bundle' -prune -o -name 'public' -prune -o -name 'cache' -prune -o -name '*' ! -name '*~' ! -name 'old-*.js' ! -name 'old-*.css' ! -name 'ext*.js' ! -name 'yui*.js' ! -name '*.dll' ! -name '*.pdb' ! -name 'development.log' -print0 | xargs -0 grep -H -n ")

(defun refresh-file ()
  (interactive)
  (revert-buffer t t t)
  )

(global-set-key [f5] 'refresh-file)
(global-set-key [(f1)] (lambda () (interactive) (manual-entry (current-word))))
(global-set-key [f2] 'find-name-dired)
(global-set-key [f6] 'find-grep)
(global-set-key [f7] 'replace-string)
(global-set-key (kbd "C-f") 'forward-word)
(global-set-key (kbd "C-b") 'backward-word)
(global-set-key (kbd "M-f") 'forward-char)
(global-set-key (kbd "M-b") 'backward-char)
(global-set-key (kbd "M-d") 'delete-region)

(define-key dired-mode-map "=" 'dired-diff)

(global-set-key "\C-x[" 'comment-region)
(global-set-key "\C-x]" 'uncomment-region)
(global-set-key "\C-x=" 'align-regexp)
(global-set-key "\C-x+" 'align-repeat)
(global-set-key "\C-x:" 'erase-buffer)

;; override killing emacs key
(global-unset-key "\C-xc")
(global-unset-key "\C-x\C-c")
(global-unset-key "\C-z")
(global-unset-key "\C-x\C-z")
;; Don't want emacs to die easily, so route it to far away key!!!!
(global-set-key [(f12)] 'save-buffers-kill-emacs)
;; Don't want to suspend easily
(global-set-key [(f11)] 'redraw-display)

;; start the shell from the current file
(defun kill-shell-buffer ()
  (dolist (buffer (buffer-list))
    (let ((bufname (buffer-name buffer)))     
      (cond ((equal bufname "*shell*")
             (kill-buffer buffer))))))
(defun nshell ()
  (interactive)
  (kill-shell-buffer)
  (shell))

;;
(setq minibuffer-max-depth nil)

;; Jasper hotcode replace
(defun jasper-compile ()
  (interactive)
  (shell-command "rm -fr ~/liveaccounts/NikeOnline/target/classes/jasperReports/*; cp -fr ~/liveaccounts/NikeOnline/src/main/resources/jasperReports/* ~/liveaccounts/NikeOnline/target/classes/jasperReports")
  )
(global-set-key [f10] 'jasper-compile)

; =================

; Add css mode

; =================

(autoload 'css-mode "css-mode")
(setq auto-mode-alist
     (cons '("\\.scss\\'" . css-mode) auto-mode-alist))

(setq auto-mode-alist
     (cons '("\\.ftl\\'" . html-mode) auto-mode-alist))

(setq css-indent-offset 2)

(autoload 'css-mode "css-mode")
(setq auto-mode-alist
     (cons '("\\.scss\\'" . css-mode) auto-mode-alist))
(setq auto-mode-alist
     (cons '("\\.ftl\\'" . html-mode) auto-mode-alist))
(setq css-indent-offset 2)

(add-to-list 'load-path "~/.emacs_includes/emacs-goodies-el/")

(load-theme 'wheatgrass t)

; interpret and use ansi color codes in shell output windows
(ansi-color-for-comint-mode-on)

(setq ansi-color-names-vector
      ["black" "tomato" "PaleGreen2" "gold1"
       "DeepSkyBlue1" "MediumOrchid1" "cyan" "white"])
(setq ansi-color-map (ansi-color-make-color-map))

;; Toggle between split windows and a single window
(defun toggle-windows-split()
  "Switch back and forth between one window and whatever split of windows we might have in the frame. The idea is to maximize the current buffer, while being able to go back to the previous split of windows in the frame simply by calling this command again."
  (interactive)
  (if (not (window-minibuffer-p (selected-window)))
      (progn
        (if (< 1 (count-windows))
            (progn
              (window-configuration-to-register ?u)
              (delete-other-windows))
          (jump-to-register ?u))))
  (my-iswitchb-close))

(define-key global-map (kbd "C-|") 'toggle-windows-split)


;; Note: you may also need to define the my-iswitchb-close function 
;; created by Ignacio as well: http://emacswiki.org/emacs/IgnacioPazPosse
(defun my-iswitchb-close()
 "Open iswitchb or, if in minibuffer go to next match. Handy way to cycle through the ring."
 (interactive)
 (if (window-minibuffer-p (selected-window))
    (keyboard-escape-quit)))

;; make lines unique recursively.
  (defun uniquify-all-lines-region (start end)
    "Find duplicate lines in region START to END keeping first occurrence."
    (interactive "*r")
    (save-excursion
      (let ((end (copy-marker end)))
        (while
            (progn
              (goto-char start)
              (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
          (replace-match "\\1\n\\2")))))
  
  (defun uniquify-all-lines-buffer ()
    "Delete duplicate lines in buffer and keep first occurrence."
    (interactive "*")
    (uniquify-all-lines-region (point-min) (point-max)))

(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Ah, much better!"))

(put 'erase-buffer 'disabled nil)

;; ediff settings for git
(require 'ediff)
(defun ediff-current-buffer-revision () 
  "Run Ediff to diff current buffer's file against VC depot. 
Uses `vc.el' or `rcs.el' depending on `ediff-version-control-package'." 
  (interactive) 
  (let ((file (or (buffer-file-name) 
          (error "Current buffer is not visiting a file")))) 
(if (and (buffer-modified-p) 
     (y-or-n-p (message "Buffer %s is modified. Save buffer? " 
                (buffer-name)))) 
    (save-buffer (current-buffer))) 
(ediff-load-version-control) 
(funcall 
 (intern (format "ediff-%S-internal" ediff-version-control-package)) 
 "" "" nil))) 



;; helpful commands

(fset 'git-help
      "echo \"EOL
git log -p [branchname] // show details
git log -p -1 (show last one with details)
git log --graph --pretty=oneline --since='1 week ago'
git show HEAD^
git show HEAD~4
git log -p --since='1 week ago' global.scss // see logs for a single file
git tag v2.5 1b2e1d63ff //name a certain commit
git diff HEAD~10:file-path HEAD:file-path // diff across revisions
git add -i // add or remove files from index
git reset --hard origin/master // hard is only needed if you want to discard your local changes
git comment --amend // add some fix to your last commit, if you forget to pass checkstyle!
git log --since='7 day' --name-only
git log --diff-filter=D --summary --since='1 day ago'
git log master --not --remotes //show changes that are not pushed yet
git log -p -w -2 // ignore whitespace in 
git log --follow // follow renamed files
git diff [branch1]:[file1] [branch2]:[file2]
git checkout [branch] [file] --- revert a file from a different branch
git checkout [version]~1 [file] //revert file from previous commit
git checkout --ours filename.c
git checkout --theirs filename.c
git push origin <yourbranch>:<remotebranch>
git push origin :<remotebranch> // delete remote branch
git revert <commitid> // revert a commit
git reset --soft HEAD^ // remove last commit
EOL
\"\C-m")

(fset 'idea-keys
      "echo \"EOL
 Last Edition: Ctrl+Shift+Backspace
 Search Symbol: C+S+M+Backspace
 EOL
\n\"\C-m")

;; format json
(defun json-format ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
     "python -mjson.tool" (current-buffer) t)))

;; useful linux command
;; 'lsof -w -n -i tcp:8080'   list process using port 8080
(put 'upcase-region 'disabled nil)

(fset 'git-help-mo
      (concat "echo \"\n" 
              "git diff \n" 
              "git diff --cached\ngit status\ngit log -p [branchname] // show details\n"
              "git log -p -1 (show last one with details)\n"
              "git log --since='1 day ago'\n"
              "git show 44f1f96e23ecb835e4442bf464861231b2d96d7e\n"
              "git log --graph --pretty=oneline --since='1 week ago'\n"
              "git branch\n"
              "git checkout [branch] // switch between branches\n"
              "git merge [branch] // for conflicts, git diff (show conflicts), fix them, then git commit -a to commit them in. \n"
              "git branch -d [name] // delete a branch\n"
              "git show HEAD^\n"
              "git show HEAD~4\n"
              "git log -p --since='1 week ago' global.scss // see logs for a single file\n"
              "git tag v2.5 1b2e1d63ff //name a certain commit\n"
              "git diff HEAD~10:file-path HEAD:file-path // diff across revisions\n"
              "git add -i // add or remove files from index\n"
              "git reset --hard origin/master // hard is only needed if you want to discard your local changes\n"
              "git comment --amend // add some fix to your last commit, if you forget to pass checkstyle!\n"
              "git add -i\n"
              "git log --since='7 day' --name-only\n"
              "git log --diff-filter=D --summary --since='1 day ago'\n"
              "git log master --not --remotes //show changes that are not pushed yet\n"
              "git log --author=\"author-name\"\n"
              "git log -p -w -2 // ignore whitespace in comparison\n"
              "git checkout [branch] [file] --- revert a file from a different branch\n"
              "git diff [branch1]:[file1] [branch2]:[file2]\n"
              "git checkout [version]~1 [file] //revert file from previous commit\n"
              "git branch -d branch-name    -- delete a local branch\n"
              "git checkout --track -b production remoates/origin/production\n"
              "git checkout --ours filename.c\n"
              "git checkout --theirs filename.c\n"
              "git add filename.c\n"
              "git push origin <yourbranch>:<remotebranch>\n"
              "git push origin :<remotebranch> // delete remote branch\n"
              "git log origin/master..HEAD or git diff origin/master..HEAD // view changes that you are going to push\n"
              "git revert <commitid> // revert a commit\n"
              "git reset --soft HEAD^ // remove last commit\n"
              "git reset --hard origin/master // sync with remote repo (to undo merge, for ex)\n"
              "git commit --amend -m \"New commit message\" // edit last commit message \n"
              "git log --follow // follow renamed files \n"
              "git config --get remote.origin.url // get remote origin url \n"
              "git remote set-url origin /data/repo/sheepy-repo // change remote origin url \n"
              "git show HEAD^:main.cpp > old_main.cpp // checkout to a different file \n"
              "git commit -F- <<EOF // multiple line comment\n"
              "\"\C-m"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
