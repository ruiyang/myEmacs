(let ((default-directory "~/.emacs_includes"))
  (normal-top-level-add-subdirs-to-load-path))

;; add marlalade package repro (added because of slime-js)
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; sudo related
(require 'tramp)

(require 'dired+)
(require 'dired-details)
(dired-details-install)
;; Split windows in Emacs 22 compatible way
(setq split-height-threshold nil)
(setq split-width-threshold most-positive-fixnum)
;; no need to try to split window vertically based on the width
(setq split-width-threshold nil)

;; turn off the tool bar and get rid of the splash screen
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-splash-screen t)


(windmove-default-keybindings)

(setq x-select-enable-clipboard t)

(iswitchb-mode)

;; create an invisible backup directory and make the backups also invisable
(defun make-backup-file-name (filename)
  (defvar backups-dir "~/.backups/")
  (make-directory backups-dir t)
  (expand-file-name
   (concat backups-dir "." (file-name-nondirectory filename) "~")
   (file-name-directory filename)))

;; Disable all version control
(setq vc-handled-backends nil)


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

;; customize find-grep
(setq grep-find-command "find . -name 'target' -prune -o -name 'webapp*assets' -prune -o -name 'public' -prune -o -name 'cache' -prune -o -name '*' ! -name '*~' ! -name 'old-*.js' ! -name 'old-*.css' ! -name 'ext*.js' ! -name 'yui*.js' ! -name '*.dll' ! -name '*.pdb' -print0 | xargs -0 grep -H -n ")

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
  (shell-command "rm -fr /cygdrive/c/MYOB/liveaccounts/NikeOnline/target/classes/jasperReports/*; cp -fr /cygdrive/c/MYOB/liveaccounts/NikeOnline/src/main/resources/jasperReports/* /cygdrive/c/MYOB/liveaccounts/NikeOnline/target/classes/jasperReports")
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
(put 'upcase-region 'disabled t)

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
