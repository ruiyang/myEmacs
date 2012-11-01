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
(setq-default tab-width 2) ; set tab width to 4 for all buffers
(setq-default indent-tabs-mode nil) ; always replace tabs with spaces

;; customize find-grep
(setq grep-find-command "find . -name 'target' -prune -o -name 'webapp*assets' -prune -o -name 'public' -prune -o -name 'cache' -prune -o -name '*' ! -name '*~' ! -name 'old-*.js' ! -name 'old-*.css' ! -name 'ext*.js' ! -name 'yui*.js' ! -name '*.dll' ! -name '*.pdb' -print0 | xargs -0 grep -H -n ")

(defun refresh-file ()
  (interactive)
  (revert-buffer t t t)
  )

(global-set-key [f5] 'refresh-file)
(global-set-key [(f1)] (lambda () (interactive) (manual-entry (current-word))))
(global-set-key [f2] 'find-dired)
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

;;
(autoload 'javascript-mode "javascript" nil t)
(setq js-indent-level 2)

(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.hbs$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.ftl$" . html-mode))
(add-to-list 'auto-mode-alist '("buildfile" . ruby-mode))

(custom-set-variables
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
 '(js2-indent-on-enter-key t)
)

(setq javascript-indent-level 2)

;;
(setq c-basic-offset 2)


; =================

; Add css mode

; =================

(autoload 'css-mode "css-mode")
(setq auto-mode-alist
     (cons '("\\.scss\\'" . css-mode) auto-mode-alist))

(setq auto-mode-alist
     (cons '("\\.ftl\\'" . html-mode) auto-mode-alist))

(setq css-indent-offset 2)

;; For tabs configuration

(require 'cc-mode)
(defun my-build-tab-stop-list (width)
  (let ((num-tab-stops (/ 80 width))
	(counter 1)
	(ls nil))
    (while (<= counter num-tab-stops)
      (setq ls (cons (* width counter) ls))
      (setq counter (1+ counter)))
    (set (make-local-variable 'tab-stop-list) (nreverse ls))))

(defun my-c-mode-common-hook ()
  (setq tab-width 2) ;; change this to taste, this is what K&R uses :)
  (my-build-tab-stop-list tab-width)
  (setq c-basic-offset tab-width)
  (setq indent-tabs-mode nil)) ;; force only spaces for indentation
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;;xml mode
;; (load "nxml-mode-20041004/rng-auto.el")
;; (setq auto-mode-alist
;;       (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\|xul\\|rdf\\)\\'" . nxml-mode)
;;                     auto-mode-alist))

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

;; enable EasyPG
;; (require 'epa)
;; (epa-file-enable)

;; include slime
;; (require 'setup-slime-js)
;; (add-to-list 'load-path "/usr/share/emacs24/site-lisp/slime")
;; (require 'slime)
;; (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
;; (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))

;; ;;
;; (global-set-key [f7] 'slime-js-reload)
;; (add-hook 'js2-mode-hook
;;           (lambda ()
;;             (slime-js-minor-mode 1)))

;; ;; slime css
;; (add-hook 'css-mode-hook
;;           (lambda ()
;;             (define-key css-mode-map "\M-\C-x" 'slime-js-refresh-css)
;;             (define-key css-mode-map "\C-c\C-r" 'slime-js-embed-css)))



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
