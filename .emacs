;-------------------------------------------------------------------------------
; auto load package
;-------------------------------------------------------------------------------
(require 'package)
(setq package-list '(evil key-chord magit p4 helm neotree
                          markdown-mode zenburn-theme
                          powerline powerline-evil company flycheck))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; install any package not already present
(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

;-------------------------------------------------------------------------------
; additional load paths
;-------------------------------------------------------------------------------
(let ((default-directory (expand-file-name "~/.emacs.d")))
  (normal-top-level-add-subdirs-to-load-path))

;-------------------------------------------------------------------------------
; general settings
;-------------------------------------------------------------------------------
;; theme
(setq inhibit-startup-message t)
(load-theme 'zenburn t)
(which-function-mode t)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; indentation
(setq-default indent-tabs-mode nil
              tab-width 4)
(define-key global-map (kbd "RET") 'newline-and-indent)

;; make bindings
(global-set-key (kbd "C-c m") 'compile)
(global-set-key (kbd "C-c n") 'next-error)
(global-set-key (kbd "C-c p") 'previous-error)

;; scroll-smoothly
(setq scroll-step 1
      ;scroll-margin 5 ;; scroll-margin set makes compilation window jump
      scroll-conservatively 10000
      auto-window-vscroll nil)

;; highlight
(require 'whitespace)
(setq whitespace-style '(face lines-tail))
(global-whitespace-mode t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(defun m-highlight ()
  "highlight keywords, like TODO"
  (interactive)
  (defvar action-keywords-regex
    (regexp-opt
     '("TODO" "BUG" "FIXME" "NOTE"
       "@todo" "@bug" "@fixme" "@note")
     'words))
  (highlight-regexp action-keywords-regex 'hi-blue))
(add-hook 'find-file-hooks 'm-highlight)
;; highlight current line
(global-hl-line-mode +1)

;; control those backup files
(setq backup-directory-alist `((".*" . "~/.saves_emacs"))
      kept-new-versions 6
      kept-old-versions 2
      delete-old-versions t
      backup-by-copying t)

;; auto create matching parens/braces and highlight match
(show-paren-mode t)
(electric-pair-mode t)

;; file navigation
(global-set-key (kbd "C-c , o") 'ff-find-other-file)
(global-set-key (kbd "C-c o") 'pop-global-mark)
;; alt key too hard
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; fix the PATH variable
(defun m-set-exec-path-from-shell()
  (let ((path-from-shell (shell-command-to-string "TERM=vt100 $SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when window-system (m-set-exec-path-from-shell))

;-------------------------------------------------------------------------------
; company
;-------------------------------------------------------------------------------
;; complete anything... immediately
(defun m-company-hook()
  (global-company-mode)
  (setq company-idle-delay 0
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil)
  ; C-n/p more natural than M-n/p
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))
(add-hook 'after-init-hook 'm-company-hook)

;; don't want company in gud-gdb
(defun m-gud-gdb-hook()
  (company-mode nil))
(add-hook 'gdb-mode-hook 'm-gud-gdb-hook)

;-------------------------------------------------------------------------------
; compilation settings
;-------------------------------------------------------------------------------
(setq compilation-scroll-output t)
;; 1 - include warnings and errors 2 - include errors
(setq compilation-skip-threshold 1)
;; cap M recompile last target
(global-set-key (kbd "C-c M") 'recompile)

;-------------------------------------------------------------------------------
; flycheck
;-------------------------------------------------------------------------------
(require 'flycheck)
(global-flycheck-mode)

;-------------------------------------------------------------------------------
; powerline
;-------------------------------------------------------------------------------
(powerline-evil-center-color-theme)
;; format -- adapted from the default evil powerline
(setq-default mode-line-format
  (quote
   ("%e"
    (:eval
     (let*
         ((active
           (powerline-selected-window-active))
          (mode-line
           (if active 'mode-line 'mode-line-inactive))
          (face1
           (if active 'powerline-active1 'powerline-inactive1))
          (face2
           (if active 'powerline-active2 'powerline-inactive2))
          (separator-left
           (intern
            (format "powerline-%s-%s" powerline-default-separator
                    (car powerline-default-separator-dir))))
          (separator-right
           (intern
            (format "powerline-%s-%s" powerline-default-separator
                    (cdr powerline-default-separator-dir))))
          (lhs
           (list
            (powerline-raw "%*" nil 'l)
            (powerline-buffer-size nil 'l)
            (powerline-buffer-id nil 'l)
            (powerline-raw " ")
            (funcall separator-left mode-line face1)
            (powerline-narrow face1 'l)
            (powerline-vc face1)))
          (rhs
           (list
            (powerline-raw global-mode-string face1 'r)
            (powerline-raw "%4l" face1 'r)
            (powerline-raw ":" face1)
            (powerline-raw "%3c" face1 'r)
            (funcall separator-right face1 mode-line)
            (powerline-raw " ")
            (powerline-raw "%6p" nil 'r)
            (powerline-hud face2 face1)))
          (center
           (append
            (list
             (powerline-raw " " face1)
             (funcall separator-left face1 face2)
             (when
                 (boundp 'erc-modified-channels-object)
               (powerline-raw erc-modified-channels-object face2 'l))
             (powerline-major-mode face2 'l)
             (powerline-process face2)
             (powerline-raw " " face2))
            (let
                ((evil-face
                  (powerline-evil-face)))
              (if
                  (split-string
                   (format-mode-line minor-mode-alist))
                  (append
                   (if evil-mode
                       (list
                        (funcall separator-right face2 evil-face)
                        (powerline-raw
                         (powerline-evil-tag)
                         evil-face 'l)
                        (powerline-raw " " evil-face)
                        (funcall separator-left evil-face face2)))
                   (list
                    (powerline-raw " " face2)
                    (funcall separator-right face2 face1)))
                (list
                 (powerline-raw
                  (powerline-evil-tag)
                  evil-face)
                 (funcall separator-right evil-face face1)))))))
       (concat
        (powerline-render lhs)
        (powerline-fill-center face1
                               (/
                                (powerline-width center)
                                2.0))
        (powerline-render center)
        (powerline-fill face1
                        (powerline-width rhs))
        (powerline-render rhs)))))))

;-------------------------------------------------------------------------------
; CEDET
;-------------------------------------------------------------------------------
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(global-ede-mode t)
(semantic-mode t)

;; bind keys
;; changing a couple semantic shortcuts
(global-set-key (kbd "C-c , .") 'semantic-ia-fast-jump)
(global-set-key (kbd "C-c , t") 'semantic-analyze-proto-impl-toggle)
(global-set-key (kbd "C-c , K") 'semantic-ia-show-doc)
(global-set-key (kbd "C-c SPC") 'semantic-ia-complete-symbol)

;-------------------------------------------------------------------------------
; helm
;-------------------------------------------------------------------------------
(require 'helm)
(require 'helm-config)
(require 'helm-eshell)
(require 'helm-files)
(require 'helm-grep)

;; make tab still useful and finish words
;;; rebind tab to do persistent action
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
;;; make TAB works in terminal
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z")  'helm-select-action)

(define-key helm-grep-mode-map (kbd "<return>")
  'helm-grep-mode-jump-other-window)

(setq
  ;; do not display invisible candidates
  helm-quick-update t
  ;; open helm buffer in another window
  helm-split-window-default-side 'other
  ;; open helm buffer inside current window, not occupy whole other window
  helm-split-window-in-side-p t
  helm-candidate-number-limit 200
  helm-M-x-requires-pattern 0
  helm-boring-file-regexp-list
  '("\\.git$" "\\.hg$" "\\.la$" "\\.o$" "\\.pyc$")
  helm-ff-file-name-history-use-recentf t
  ;; needed in helm-buffers-list
  ido-use-virtual-buffers t
  helm-buffers-fuzzy-matching t)

;; override default bindings with helm equivalents
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-x") 'helm-M-x)

(global-set-key (kbd "C-c h k") 'helm-show-kill-ring)
(global-set-key (kbd "C-c h s") 'helm-semantic-or-imenu)
(global-set-key (kbd "C-c h m") 'helm-man-woman)
(global-set-key (kbd "C-c h f") 'helm-find)

;; save current position to mark ring
(add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

(helm-mode 1)

;-------------------------------------------------------------------------------
; neotree
;-------------------------------------------------------------------------------
(require 'neotree)
(global-set-key (kbd "C-c C-n") 'neotree-toggle)
(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

;-------------------------------------------------------------------------------
; evil mode
;-------------------------------------------------------------------------------
(setq evil-want-C-u-scroll t
      evil-want-C-i-jump t)
(require 'evil)
(evil-mode t)

;; set modes for different buffers
(evil-set-initial-state 'git-commit-mode 'emacs)
(evil-set-initial-state 'dired-mode 'emacs)

;; insert mode actually be emacs
(add-hook 'evil-insert-state-entry-hook 'evil-emacs-state)

;; emacs 24.5 suddently stopped respecting evil-want-C-u-scroll
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)

;-------------------------------------------------------------------------------
; key chord
;-------------------------------------------------------------------------------
(require 'key-chord)
(key-chord-define evil-insert-state-map "kj" 'evil-normal-state)
(key-chord-define evil-visual-state-map "kj" 'evil-normal-state)
(key-chord-define evil-emacs-state-map "kj" 'evil-normal-state)
(key-chord-mode t)

;-------------------------------------------------------------------------------
; version controls
;-------------------------------------------------------------------------------
(require 'p4)
(require 'magit)

(defun m-magit-blame()
  "prefix function for magit blame"
  (interactive)
  (progn
    (magit-blame-mode)
    (evil-emacs-state)))

(defun m-magit-status()
  "prefix function for magit status"
  (interactive)
  (magit-status))

(defun m-magit-log()
  "prefix function for magit log"
  (interactive)
  (magit-log))

(global-set-key (kbd "C-x g s") 'm-magit-status)
(global-set-key (kbd "C-x g b") 'm-magit-blame)
(global-set-key (kbd "C-x g l") 'm-magit-log)

;-------------------------------------------------------------------------------
; languages
;-------------------------------------------------------------------------------
;; all cc modes
(defun m-c-mode-common-hook()
  (setq c-basic-offset 4
        sr-speedbar-width 32)
  (flyspell-prog-mode))
(add-hook 'c-mode-common-hook 'm-c-mode-common-hook)

;; c/cpp
(defun m-c-mode-hook()
  (setq ff-find-other-file-alist '(("\\.cpp$" (".h"))
                                   ("\\.c$" (".h"))
                                   ("\\.h$" (".cpp"))
                                   ("\\.h$" (".c"))))
  (setq ff-search-directories '("." "../src" "../include"))
  (setq c-default-style "stroustrup")
  (c-set-style "stroustrup"))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-hook 'c-mode-hook 'm-c-mode-hook)
(add-hook 'c++-mode-hook 'm-c-mode-hook)

;; python
(defun m-python-shell-send-buffer-and-switch()
  "Send the current buffer to a python shell and show that shell."
  (interactive)
  ;;; for some reason, save-current-buffer and save-excursion
  ;;; do not restore the code buffer
  (python-shell-send-buffer)
  (python-shell-switch-to-shell)
  (end-of-buffer)
  (other-window 1))

(defun m-python-mode-hook()
  (setq python-indent 4
        python-indent-guess-indent-offset nil)
  (flyspell-prog-mode)
  (define-key python-mode-map (kbd "C-c C-c")
    'm-python-shell-send-buffer-and-switch))
(add-hook 'python-mode-hook 'm-python-mode-hook)

;; LaTeX
(defun m-latex-mode-hook()
  (flyspell-mode t)
  (auto-fill-mode t)
  (setq current-fill-column 80
        fill-column 80))
(add-hook 'latex-mode-hook 'm-latex-mode-hook)

;; JavaScript
(defun m-js-mode-hook()
  (flyspell-mode t)
  (setq js-indent-level 2))
(add-hook 'js-mode-hook 'm-js-mode-hook)

;; markdown
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(defun m-markdown-mode-hook()
  (flyspell-mode t)
  (auto-fill-mode t)
  (setq current-fill-column 80
        fill-column 80))
(add-hook 'markdown-mode-hook 'm-markdown-mode-hook)

;-------------------------------------------------------------------------------
; optional settings
;-------------------------------------------------------------------------------
;; don't want to load vsat stuff when editing general files in OS X
(if (not (eq system-type 'darwin))
    (require 'vsat nil t))
(require 'local nil t)
(require 'linux_dev nil t)
