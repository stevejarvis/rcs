;-------------------------------------------------------------------------------
; auto load package
;-------------------------------------------------------------------------------
(require 'package)
(setq package-list '(evil
                     sr-speedbar
                     key-chord
                     magit
                     p4
                     zenburn-theme))

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
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

;-------------------------------------------------------------------------------
; general settings
;-------------------------------------------------------------------------------
;; theme
(setq inhibit-startup-message t)
(load-theme 'zenburn t)
(which-function-mode t)
(tool-bar-mode -1)

;; indentation
(setq-default indent-tabs-mode nil
              tab-width 4)
(define-key global-map (kbd "RET") 'newline-and-indent)

;; make bindings
(global-set-key (kbd "C-c m") 'compile)
(global-set-key (kbd "C-c n") 'next-error)
(global-set-key (kbd "C-c p") 'previous-error)
(setq compilation-scroll-output t)

;; scroll-smoothly
(setq scroll-step 1
      scroll-margin 5
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
     '("TODO" "BUG" "FIXME" "NOTE")
     'words))
  (highlight-regexp action-keywords-regex 'hi-blue))
(add-hook 'find-file-hooks 'm-highlight)

;; control those backup files
(setq backup-directory-alist `((".*" . "~/.saves_emacs"))
      kept-new-versions 6
      kept-old-versions 2
      delete-old-versions t
      backup-by-copying t)

(global-set-key (kbd "C-c , o") 'ff-find-other-file)
(show-paren-mode t)
(global-set-key (kbd "C-c o") 'pop-global-mark)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

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
(global-set-key (kbd "C-c K") 'semantic-ia-show-doc)
(global-set-key (kbd "C-c SPC") 'semantic-ia-complete-symbol)

;-------------------------------------------------------------------------------
; evil mode
;-------------------------------------------------------------------------------
(setq evil-want-C-u-scroll t
      evil-want-C-i-jump t)
(require 'evil)
(evil-mode t)

;; insert mode actually be emacs
(add-hook 'evil-insert-state-entry-hook 'evil-emacs-state)

;-------------------------------------------------------------------------------
; key chord
;-------------------------------------------------------------------------------
(require 'key-chord)
(key-chord-define evil-insert-state-map "kj" 'evil-normal-state)
(key-chord-define evil-visual-state-map "kj" 'evil-normal-state)
(key-chord-define evil-emacs-state-map "kj" 'evil-normal-state)
(key-chord-mode t)

;-------------------------------------------------------------------------------
; sr speedbar
;-------------------------------------------------------------------------------
(require 'sr-speedbar)
(global-set-key (kbd "C-c s") 'sr-speedbar-toggle)

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
  (magit-status default-directory))

(defun m-magit-log()
  "prefix function for magit log"
  (interactive)
  (magit-log))

(global-set-key (kbd "C-x g s") 'm-magit-status)
(global-set-key (kbd "C-x g b") 'm-magit-blame)
(global-set-key (kbd "C-x g l") 'm-magit-log)

;-------------------------------------------------------------------------------
; call me maybe
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
  (setq c-default-style "k&r"))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-hook 'c-mode-hook 'm-c-mode-hook)
(add-hook 'c++-mode-hook 'm-c-mode-hook)

;; python
(defun m-python-mode-hook()
  (setq python-indent 4
        python-indent-guess-indent-offset nil)
  (flyspell-prog-mode))
(add-hook 'python-mode-hook 'm-python-mode-hook)
;; LaTeX
(defun m-latex-mode-hook()
  (flyspell-mode t)
  (auto-fill-mode t)
  (setq current-fill-column 80
        fill-column 80))
(add-hook 'latex-mode-hook 'm-latex-mode-hook)

;-------------------------------------------------------------------------------
; optional settings
;-------------------------------------------------------------------------------
(require 'vsat nil t)
(require 'local nil t)
