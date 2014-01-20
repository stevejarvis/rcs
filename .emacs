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
(load-theme 'zenburn t)
(which-function-mode t)

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

;; highlight over 80 char
(require 'whitespace)
(setq whitespace-style '(face lines-tail))
(global-whitespace-mode t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; control those backup files
(setq backup-directory-alist `((".*" . "~/.saves_emacs"))
      kept-new-versions 6
      kept-old-versions 2
      delete-old-versions t
      backup-by-copying t)

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
(global-set-key (kbd "C-c j") 'semantic-ia-fast-jump)
(global-set-key (kbd "C-c J") 'semantic-complete-jump)
(global-set-key (kbd "C-c o") 'pop-global-mark)
(global-set-key (kbd "C-c t") 'semantic-analyze-proto-impl-toggle)
(global-set-key (kbd "C-c r") 'semantic-symref)
(global-set-key (kbd "C-c SPC") 'semantic-ia-complete-symbol)
(global-set-key (kbd "C-c K") 'semantic-ia-show-doc)

;-------------------------------------------------------------------------------
; evil mode
;-------------------------------------------------------------------------------
(setq evil-want-C-u-scroll t)
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
  (setq c-basic-offset 4))
(add-hook 'c-mode-common-hook 'm-c-mode-common-hook)

;; c/cpp
(defun m-c-mode-hook()
  (setq c-default-style "k&r"))
(add-hook 'c-mode-hook 'm-c-mode-hook)
(add-hook 'c++-mode-hook 'm-c-mode-hook)

;; python
(defun m-python-mode-hook()
  (setq python-indent 4
        python-indent-guess-indent-offset nil))
(add-hook 'python-mode-hook 'm-python-mode-hook)

;; LaTeX
(defun m-latex-mode-hook()
  (flyspell-mode t))
(add-hook 'latex-mode-hook 'm-latex-mode-hook)

;; mu4e compose
(defun m-compose-mode-hook()
  (flyspell-mode t))
(add-hook 'mu4e-compose-mode-hook 'm-compose-mode-hook)

;-------------------------------------------------------------------------------
; optional work settings
;-------------------------------------------------------------------------------
(require 'vsat nil t)

;-------------------------------------------------------------------------------
; mu4e email
;-------------------------------------------------------------------------------
(require 'mu4e)
(global-set-key (kbd "C-x m") 'mu4e)

(setq mu4e-drafts-folder "/Drafts")
(setq mu4e-sent-folder   "/Sent")
(setq mu4e-trash-folder  "/Trash")

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
(setq mu4e-maildir-shortcuts
      '( ("/INBOX"     . ?i)
         ("/Sent"      . ?s)
         ("/Drafts"    . ?d)
         ("/Trash"     . ?t)))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap"
      mu4e-update-interval 900)

;; something about ourselves
(setq user-mail-address "steve@modtalk.org"
      user-full-name  "Steve Jarvis")

;; rendering
(setq mu4e-html2text-command "w3m")

;; sending mail
(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials '(("modtalk.org" 587 nil nil))
      smtpmail-auth-credentials
      '(("modtalk.org" 587 "steve@modtalk.org" nil))
      smtpmail-default-smtp-server "modtalk.org"
      smtpmail-smtp-server "modtalk.org"
      smtpmail-smtp-service 587)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)
