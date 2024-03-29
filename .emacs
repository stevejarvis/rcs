;;; package --- General settings, setting up my emacs.
; auto load package
;-------------------------------------------------------------------------------
; auto-fetch things from melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq package-list '(; keys and usability
                     evil
                     key-chord
                     helm
                     projectile helm-projectile
                     ;neotree
                     powerline powerline-evil
                     auto-complete
                     flycheck
		     ; actually install ag/silver searcher for ag and dumb-search
		     ag
		     dumb-jump
                     undo-tree
        	     ;helm-ag
                     ; source control
                     magit
                     ; theme
                     solarized-theme
                     zenburn-theme
                     ; languages beyond what's baked in
                     markdown-mode
                     go-mode
		     yaml-mode
                     pug-mode
		     ws-butler
                     terraform-mode
                     protobuf-mode
                     ))

;; install that stuff.
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; install any package not already present
(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

;-------------------------------------------------------------------------------
; general settings
;-------------------------------------------------------------------------------
;; theme
(setq inhibit-startup-message t)
(load-theme 'zenburn t)
;(load-theme 'solarized-dark t)
;; font size, height is 1/10 "size", so 120 = 12pt.
(set-face-attribute 'default nil :height 140)
;;; get rid of the typical GUI menu bar
(tool-bar-mode -1)
(menu-bar-mode -1)

;;; make bindings
(global-set-key (kbd "C-c m") 'compile)
(global-set-key (kbd "C-c n") 'next-error)
(global-set-key (kbd "C-c p") 'previous-error)

;;; scroll-smoothly
(setq scroll-step 1
      scroll-conservatively 10000
      auto-window-vscroll nil)

;; tabs can just go right ahead and die
(setq-default indent-tabs-mode nil)
(setq tab-width 2)

;;; clean up whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
; only cleans up lines that changed, so no noisy commits
(add-hook 'prog-mode-hook #'ws-butler-mode)

;;; highlight key words and current line
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
;;; highlight current line
(global-hl-line-mode +1)

;;; control those backup files. they show up everywhere.
(setq backup-directory-alist `((".*" . "~/.saves_emacs"))
      kept-new-versions 6
      kept-old-versions 2
      delete-old-versions t
      backup-by-copying t)
;;; don't need lock files
(setq create-lockfiles nil)

;;; auto create matching parens/braces and highlight match
(electric-pair-mode t)
(show-paren-mode t)

;;; fix the PATH variable
(defun m-set-exec-path-from-shell()
  (let ((path-from-shell (shell-command-to-string "TERM=vt100 $SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when window-system (m-set-exec-path-from-shell))

;; auto-complete
(ac-config-default)

;; turn on flycheck
(require 'flycheck)
(global-flycheck-mode)

;-------------------------------------------------------------------------------
; compilation settings
;-------------------------------------------------------------------------------
(setq compilation-scroll-output t)
;; 1 - include warnings and errors 2 - include errors
(setq compilation-skip-threshold 1)
;; cap M recompile last target
(global-set-key (kbd "C-c M") 'recompile)

;-------------------------------------------------------------------------------
; helm and projectile
;-------------------------------------------------------------------------------
(require 'helm)
(require 'helm-projectile)
(projectile-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(require 'helm-config)
(require 'helm-files)
(require 'helm-grep)

;; make tab still useful and finish words
;;; rebind tab to do persistent action
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)

(setq
  ;; open helm buffer in another window
  ;helm-split-window-default-side 'other
  ;; open helm buffer inside current window, not occupy whole other window
  ;helm-split-window-in-side-p t
  helm-candidate-number-limit 200
  helm-boring-file-regexp-list '("\\.git$" "\\.hg$" "\\.la$" "\\.o$" "\\.pyc$")
  helm-ff-file-name-history-use-recentf t
  ;; needed in helm-buffers-list
  helm-buffers-fuzzy-matching t)

;; override default bindings with helm equivalents
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)
;; alt key too hard
(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "M-x") 'helm-M-x)

(global-set-key (kbd "C-c h k") 'helm-show-kill-ring)
(global-set-key (kbd "C-c h m") 'helm-man-woman)
(global-set-key (kbd "C-c h f") 'helm-find)

;; save current position to mark ring
(add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

(helm-mode 1)

;-------------------------------------------------------------------------------
; turn on xref for dumb-jump
;-------------------------------------------------------------------------------
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

;-------------------------------------------------------------------------------
; neotree
;-------------------------------------------------------------------------------
;(require 'neotree)
;(global-set-key (kbd "C-c C-n") 'neotree-toggle)
;(add-hook 'neotree-mode-hook
;          (lambda ()
;            (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
;            (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
;            (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
;            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

;-------------------------------------------------------------------------------
; evil mode
;-------------------------------------------------------------------------------
(setq evil-want-C-u-scroll t
      evil-want-C-i-jump t)
(require 'evil)
(evil-mode t)

;; set modes for different buffers
(evil-set-initial-state 'git-commit-mode 'emacs)

;; evil defines M-., which conflicts with xref-js, so unbind it.
(define-key evil-normal-state-map (kbd "M-.") nil)

;; insert mode actually be emacs
(add-hook 'evil-insert-state-entry-hook 'evil-emacs-state)

;;; more info https://github.com/emacs-evil/evil/pull/1360
(global-undo-tree-mode)
;(setq evil-undo-system 'undo-tree)
(evil-set-undo-system 'undo-tree)

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
(require 'magit)

(global-set-key (kbd "C-x g s") 'magit-status)
(global-set-key (kbd "C-x g b") 'magit-blame)

;-------------------------------------------------------------------------------
; languages
;-------------------------------------------------------------------------------
;; plain text that would be nice to wrap and spellcheck
(defun m-plaintext-hook()
  (flyspell-mode t)
  (auto-fill-mode t))

;-------------------------------------------------------------------------------
;; python
;-------------------------------------------------------------------------------

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

;-------------------------------------------------------------------------------
;; JavaScript
;-------------------------------------------------------------------------------
(setq js-indent-level 2)
(add-hook 'js-mode-hook (lambda ()
  (setq tab-width 2)))

(defun m-pug-mode-hook()
  (remove-hook 'before-save-hook 'delete-trailing-whitespace t)
  (message "Cleared trailing whitespace hooker"))
(add-hook 'pug-mode-hook 'm-pug-mode-hook)

;-------------------------------------------------------------------------------
;; Go
;-------------------------------------------------------------------------------
; go install github.com/rogpeppe/godef@latest
; go install golang.org/x/tools/cmd/goimports@latest
;; the make command doesn't quite work, need to get vc-root-dir of current buffer..
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "M-.") 'godef-jump)
                          (setq gofmt-command "goimports")
                          (add-hook 'before-save-hook 'gofmt-before-save)
                          (setq tab-width 4)
                          (set (make-local-variable 'compile-command)
                               (concat "cd " (vc-root-dir) "; make install; cd -"))))

;-------------------------------------------------------------------------------
;; markdown
;-------------------------------------------------------------------------------
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
;;; markdown preview needs a markdown parser in path
;;; symlink "~/bin/markdown" to an app that can do it
(setq markdown-command (format "%s/bin/markdown" (getenv "HOME")))
(add-hook 'markdown-mode-hook 'm-plaintext-hook)

;; LaTex
(defun m-latex-mode-hook()
  ;;; M-q to reformat current paragraph
  (auto-fill-mode)
  (set-fill-column 80))
(add-hook 'latex-mode-hook 'm-plaintext-hook)
(add-hook 'latex-mode-hook 'm-latex-mode-hook)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("b77a00d5be78f21e46c80ce450e5821bdc4368abf4ffe2b77c5a66de1b648f10" default))
 '(package-selected-packages
   '(protobuf-mode terraform-mode ws-butler pug-mode yaml-mode go-mode markdown-mode zenburn-theme solarized-theme magit undo-tree dumb-jump ag flycheck auto-complete powerline-evil powerline helm-projectile projectile helm key-chord evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
