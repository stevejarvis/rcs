;-------------------------------------------------------------------------------
; load paths
;-------------------------------------------------------------------------------
(let ((default-directory (expand-file-name "~/.emacs.d")))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

;-------------------------------------------------------------------------------
; general settings
;-------------------------------------------------------------------------------
;; theme
(load-theme 'manoj-dark)
(which-function-mode t)
;; indentation style
(setq c-default-style "k&r"
      c-basic-offset 4)
(setq-default indent-tabs-mode nil)
(define-key global-map (kbd "RET") 'newline-and-indent)
;; make bindings
(global-set-key (kbd "C-c m") 'compile)
(global-set-key (kbd "C-c n") 'next-error)
(global-set-key (kbd "C-c p") 'previous-error)
;; scroll-smoothly
(setq scroll-step 1)
(setq scroll-margin 5)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)
;; optional perforce
(require 'p4 nil t)
;; highlight over 80 char
(require 'whitespace)
(setq whitespace-style '(face lines-tail))
(global-whitespace-mode t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

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
; optional work settings
;-------------------------------------------------------------------------------
(require 'vsat nil t)