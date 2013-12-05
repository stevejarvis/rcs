; paths
(let ((default-directory (expand-file-name "~/.emacs.d")))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

; general settings
;; theme
(when (not (require 'vsat nil t))
  (load-theme 'tsdh-dark))
(which-function-mode 1)
;; indentation
(setq c-default-style "k&r"
      c-basic-offset 4)
(setq-default indent-tabs-mode nil)
(define-key global-map (kbd "RET") 'newline-and-indent)
;; general bindings
(global-set-key (kbd "C-c m") 'compile)
;; scroll-smoothly
(setq scroll-step 1)
(setq scroll-margin 5)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)
;; optional perforce
(require 'p4 nil 'noerror)
;; delete trailing space
(add-hook 'before-save-hook 'delete-trailing-whitespace)

; enable CEDET
;; work/home diffs
(when (not (require 'vsat nil t))
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
  (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
  (global-ede-mode 1)
  (semantic-mode 1)
  ;; bind keys
  (global-set-key (kbd "C-c j") 'semantic-ia-fast-jump)
  (global-set-key (kbd "C-c p") 'pop-global-mark)
  (global-set-key (kbd "C-c r") 'semantic-symref))

; evil mode
(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode 1)
;; want insert mode to actually be emacs
(add-hook 'evil-insert-state-entry-hook 'evil-emacs-state)

; key-chord and kj to escape from insert
(require 'key-chord)
(key-chord-define evil-insert-state-map "kj" 'evil-normal-state)
(key-chord-define evil-visual-state-map "kj" 'evil-normal-state)
(key-chord-define evil-emacs-state-map "kj" 'evil-normal-state)
(key-chord-mode 1)

; speedbar
(require 'sr-speedbar)
(global-set-key (kbd "C-c s") 'sr-speedbar-toggle)

; highlight over 80 char
(require 'whitespace)
(setq whitespace-style '(face lines-tail))
(global-whitespace-mode t)

(put 'downcase-region 'disabled nil)
