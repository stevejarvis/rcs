; paths
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/evil")
(add-to-list 'load-path "~/.emacs.d/sr-speedbar")

; general settings
(load-theme 'tsdh-dark)
(which-function-mode 1)
;; indentation
(setq c-default-style "k&r"
      c-basic-offset 4)
(define-key global-map (kbd "RET") 'newline-and-indent)
;; general bindings
(global-set-key (kbd "C-c m") 'compile)

; enable CENET
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(semantic-mode 1)
(global-ede-mode 1)
;; bind keys
(global-set-key (kbd "C-c j") 'semantic-ia-fast-jump)
(global-set-key (kbd "C-c p") 'pop-global-mark)
(global-set-key (kbd "C-c r") 'semantic-symref)

; evil mode
(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode 1)
;; want insert mode to actually be emacs
(add-hook 'evil-insert-state-entry-hook 'evil-emacs-state)
;; key-chord and kj to escape from insert
(require 'key-chord)
(key-chord-define evil-insert-state-map "kj" 'evil-normal-state)
(key-chord-define evil-visual-state-map "kj" 'evil-normal-state)
(key-chord-define evil-emacs-state-map "kj" 'evil-normal-state)
(key-chord-mode 1)

; speedbar
(require 'sr-speedbar)
(sr-speedbar-open)
(global-set-key (kbd "C-c s") 'sr-speedbar-toggle)

; highlight over 80 char
(require 'whitespace)
(setq whitespace-style '(face lines-tail trailing))
(global-whitespace-mode t)

; delete trailing space
(add-hook 'before-save-hook 'delete-trailing-whitespace)

; optional perforce with standard RCS commands
(require 'vc-p4 nil 'noerror)
