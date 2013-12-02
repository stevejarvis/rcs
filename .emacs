; paths
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/evil")
(add-to-list 'load-path "~/.emacs.d/sr-speedbar")

; general settings
(load-theme 'tsdh-dark)
;; indentation
(setq c-default-style "k&r"
      c-basic-offset 4)
(define-key global-map (kbd "RET") 'newline-and-indent)
;; bindings
(global-set-key (kbd "C-c m") 'compile)

; enable CENET
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(semantic-mode 1)
(global-ede-mode 1)

;; tag bouncing
;(defvar semantic-tags-location-ring (make-ring 20))
;
;(defun semantic-goto-definition (point)
;  "Goto definition using semantic-ia-fast-jump
;save the pointer marker if tag is found"
;  (interactive "d")
;  (condition-case err
;      (progn
;        (ring-insert semantic-tags-location-ring (point-marker))
;        (semantic-ia-fast-jump point))
;    (error
;     ; if not found remove the tag saved in the ring
;     (set-marker (ring-remove semantic-tags-location-ring 0) nil nil)
;     (signal (car err) (cdr err)))))
;
;(defun semantic-pop-tag-mark ()
;  "popup the tag save by semantic-goto-definition"
;  (interactive)
;  (if (ring-empty-p semantic-tags-location-ring)
;      (message "%s" "No more tags available")
;    (let* ((marker (ring-remove semantic-tags-location-ring 0))
;              (buff (marker-buffer marker))
;                 (pos (marker-position marker)))
;      (if (not buff)
;            (message "Buffer has been deleted")
;        (switch-to-buffer buff)
;        (goto-char pos))
;      (set-marker marker nil nil))))

(global-set-key (kbd "C-c j") 'semantic-ia-fast-jump)
;(global-set-key (kbd "C-c b") 'semantic-pop-tag-mark)
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
(setq whitespace-action '(auto-cleanup))
(global-whitespace-mode t)

; delete trailing space
(add-hook 'before-save-hook 'delete-trailing-whitespace)
