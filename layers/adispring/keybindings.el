(global-set-key "\C-s" 'swiper)

;; using smex's memorize & counsel's complete
(global-set-key (kbd "M-x") 'counsel-M-x)
;; This is your old M-x.
(global-set-key (kbd "C-c C-x M-x") 'execute-extended-command)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-c s") 'spacemacs/helm-project-do-ag)

;; M-w save current line when no region selected
(global-set-key [remap kill-ring-save] 'easy-kill)

;; expand-region
(global-set-key (kbd "M-m M-w") 'er/expand-region)

(global-set-key (kbd "M-m M-g") 'find-file-at-point)

;; multiple-cusors
(global-set-key (kbd "M-m M-l") 'mc/edit-lines)
(global-set-key (kbd "M-m M-n") 'mc/mark-next-like-this)
(global-set-key (kbd "M-m M-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-m M-a") 'mc/mark-all-like-this)

;; custome hotkey
(global-set-key (kbd "C-x C-j") #'dired-jump)

;; switch to jsx-ide
(global-set-key (kbd "C-c x")         (quote js2-jsx-mode))
(global-set-key (kbd "C-c z")         (quote react-mode))

;; web-mode
(add-hook 'web-mode-hook
          (lambda ()
            (define-key web-mode-map (kbd "C-c C-p") 'nodejs-repl-send-last-sexp)
            (define-key web-mode-map (kbd "C-c C-o") 'nodejs-repl-send-region)
            (define-key web-mode-map (kbd "C-c C-m") 'nodejs-repl-send-line)
            (define-key web-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl)))

