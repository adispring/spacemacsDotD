
;; This is your old M-x.
(global-set-key (kbd "C-c C-x M-x") 'execute-extended-command)

;; search
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c s") 'spacemacs/helm-project-do-ag)
(global-set-key (kbd "C-c d") 'swiper-thing-at-point)

(global-set-key (kbd "M-s o") 'occur-dwim)
(global-set-key (kbd "M-s e") 'iedit-mode)
;; (bind-key* "M-s o" 'occur-dwim)

;; M-w save current line when no region selected
(global-set-key [remap kill-ring-save] 'easy-kill)

;; expand-region
(global-set-key (kbd "M-m M-w") 'er/expand-region)

(global-set-key (kbd "M-m M-g") 'find-file-at-point)

;; switch to jsx-ide
(global-set-key (kbd "C-c x") (quote js2-jsx-mode))
(global-set-key (kbd "C-c z") (quote react-mode))

(bind-key* "C-c l" 'adispring/insert-chrome-current-tab-url)

;; insert
(global-set-key (kbd "C-i") (lambda () (interactive) (evil-insert-newline-below)))

(global-set-key "\M-n" 'adi/scroll-up-in-place)
(global-set-key "\M-p" 'adi/scroll-down-in-place)

(global-set-key (kbd "C-c o") 'browse-url)
