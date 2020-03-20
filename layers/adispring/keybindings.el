;; Ivy keybindings

;; Ivy-based interface to standard commands
;; This is your old M-x.
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-c C-x M-x") 'execute-extended-command)
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)

;; Ivy-based interface to shell and system tools
;; (global-set-key (kbd "C-c g") 'counsel-git)
;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-rg)
(global-set-key (kbd "C-c m") 'counsel-linux-app)
(global-set-key (kbd "C-c n") 'counsel-fzf)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-c J") 'counsel-file-jump)
(global-set-key (kbd "C-c w") 'counsel-wmctrl)

;; Ivy-resume and other commands
;; ivy-resume resumes the last Ivy-based completion.
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "C-c b") 'counsel-bookmark)
(global-set-key (kbd "C-c t") 'counsel-load-theme)

;; search
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

(global-set-key (kbd "C-c u") 'browse-url)
