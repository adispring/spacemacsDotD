
;; using smex's memorize & counsel's complete
(global-set-key (kbd "M-x") 'counsel-M-x)
;; This is your old M-x.
(global-set-key (kbd "C-c C-x M-x") 'execute-extended-command)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)

;; search
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c s") 'spacemacs/helm-project-do-ag)
(global-set-key (kbd "C-c d") 'swiper-thing-at-point)

(global-set-key (kbd "M-s i") 'counsel-imenu)
(global-set-key (kbd "M-s o") 'occur-dwim)
(global-set-key (kbd "M-s e") 'iedit-mode)
;; (bind-key* "M-s o" 'occur-dwim)

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

;; do not nedd the following key-binding, C-x C-j required dired-x
;; (global-set-key (kbd "C-x C-j") #'dired-jump)

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

  ;; disable company default <return> behavior
  ;;; Prevent suggestions from being triggered automatically. In particular,
  ;;; this makes it so that:
  ;;; - TAB will always complete the current selection.
  ;;; - RET will only complete the current selection if the user has explicitly
  ;;;   interacted with Company.
  ;;; - SPC will never complete the current selection.
  ;;;
  ;;; Based on:
  ;;; - https://github.com/company-mode/company-mode/issues/530#issuecomment-226566961
  ;;; - https://emacs.stackexchange.com/a/13290/12534
  ;;; - http://stackoverflow.com/a/22863701/3538165
  ;;;
  ;;; See also:
  ;;; - https://emacs.stackexchange.com/a/24800/12534
  ;;; - https://emacs.stackexchange.com/q/27459/12534

  ;; <return> is for windowed Emacs; RET is for terminal Emacs
(with-eval-after-load 'company
  (progn
    (dolist (key '("<return>" "RET"))
      ;; Here we are using an advanced feature of define-key that lets
      ;; us pass an "extended menu item" instead of an interactive
      ;; function. Doing this allows RET to regain its usual
      ;; functionality when the user has not explicitly interacted with
      ;; Company.
      (define-key company-active-map (kbd key)
        `(menu-item nil company-complete
                    :filter ,(lambda (cmd)
                               (when (company-explicit-action-p)
                                 cmd)))))

    (define-key company-active-map (kbd "TAB") #'company-complete-selection)
    (define-key company-active-map (kbd "SPC") nil)))

(bind-key* "C-c l" 'adispring/insert-chrome-current-tab-url)

;; insert
(global-set-key (kbd "C-i") (lambda () (interactive) (evil-insert-newline-below)))

(global-set-key "\M-n" 'adi/scroll-up-in-place)
(global-set-key "\M-p" 'aid/scroll-down-in-place)
