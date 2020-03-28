;;; packages.el --- adi layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: wangzengdi <wangzengdi@wangzengdideMacBook-Pro.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `adi-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `adi/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `adi/pre-init-PACKAGE' and/or
;;   `adi/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(setq adi-layer-path (file-name-directory load-file-name))
(defconst adi-js-indent-level 4)

(defconst adi-packages
  '(
    projectile
    (dired-mode :location built-in)
    peep-dired
    (sh-mode :location built-in)
    avy
    dumb-jump
    multiple-cursors
    smartparens
    youdao-dictionary
    org
    flycheck
    company
    web-search
    web-mode
    css-mode
    less-css-mode
    rainbow-mode
    json-mode
    nodejs-repl
    ac-js2
    hexo
    prettier-js
    thrift
    tide
    geiser
    js-react-redux-yasnippets
    keyfreq
    git-gutter
    (livedown :location (recipe
                         :fetcher github
                         :repo "shime/emacs-livedown"));;markdown在线预览，设置来源github
    )
  )

(defun adi/init-js-react-redux-yasnippets ()
  (use-package js-react-redux-yasnippets
    :after yasnippet))

(defun adi/init-sh-mode ()
  (use-package sh-mode
    :mode (("\\.symlink\\'" . sh-mode))))

(defun adi/init-peep-dired ()
  (use-package peep-dired))

(defun adi/init-git-gutter ()
  (use-package git-gutter))

(defun adi/post-init-geiser ()
  (use-package geiser
    :ensure t
    :init (progn
            (setq scheme-program-name "chez")
            (setq geiser-chez-binary "chez")
            (setq geiser-active-implementations '(chez))
            ))
  )

(defun adi/init-keyfreq ()
  (use-package keyfreq
    :ensure t
    :config (progn
              (require 'init-keyfreq (concat adi-layer-path "lisp/init-keyfreq.el"))
              (turnon-keyfreq-mode)
              (init-keyfreq-excluded-commands)
              ))
  )


(defun adi/post-init-projectile ()
  (use-package projectile
    :config
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    (projectile-mode +1))
  )

(defun adi/init-multiple-cursors ()
  (use-package multiple-cursors
    :bind (("M-m M-l" . mc/edit-lines)
           ("M-m M-n" . mc/mark-next-like-this)
           ("M-m M-p" . mc/mark-previous-like-this)
           ("M-m M-a" . mc/mark-all-like-this))
    ))

(defun adi/post-init-tide ()
  (use-package tide
  :bind (("M-." . tide-jump-to-definition)
         ("M-," . tide-jump-back))
  :config
  (setup-tide-mode)))

(defun adi/init-thrift ()
  (use-package thrift
    :mode (("\\.proto\\'" . thrift-mode))
    ))

;; 每个 major mode 会自己设置本地 company-backends，在设置之前会清空本地 company-backends，
;; 所以自己设置的 company-backends 一般就不会起作用了，参考如下：
;; https://github.com/syl20bnr/spacemacs/issues/924
;;
;; 设置 company-backends 的最新方法：
;; https://emacs-china.org/t/topic/2590

(defun adi/post-init-company ()
  (use-package company
    :init
    (global-company-mode t)
    (setq company-show-numbers t)
    (add-hook 'markdown-mode-hook (lambda () (company-mode -1)) 'append)
    (add-hook 'org-mode-hook (lambda () (company-mode -1)) 'append)
    :config
    (progn
      (setq company-tooltip-align-annotations t)
      (define-key company-active-map (kbd "TAB") #'company-complete-selection)
      (define-key company-active-map (kbd "SPC") nil))
      ;; <return> is for windowed Emacs; RET is for terminal Emacs
      (dolist (key '("<return>" "RET"))
        (define-key company-active-map (kbd key)
          `(menu-item nil company-complete
                      :filter ,(lambda (cmd)
                                 (when (company-explicit-action-p)
                                   cmd)))))

    )
  )

;; markdown 实时预览
;; 在 md 文件下
;; M-x livedown:preview 开启
;; M-x livedown:kill 关闭
(defun adi/init-livedown ()
  (use-package livedown
    :ensure t
    :config
    (custom-set-variables
     '(livedown:autostart nil) ; 启动md自动打开预览功能 automatically open preview when opening markdown files
     '(livedown:open t)        ; 启动预览自动打开窗口automatically open the browser window
     '(livedown:port 1337))    ; 端口 port for livedown server
    )
  )

(defun adi/post-init-prettier-js ()
  (use-package prettier-js
    :hook (web-mode . prettier-js-mode)))

;; https://www.emacswiki.org/emacs/AutoModeAlist
(defun adi/post-init-json-mode ()
  (use-package json-mode
    :ensure t
    :mode (("\\.json\\'" . json-mode)
           ("\\eslintrc\\'" . json-mode))
    :config
    (add-hook 'json-mode-hook
              (lambda ()
                (add-hook 'before-save-hook 'web-beautify-js-buffer t t)))
    ))


(defun adi/post-init-org ()
  (use-package org
    :config
    (spacemacs|disable-company org-mode)
    (add-hook 'org-mode-hook (lambda () (fci-mode t)))
    ))

(defun adi/init-hexo ()
  (use-package hexo))

(defun adi/post-init-smartparens ()
  (if dotspacemacs-smartparens-strict-mode
      (add-hook 'web-mode-hook #'smartparens-strict-mode)
    (add-hook 'web-mode-hook #'smartparens-mode)))

;; https://github.com/jwiegley/use-package/issues/384
(defun adi/post-init-avy ()
  (use-package avy
    :bind (("C-c m" . avy-copy-region)
           ("C-c f" . avy-goto-char-2))))

(defun adi/post-init-less-css-mode ()
  (use-package less-css-mode
    :ensure t
    :mode "\\.less\\'"))

(defun adi/init-rainbow-mode ()
  (use-package rainbow-mode
    :hook css-mode))

(defun adi/post-init-css-mode ()
  (use-package css-mode
    :mode (("\\.cssm?\\'" . css-mode)
           ("\\.scss\\'" . css-mode))
    :config
    (defun css-imenu-make-index ()
      (save-excursion
        (imenu--generic-function '((nil "^ *\\([^ ]+\\) *{ *$" 1)))))

    (add-hook 'css-mode-hook
              (lambda ()
                (add-hook 'before-save-hook 'web-beautify-css-buffer t t))))

    (add-hook 'css-mode-hook
              (lambda ()
                (setq imenu-create-index-function 'css-imenu-make-index))))

(defun adi/post-init-dumb-jump ()
  (use-package dumb-jump
    :bind (("M-g o" . dumb-jump-go-other-window)
           ("M-g j" . dumb-jump-go)
           ("M-g b" . dumb-jump-back)
           ("M-g i" . dumb-jump-go-prompt)
           ("M-g x" . dumb-jump-go-prefer-external)
           ("M-g z" . dumb-jump-go-prefer-external-other-window))
    :config (setq dumb-jump-selector 'ivy)
    )
  )

(defun adi/init-youdao-dictionary ()
  (use-package youdao-dictionary
    :bind ("C-c y" . youdao-dictionary-search-at-point+)))

(defun adi/init-ac-js2 ()
  (use-package ac-js2
    :defer t
    :init
    ;; M . : jump to definitions
    (setq ac-js2-evaluate-calls t)))

(defun adi/post-init-nodejs-repl ()
  (use-package nodejs-repl
    :bind (
           :map web-mode-map
                ("C-c C-p" . nodejs-repl-send-last-sexp)
                ("C-c C-o" . nodejs-repl-send-region)
                ("C-c C-z" . nodejs-repl-switch-to-repl))
    ))

(defun adi/init-web-search ()
  (use-package web-search
    :bind ("C-c C-v" . web-search)))

(defun adi/post-init-web-mode ()
  (use-package web-mode
    :mode ("\\.html?\\'" "\\.jsx?\\'")
    :config
    (adi-web-mode-indent-setup adi-js-indent-level)
    (adi-js-imenu-setup)
    (tide-mode)
    (when (string-match-p "\\.jsx?\\'" (file-name-extension buffer-file-name))
      (setup-tide-mode))
    (setq web-mode-enable-auto-pairing t)
    (setq web-mode-enable-css-colorization t)
    (web-mode-toggle-current-element-highlight)
    (web-mode-dom-errors-show)
    (setq emmet-expand-jsx-className? t)
    (defadvice web-mode-highlight-part (around tweak-jsx activate)
      (if (string-match-p "jsx?" web-mode-content-type )
          (let ((web-mode-enable-part-face nil)) ad-do-it)
        ad-do-it))))

(defun adi/post-init-flycheck ()
  (use-package flycheck
    :config
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(javascript-jshint tsx-tide)))
    ;; (flycheck-add-mode 'javascript-standard 'web-mode)
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (flycheck-add-mode 'javascript-eslint 'typescript-tsx-mode)
    (flycheck-add-next-checker 'javascript-standard 'javascript-eslint 'append)
    ;; (add-hook 'web-mode-hook #'adi/web-use-standard-from-node-modules)
    (add-hook 'web-mode-hook #'adi/web-use-eslint-from-node-modules)
    (add-hook 'typescript-tsx-mode-hook #'adi/web-use-eslint-from-node-modules)
    ))

(defun adi/init-dired-mode ()
  (use-package dired-mode
    :defer t
    :init
    (progn
      (require 'dired-x)
      (require 'dired-aux)
      (setq dired-listing-switches "-alh")

      (setq dired-omit-files
            (concat dired-omit-files "\\|^.DS_Store$\\|^.projectile$\\|\\.js\\.meta$\\|\\.meta$"))

      (add-hook 'dired-mode-hook
                (lambda ()
                  ;; Set dired-x buffer-local variables here.  For example:
                  (dired-omit-mode 1)
                  ))
      ;; always delete and copy recursively
      (setq dired-recursive-deletes 'always)
      (setq dired-recursive-copies 'always)

      (define-key dired-mode-map "e" 'ora-ediff-files)
      (define-key dired-mode-map (kbd "C-x v") 'peep-dired)

      (defvar dired-filelist-cmd '(("vlc" "-L")))

      )))

;;; packages.el ends here
