;;; packages.el --- adispring layer packages file for Spacemacs.
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
;; added to `adispring-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `adispring/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `adispring/pre-init-PACKAGE' and/or
;;   `adispring/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(setq adispring-layer-path (file-name-directory load-file-name))
(defconst adispring-js-indent-level 4)

(defconst adispring-packages
  '(
    projectile
    rust-mode
    racer
    (dired-mode :location built-in)
    (sh-mode :location built-in)
    peep-dired
    avy
    dumb-jump
    multiple-cursors
    smartparens
    youdao-dictionary
    org
    flycheck
    company
    company-quickhelp
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
    vlf
    js-react-redux-yasnippets
    keyfreq
    git-gutter
    (livedown :location (recipe
                         :fetcher github
                         :repo "shime/emacs-livedown"));;markdown在线预览，设置来源github
    )
  )

(defun adispring/init-js-react-redux-yasnippets ()
  (use-package js-react-redux-yasnippets
    :after yasnippet
    ))

(defun adispring/init-sh-mode ()
  (use-package sh-mode
    :mode (("\\.symlink\\'" . sh-mode))))

(defun adispring/init-peep-dired ()
  (use-package peep-dired))

(defun adispring/init-git-gutter ()
  (use-package git-gutter))

(defun adispring/post-init-geiser ()
  (use-package geiser
    :ensure t
    :init (progn
            (setq scheme-program-name "chez")
            (setq geiser-chez-binary "chez")
            (setq geiser-active-implementations '(chez))
            ))
  )

(defun adispring/init-keyfreq ()
  (use-package keyfreq
    :ensure t
    :config (progn
              (require 'init-keyfreq (concat adispring-layer-path "lisp/init-keyfreq.el"))
              (turnon-keyfreq-mode)
              (init-keyfreq-excluded-commands)
              ))
  )


(defun adispring/post-init-projectile ()
  (use-package projectile
    :ensure t
    :config
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    (projectile-mode +1))
  )

(defun adispring/post-init-racer ()
  (use-package racer
    :ensure t
    :init (progn
            (add-hook 'rust-mode-hook #'racer-mode)
            (add-hook 'racer-mode-hook #'eldoc-mode)))
  )

(defun adispring/post-init-rust-mode ()
  (use-package rust-mode
    :ensure t
    :init (progn
            (add-hook 'rust-mode-hook 'cargo-minor-mode)
            (add-hook 'toml-mode-hook 'cargo-minor-mode))))

(defun adispring/init-multiple-cursors ()
  (use-package multiple-cursors
    :bind (("M-m M-l" . mc/edit-lines)
           ("M-m M-n" . mc/mark-next-like-this)
           ("M-m M-p" . mc/mark-previous-like-this)
           ("M-m M-a" . mc/mark-all-like-this))
    ))

(defun adispring/post-init-tide ()
  (use-package tide
  :ensure t
  :bind (("M-." . tide-jump-to-definition)
         ("M-," . tide-jump-back)
         )

  :config
  (setup-tide-mode)
  ;; aligns annotation to the right hand side

  ;; formats the buffer before saving
  ;;(add-hook 'before-save-hook 'tide-format-before-save)

  ;; configure javascript-tide checker to run after your default javascript checker
  ;; (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)

  ;; configure jsx-tide checker to run after your default jsx checker
  ;; (flycheck-add-mode 'javascript-eslint 'web-mode)
  ;; (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)
  )
  )

(defun adispring/init-thrift ()
  (use-package thrift
    :mode (("\\.proto\\'" . thrift-mode))
    ))

;; 每个 major mode 会自己设置本地 company-backends，在设置之前会清空本地 company-backends，
;; 所以自己设置的 company-backends 一般就不会起作用了，参考如下：
;; https://github.com/syl20bnr/spacemacs/issues/924
;;
;; 设置 company-backends 的最新方法：
;; https://emacs-china.org/t/topic/2590

(defun adispring/post-init-company ()
  (use-package company
    :defer t
    :init
    (progn
      (global-company-mode t)
      (setq company-show-numbers t)
      (add-hook 'markdown-mode-hook (lambda () (company-mode -1)) 'append)
      (add-hook 'org-mode-hook (lambda () (company-mode -1)) 'append)
      )
    :config
    (progn
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
      (setq company-tooltip-align-annotations t)
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
      (define-key company-active-map (kbd "SPC") nil))
    )
  )

(defun adispring/post-init-company-quickhelp ()
  (use-package company-quickhelp
    :after company
    :config
    (company-quickhelp-mode))
  )

;; markdown 实时预览
;; 在 md 文件下
;; M-x livedown:preview 开启
;; M-x livedown:kill 关闭
(defun adispring/init-livedown ()
  (use-package livedown
    :config
    (custom-set-variables
     '(livedown:autostart nil) ; 启动md自动打开预览功能 automatically open preview when opening markdown files
     '(livedown:open t)        ; 启动预览自动打开窗口automatically open the browser window
     '(livedown:port 1337))    ; 端口 port for livedown server
    (require 'livedown)
    )
  )

(defun adispring/post-init-prettier-js ()
  (use-package prettier-js
    :config (setq prettier-js-command "prettier")
    (add-hook 'typescript-mode-hook 'prettier-js-mode)
    (add-hook 'typescript-tsx-mode-hook 'prettier-js-mode)
    )
  )

;; https://www.emacswiki.org/emacs/AutoModeAlist
(defun adispring/post-init-json-mode ()
  (use-package json-mode
    :ensure t
    :mode (("\\.json\\'" . json-mode)
           ("\\eslintrc\\'" . json-mode))
    :config
    (setq-default js-indent-level adispring-js-indent-level)
    (add-hook 'json-mode-hook
              (lambda ()
                (add-hook 'before-save-hook 'web-beautify-js-buffer t t)))
    ))


(defun adispring/post-init-org ()
  (use-package org
    :config
    (spacemacs|disable-company org-mode)
    (add-hook 'org-mode-hook (lambda () (fci-mode t)))
    ))

(defun adispring/init-hexo ()
  (use-package hexo))

(defun adispring/post-init-smartparens ()
  (if dotspacemacs-smartparens-strict-mode
      (add-hook 'web-mode-hook #'smartparens-strict-mode)
    (add-hook 'web-mode-hook #'smartparens-mode)))

;; https://github.com/jwiegley/use-package/issues/384
(defun adispring/post-init-avy ()
  (use-package avy
    :bind (("C-c m" . avy-copy-region)
           ("C-c f" . avy-goto-char-2))))

(defun adispring/post-init-less-css-mode ()
  (use-package less-css-mode
    :ensure t
    :mode "\\.less\\'"))

(defun adispring/init-rainbow-mode ()
  (use-package rainbow-mode
    :init
    :hook css-mode))

(defun adispring/post-init-css-mode ()
  (use-package css-mode
    :mode (("\\.css\\'" . css-mode)
           ("\\.cssm\\'" . css-mode)
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

(defun adispring/post-init-dumb-jump ()
  (use-package dumb-jump
    :bind (("M-g o" . dumb-jump-go-other-window)
           ("M-g j" . dumb-jump-go)
           ("M-g b" . dumb-jump-back)
           ("M-g i" . dumb-jump-go-prompt)
           ("M-g x" . dumb-jump-go-prefer-external)
           ("M-g z" . dumb-jump-go-prefer-external-other-window))
    :config (setq dumb-jump-selector 'ivy) ;; (setq dumb-jump-selector 'helm)
    )
  )



(defun adispring/init-youdao-dictionary ()
  (use-package youdao-dictionary
    :defer t
    :init
    :bind ("C-c y" . youdao-dictionary-search-at-point+)
    )
  )

(defun adispring/init-ac-js2 ()
  (use-package ac-js2
    :defer t
    :init
    ;; M . : jump to definitions
    (setq ac-js2-evaluate-calls t)
    )
  )

(defun adispring/post-init-nodejs-repl ()
  (use-package nodejs-repl
    :defer t
    :bind (
           :map web-mode-map
                ("C-c C-p" . nodejs-repl-send-last-sexp)
                ("C-c C-o" . nodejs-repl-send-region)
                ("C-c C-z" . nodejs-repl-switch-to-repl))
    ))

(defun adispring/init-web-search ()
  (use-package web-search
    :bind ("C-c C-v" . web-search)
    ))

(defun adispring/post-init-web-mode ()
  (use-package web-mode
    :mode (
           ("\\.html?\\'" . web-mode)
           ("\\.jsx?\\'" . web-mode))
    :config
    (add-hook 'web-mode-hook
              (lambda ()
                (adi-web-mode-indent-setup adispring-js-indent-level)))
    (add-hook 'web-mode-hook 'adi-js-imenu-setup)
    (add-hook 'web-mode-hook 'tide-mode)
    ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Anonymous-Functions.html#Anonymous-Functions
    (add-hook 'web-mode-hook #'(lambda ()
                                 (enable-minor-mode
                                  '("\\.jsx?\\'" . prettier-js-mode)
                                  )))
    (add-hook 'web-mode-hook
              (lambda ()
                (when (string-match-p "\\.jsx?\\'" (file-name-extension buffer-file-name))
                  (setup-tide-mode))))
    (setq web-mode-enable-auto-pairing t)
    (setq web-mode-enable-css-colorization t)
    (web-mode-toggle-current-element-highlight)
    (web-mode-dom-errors-show)
    (setq emmet-expand-jsx-className? t)
    (defadvice web-mode-highlight-part (around tweak-jsx activate)
      (if (string-match-p "jsx?" web-mode-content-type )
          (let ((web-mode-enable-part-face nil))
            ad-do-it)
        ad-do-it))
    )

  )

(defun adispring/post-init-flycheck ()
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

(defun adispring/init-vlf ()
  (use-package vlf
    :config
    (custom-set-variables '(vlf-application 'dont-ask))
    ))

(defun adispring/init-dired-mode ()
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
      (define-key dired-mode-map (kbd "C-x p") 'peep-dired)

      (defvar dired-filelist-cmd '(("vlc" "-L")))

      )))

;;; packages.el ends here
