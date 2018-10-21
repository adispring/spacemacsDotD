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

(defconst adispring-packages
  '(
    (dired-mode :location built-in)
    youdao-dictionary
    flycheck
    company
    tern
    company-tern
    avy
    web-search
    dumb-jump
    web-mode
    css-mode
    json-mode
    nodejs-repl
    ac-js2
    smartparens
    hexo
    org
    prettier-js
    (livedown :location (recipe
                         :fetcher github
                         :repo "shime/emacs-livedown"));;markdown在线预览，设置来源github
    )
  )

(defun adispring/post-init-company ()
  (use-package company
    :defer t
    :init
    (progn
      (global-company-mode t)
      (add-hook 'markdown-mode-hook (lambda () (company-mode -1)) 'append)
      (add-hook 'org-mode-hook (lambda () (company-mode -1)) 'append)
      )
    )
  )

(defun adispring/post-init-tern ()
  (use-package tern
    :ensure t
    :config
    (add-hook 'web-mode-hook 'tern-mode)
    :bind (:map tern-mode-keymap
                ("M-*" . tern-pop-find-definition))
    )
  )

(defun adispring/post-init-company-tern ()
  (use-package company-tern
    :ensure t
    :config
    (add-to-list 'company-backends 'company-tern)
    )
  )

;;markdown实时预览
;;在md文件下
;;M-x livedown:preview开启
;;M-x livedown:kill关闭
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
    :config (setq prettier-js-command "prettier-standard")
    )
  )

(defun adispring/init-prettier-standard-js ()
  (use-package prettier-standard-js)
  )

(defun adispring/post-init-json-mode ()
  (progn
    (add-hook 'json-mode-hook 'adi-web-mode-indent-setup)
    (add-hook 'web-mode-hook #'(lambda ()
                                 (enable-minor-mode
                                  '("\\.json\\'" . prettier-js-mode)
                                  )))
    ))

(defun adispring/post-init-org ()
  (progn
    (use-package org
      :bind (:map spacemacs-org-mode-map-root-map ("M-RET" . nil))
      ;; :config (setq org-startup-indented t)
      )
    (with-eval-after-load 'org
      (progn
        (spacemacs|disable-company org-mode)
        (add-hook 'org-mode-hook (lambda () (fci-mode t)))
        )
      )
    ))

(defun adispring/init-hexo ()
  (use-package hexo)
  )
(defun adispring/post-init-smartparens ()
  (if dotspacemacs-smartparens-strict-mode
      (add-hook 'web-mode-hook #'smartparens-strict-mode)
    (add-hook 'web-mode-hook #'smartparens-mode)))

(defun adispring/post-init-avy ()
  (progn
    (global-set-key (kbd "C-c m") 'avy-copy-region)
    (global-set-key (kbd "C-c f") 'avy-goto-char-2)
    ))

(defun adispring/post-init-css-mode ()
  (progn
    (add-to-list 'auto-mode-alist '("\\.css\\'"    . css-mode))       ;; CSS
    (add-to-list 'auto-mode-alist '("\\.less\\'"    . css-mode))       ;; LESS
    (add-to-list 'auto-mode-alist '("\\.scss\\'"    . css-mode))       ;; SCSS

    (dolist (hook '(css-mode-hook))
      (add-hook hook 'rainbow-mode))

    (defun css-imenu-make-index ()
      (save-excursion
        (imenu--generic-function '((nil "^ *\\([^ ]+\\) *{ *$" 1)))))

    (eval-after-load 'css-mode
      '(add-hook 'css-mode-hook
                 (lambda ()
                   (add-hook 'before-save-hook 'web-beautify-css-buffer t t))))

    (add-hook 'css-mode-hook
              (lambda ()
                (setq imenu-create-index-function 'css-imenu-make-index)))))

(defun adispring/post-init-dumb-jump ()
  (use-package dumb-jump
    :bind (("M-g o" . dumb-jump-go-other-window)
           ("M-g j" . dumb-jump-go)
           ("M-g b" . dumb-jump-back)
           ("M-g i" . dumb-jump-go-prompt)
           ("M-g x" . dumb-jump-go-prefer-external)
           ("M-g z" . dumb-jump-go-prefer-external-other-window))
    :config (setq dumb-jump-selector 'ivy) ;; (setq dumb-jump-selector 'helm)
    :ensure)
  )

(defun adispring/init-dired-mode ()
  (use-package dired-mode
    :defer t
    :init
    (progn
      (require 'dired-x)
      (require 'dired-aux)
      (setq dired-listing-switches "-alh")
      (setq dired-guess-shell-alist-user
            '(("\\.pdf\\'" "open")
              ("\\.docx\\'" "open")
              ("\\.\\(?:djvu\\|eps\\)\\'" "open")
              ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" "open")
              ("\\.\\(?:xcf\\)\\'" "open")
              ("\\.csv\\'" "open")
              ("\\.tex\\'" "open")
              ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|ogv\\)\\(?:\\.part\\)?\\'"
               "open")
              ("\\.\\(?:mp3\\|flac\\)\\'" "open")
              ("\\.html?\\'" "open")
              ("\\.md\\'" "open")))

      (setq dired-omit-files
            (concat dired-omit-files "\\|^.DS_Store$\\|^.projectile$\\|\\.js\\.meta$\\|\\.meta$"))

      ;; always delete and copy recursively
      (setq dired-recursive-deletes 'always)
      (setq dired-recursive-copies 'always)

      (defun ora-ediff-files ()
        (interactive)
        (let ((files (dired-get-marked-files))
              (wnd (current-window-configuration)))
          (if (<= (length files) 2)
              (let ((file1 (car files))
                    (file2 (if (cdr files)
                               (cadr files)
                             (read-file-name
                              "file: "
                              (dired-dwim-target-directory)))))
                (if (file-newer-than-file-p file1 file2)
                    (ediff-files file2 file1)
                  (ediff-files file1 file2))
                (add-hook 'ediff-after-quit-hook-internal
                          (lambda ()
                            (setq ediff-after-quit-hook-internal nil)
                            (set-window-configuration wnd))))
            (error "no more than 2 files should be marked"))))

      (define-key dired-mode-map "e" 'ora-ediff-files)
      ;; TODO: have no effect.
      (define-key dired-mode-map (kbd "C-p") 'dired-previous-line)

      (defvar dired-filelist-cmd
        '(("vlc" "-L")))

      )))

;; TODO: have no effect.
(defun adispring/post-init-dired-mode ()
  (with-eval-after-load 'dired-mode
    (define-key dired-mode-map (kbd "C-p") 'dired-previous-line)
    ))

(defun adispring/init-youdao-dictionary ()
  (use-package youdao-dictionary
    :defer t
    :init
    (define-key global-map (kbd "C-c y") 'youdao-dictionary-search-at-point+)
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

(defun adispring/init-nodejs-repl ()
  (use-package nodejs-repl
    :init
    :defer t))

(defun adispring/init-web-search ()
  (use-package web-search
    :defer t
    :init
    (define-key global-map (kbd "C-c C-v") 'web-search)
    ))

(defun adispring/post-init-web-mode ()
  (progn
    (add-to-list 'auto-mode-alist '("\\.erb\\'"    . web-mode))       ;; ERB
    (add-to-list 'auto-mode-alist '("\\.html?\\'"  . web-mode))       ;; Plain HTML
    (add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))       ;; JS + JSX
    (add-to-list 'auto-mode-alist '("\\.es6\\'"    . web-mode))       ;; ES6
    ;; (add-to-list 'auto-mode-alist '("\\.css\\'"    . web-mode))       ;; CSS
    ;; (add-to-list 'auto-mode-alist '("\\.scss\\'"   . web-mode))       ;; SCSS
    (add-to-list 'auto-mode-alist '("\\.php\\'"   . web-mode))        ;; PHP
    (add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))  ;; Blade template

    (add-hook 'web-mode-hook 'adi-web-mode-indent-setup)

    (add-hook 'web-mode-hook 'adi-js-imenu-setup)
    (add-hook 'web-mode-hook #'(lambda ()
                                 (enable-minor-mode
                                  '("\\.jsx?\\'" . prettier-js-mode)
                                  )))

    (add-hook 'js2-mode-hook 'tern-mode)
    (add-hook 'web-mode-hook 'tern-mode)


    ;; (add-hook 'web-mode-hook 'company-tern)
    ;; (add-hook 'web-mode-hook #'(lambda ()
    ;;                              (enable-minor-mode
    ;;                               '("\\.css\\'" . prettier-js-mode)
    ;;                               )))


    (defadvice web-mode-highlight-part (around tweak-jsx activate)
      (if (equal web-mode-content-type "jsx")
          (let ((web-mode-enable-part-face nil))
            ad-do-it)
        ad-do-it))

    (defadvice web-mode-highlight-part (around tweak-jsx activate)
      (if (equal web-mode-content-type "js")
          (let ((web-mode-enable-part-face nil))
            ad-do-it)
        ad-do-it))

    (setq web-mode-engines-alist
          '(("blade"  . "\\.blade\\.")))
    (setq web-mode-enable-auto-pairing t)
    (setq web-mode-enable-css-colorization t)

    (with-eval-after-load "web-mode"
      (web-mode-toggle-current-element-highlight)
      (web-mode-dom-errors-show))

    (setq company-backends-web-mode '((company-tern ;; auto display js module apis
                                       company-css
                                       )
                                      company-css
                                      company-files
                                      ))

    (setq company-backends-web-mode-raw '((company-tern ;; auto display js module apis
                                       company-css
                                       )
                                      company-css
                                      company-files
                                      ))

    (add-hook 'web-mode-hook
              (lambda ()
                (set (make-local-variable 'company-backends)
                     '((company-tern company-web-html company-yasnippet)))))

    (setq company-backends '((company-tern ;; auto display js module apis
                                       company-css
                                       )
                                      company-css
                                      company-files
                                      ))

    )
  )

(defun adispring/init-flycheck-package ()
  (use-package flycheck))

(defun adispring/post-init-flycheck ()
  ;; disable jshint since we prefer eslint checking
  (progn
    (with-eval-after-load 'flycheck
      (setq-default flycheck-disabled-checkers
                    (append flycheck-disabled-checkers
                            '(javascript-jshint)))
      ;; use eslint with web-mode for jsx files
      ;; (flycheck-add-mode 'javascript-eslint 'web-mode)
      (flycheck-add-mode 'javascript-standard 'web-mode)
      )
    ;; (add-hook 'web-mode-hook #'adi/web-use-eslint-from-node-modules)
    (add-hook 'web-mode-hook #'adi/web-use-standard-from-node-modules)
    ;; (spacemacs/add-flycheck-hook 'web-mode)
    )
  )


;;; packages.el ends here
