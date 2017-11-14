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
    youdao-dictionary
    flycheck
    web-mode
    company
    nodejs-repl
    ac-js2
    )
  )

(defun adispring/init-youdao-dictionary ()
  (use-package youdao-dictionary
    :defer t
    :init
    (define-key global-map (kbd "C-c y") 'youdao-dictionary-search-at-point+)
    )
  )

(defun adispring/init-flycheck-package ()
  (use-package flycheck-package))

(defun adispring/post-init-flycheck ()
  ;; disable jshint since we prefer eslint checking
  (with-eval-after-load 'flycheck
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(javascript-jshint)))
    ;; use eslint with web-mode for jsx files
    (flycheck-add-mode 'javascript-eslint 'web-mode))
  )

(defun adispring/post-init-company ()
  (use-package company
    :init
    :defer t))

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

(defun adispring/post-init-web-mode ()
  (progn
    (add-to-list 'auto-mode-alist '("\\.erb\\'"    . web-mode))       ;; ERB
    (add-to-list 'auto-mode-alist '("\\.html?\\'"  . web-mode))       ;; Plain HTML
    (add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))       ;; JS + JSX
    (add-to-list 'auto-mode-alist '("\\.es6\\'"    . web-mode))       ;; ES6
    (add-to-list 'auto-mode-alist '("\\.css\\'"    . web-mode))       ;; CSS
    (add-to-list 'auto-mode-alist '("\\.scss\\'"   . web-mode))       ;; SCSS
    (add-to-list 'auto-mode-alist '("\\.php\\'"   . web-mode))        ;; PHP
    (add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))  ;; Blade template

    (add-hook 'web-mode-hook 'adi-web-mode-indent-setup)
    (add-hook 'web-mode-hook (lambda () (tern-mode t)))

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
                                       )
                                      company-files
                                      ))
    )
  )

;;; packages.el ends here
