(electric-pair-mode t)
;; https://www.reddit.com/r/emacs/comments/4xhxfw/how_to_tune_the_behavior_of_eletricpairmode/
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(setq electric-pair-pairs '( (?\" . ?\") (?\` . ?\`) (?\( . ?\)) (?\{ . ?\}) (?\' . ?\') ))
(show-paren-mode t)

(setq org-agenda-files (list "~/org/work.org"
                             "~/org/learn.org"))

(setq ivy-use-virtual-buffers t)

;; (add-hook 'edit-server-done-hook (lambda () (shell-command "open -a \"Google Chrome\"")))

;; haskell setting
(setq-default
 ;; hoogle in emacs instead of browser
 haskell-hoogle-command t
 )

;; web setting
(add-to-list 'auto-mode-alist '("\\.erb\\'"    . web-mode))       ;; ERB
(add-to-list 'auto-mode-alist '("\\.html?\\'"  . web-mode))       ;; Plain HTML
(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))       ;; JS + JSX
(add-to-list 'auto-mode-alist '("\\.es6\\'"    . web-mode))       ;; ES6
(add-to-list 'auto-mode-alist '("\\.css\\'"    . web-mode))       ;; CSS
(add-to-list 'auto-mode-alist '("\\.scss\\'"   . web-mode))       ;; SCSS
(add-to-list 'auto-mode-alist '("\\.php\\'"   . web-mode))        ;; PHP
(add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))  ;; Blade template
(setq web-mode-content-types-alist
      '(("jsx" . "\\.js[x]?\\'")
        ("javascript" . "\\.es6?\\'")))

(setq web-mode-engines-alist
      '(("blade"  . "\\.blade\\.")))

(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

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

(setq web-mode-enable-auto-pairing t)

(setq web-mode-enable-css-colorization t)

;; disable jshint since we prefer eslint checking
(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  ;; use eslint with web-mode for jsx files
  (flycheck-add-mode 'javascript-eslint 'web-mode))

;; M . : jump to definitions
(setq ac-js2-evaluate-calls t)

(add-hook 'web-mode-hook (lambda () (tern-mode t)))
;; (eval-after-load 'tern
;;   '(progn
;;      (require 'tern-auto-complete)
;;      (tern-ac-setup)))

(with-eval-after-load "web-mode"
  (web-mode-toggle-current-element-highlight)
  (web-mode-dom-errors-show))

(setq company-backends-web-mode '((company-tern ;; auto display js module apis
                                   ;; company-dabbrev-code
                                   ;; company-keywords
                                   ;; company-etags
                                   )
                                  company-files
                                  ;; company-dabbrev
                                  ))
