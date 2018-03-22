(electric-pair-mode t)
;; https://www.reddit.com/r/emacs/comments/4xhxfw/how_to_tune_the_behavior_of_eletricpairmode/
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(show-paren-mode t)

(when ADI-ONLY?
  (setq org-agenda-files (list "~/org_work/work.org"
                               "~/org/learn.org"
                               "~/org/2018.org"
                               "~/org/private.org"
                               "~/org/schedule.org"
                               ))
  )

(setq ivy-use-virtual-buffers t)

;; (add-hook 'edit-server-done-hook (lambda () (shell-command "open -a \"Google Chrome\"")))

;; haskell setting
(setq-default
 ;; hoogle in emacs instead of browser
 haskell-hoogle-command t
 )

;; web setting
(setq web-mode-content-types-alist
      '(("jsx" . "\\.js[x]?\\'")
        ("javascript" . "\\.es6?\\'")))

(defadvice js-jsx-indent-line (after js-jsx-indent-line-after-hack activate)
  "Workaround sgml-mode and follow airbnb component style."
  (save-excursion
    (beginning-of-line)
    (if (looking-at-p "^ +\/?> *$")
        (delete-char sgml-basic-offset))))

(defun spacemacs/check-large-file ()
  (when (> (buffer-size) 500000)
    (progn (fundamental-mode)
           (hl-line-mode -1)))
  (if (and (executable-find "wc")
           (> (string-to-number (shell-command-to-string (format "wc -l %s" (buffer-file-name))))
              5000))
      (linum-mode -1)))

(add-hook 'find-file-hook 'spacemacs/check-large-file)

(setq prettier-js-args '(
                         "--single-quote" "true"
                         "--trailing-comma" "es5"
                         ))
