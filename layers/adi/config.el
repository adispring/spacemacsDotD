(electric-pair-mode t)
;; https://www.reddit.com/r/emacs/comments/4xhxfw/how_to_tune_the_behavior_of_eletricpairmode/
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(show-paren-mode t)

(when (file-directory-p "~/org/")
  (setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$")))

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

;; 在 spacemacs 中，这样设置 company-backends 才管用
;; (spacemacs|add-company-backends
;;   :backends company-tide
;;   :modes typescript-tsx-mode)
