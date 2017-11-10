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

