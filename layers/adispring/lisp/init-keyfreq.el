(defun turnon-keyfreq-mode ()
  (interactive)
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(defun turnoff-keyfreq-mode ()
  (interactive)
  (keyfreq-mode -1)
  (keyfreq-autosave-mode -1))

(defun init-keyfreq-excluded-commands ()
  (setq keyfreq-excluded-commands
        '(self-insert-command
          helm-next-line
          ivy-previous-line
          forward-char
          backward-char
          previous-line
          next-line
          )))

(provide 'init-keyfreq)
