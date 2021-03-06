;; copy & paste
(defun copy-to-clipboard ()
  "Copies selection to x-clipboard."
  (interactive)
  (if (display-graphic-p)
      (progn
        (message "Yanked region to x-clipboard!")
        (call-interactively 'clipboard-kill-ring-save)
        )
    (if (region-active-p)
        (progn
          (shell-command-on-region (region-beginning) (region-end) "pbcopy")
          (message "Yanked region to clipboard!")
          (deactivate-mark))
      (message "No region active; can't yank to clipboard!")))
  )

(defun paste-from-clipboard ()
  "Pastes from x-clipboard."
  (interactive)
  (if (display-graphic-p)
      (progn
        (clipboard-yank)
        (message "graphics active")
        )
    (insert (shell-command-to-string "pbpaste"))
    )
  )
(evil-leader/set-key "o y" 'copy-to-clipboard)
(evil-leader/set-key "o p" 'paste-from-clipboard)

(defun occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history)
  (deactivate-mark)
  (call-interactively 'occur))

(defun adi-web-mode-indent-setup (indent-size)
  ;; web development
  (setq typescript-indent-level indent-size)
  (setq css-indent-offset  indent-size)
  (setq web-mode-markup-indent-offset indent-size)
  (setq web-mode-css-indent-offset indent-size)
  (setq web-mode-code-indent-offset indent-size)
  )

(defun adi/set-web-indent (indent-size)
  "Set web indent size"
  (interactive "nIndent size: ")
  (adi-web-mode-indent-setup indent-size))

;; (setq javascript-common-imenu-regex-list
;;       '(("Attribute" " \\([a-z][a-zA-Z0-9-_]+\\) *= *\{[a-zA-Z0-9_.(), ]+\}\\( \\|$\\)" 1)
;;         ("Controller" "[. \t]controller([ \t]*['\"]\\([^'\"]+\\)" 1)
;;         ("Controller" "[. \t]controllerAs:[ \t]*['\"]\\([^'\"]+\\)" 1)
;;         ("Filter" "[. \t]filter([ \t]*['\"]\\([^'\"]+\\)" 1)
;;         ("State" "[. \t]state[(:][ \t]*['\"]\\([^'\"]+\\)" 1)
;;         ("Factory" "[. \t]factory([ \t]*['\"]\\([^'\"]+\\)" 1)
;;         ("Service" "[. \t]service([ \t]*['\"]\\([^'\"]+\\)" 1)
;;         ("Module" "[. \t]module( *['\"]\\([a-zA-Z0-9_.]+\\)['\"], *\\[" 1)
;;         ("ngRoute" "[. \t]when(\\(['\"][a-zA-Z0-9_\/]+['\"]\\)" 1)
;;         ("Directive" "[. \t]directive([ \t]*['\"]\\([^'\"]+\\)" 1)
;;         ("Event" "[. \t]\$on([ \t]*['\"]\\([^'\"]+\\)" 1)
;;         ("Config" "[. \t]config([ \t]*function *( *\\([^\)]+\\)" 1)
;;         ("Config" "[. \t]config([ \t]*\\[ *['\"]\\([^'\"]+\\)" 1)
;;         ("OnChange" "[ \t]*\$(['\"]\\([^'\"]*\\)['\"]).*\.change *( *function" 1)
;;         ("OnClick" "[ \t]*\$([ \t]*['\"]\\([^'\"]*\\)['\"]).*\.click *( *function" 1)
;;         ("Watch" "[. \t]\$watch( *['\"]\\([^'\"]+\\)" 1)
;;         ("Function" "function[ \t]+\\([a-zA-Z0-9_$.]+\\)[ \t]*(" 1)
;;         ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function[ \t]*(" 1)
;;         ;; {{ es6 beginning
;;         ("Function" "^[ \t]*\\([A-Za-z_$][A-Za-z0-9_$]+\\)[ \t]*([a-zA-Z0-9, ]*) *\{ *$" 1) ;; es6 fn1 () { }
;;         ("Function" "^[ \t]*\\([A-Za-z_$][A-Za-z0-9_$]+\\)[ \t]*=[ \t]*(?[a-zA-Z0-9, ]*)?[ \t]*=>" 1) ;; es6 fn1 = (e) =>
;;         ;; }}
;;         ("Task" "[. \t]task([ \t]*['\"]\\([^'\"]+\\)" 1)
;;         ))

;; js-mode imenu enhancement
;; @see http://stackoverflow.com/questions/20863386/idomenu-not-working-in-javascript-mode
;; (defun adi-js-imenu-make-index ()
;;   (save-excursion
;;     (imenu--generic-function javascript-common-imenu-regex-list)))

;; (defun adi-js-imenu-setup ()
;;   (setq imenu-create-index-function 'adi-js-imenu-make-index)
;;   )

;; Have no effect when using Emacs in terminal
(defun adi/insert-chrome-current-tab-url()
  "Get the URL of the active tab of the first window"
  (interactive)
  (insert (adi/retrieve-chrome-current-tab-url)))

(defun adi/retrieve-chrome-current-tab-url()
  "Get the URL of the active tab of the first window"
  (interactive)
  (let ((result (do-applescript
                 (concat
                  "set frontmostApplication to path to frontmost application\n"
                  "tell application \"Google Chrome\"\n"
                  "	set theUrl to get URL of active tab of first window\n"
                  "	set theResult to (get theUrl) \n"
                  "end tell\n"
                  "activate application (frontmostApplication as text)\n"
                  "set links to {}\n"
                  "copy theResult to the end of links\n"
                  "return links as string\n"))))
    (format "%s" (s-chop-suffix "\"" (s-chop-prefix "\"" result)))))

(defun adi/show-in-finder ()
  (interactive)
  (shell-command "open -R ."))

(defun ivy-with-thing-at-point (cmd)
  (let ((ivy-initial-inputs-alist
         (list
          (cons cmd (thing-at-point 'symbol)))))
    (funcall cmd)))

;; Example 1
(defun counsel-ag-thing-at-point ()
  (interactive)
  (ivy-with-thing-at-point 'counsel-ag))

(defun counsel-git-grep-thing-at-point ()
  (interactive)
  (ivy-with-thing-at-point 'counsel-git-grep))

;; Example 2
(defun swiper-thing-at-point ()
  (interactive)
  (ivy-with-thing-at-point 'swiper))

(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
          (funcall (cdr my-pair)))))

;;;_*======================================================================
;;;_* define a function to scroll with the cursor in place, moving the
;;;_* page instead
;; Navigation Functions
(defun adi/scroll-down-in-place (n)
  (interactive "p")
  (previous-line n)
  (unless (eq (window-start) (point-min))
    (scroll-down n)))

(defun adi/scroll-up-in-place (n)
  (interactive "p")
  (next-line n)
  (unless (eq (window-end) (point-max))
    (scroll-up n)))

(defun adi/web-use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (global-eslint (executable-find "eslint"))
         (local-eslint (expand-file-name "node_modules/.bin/eslint"
                                         root))
         (eslint (if (file-executable-p local-eslint)
                     local-eslint
                   global-eslint)))
    (setq-local flycheck-javascript-eslint-executable eslint)))

(defun adi/web-use-standard-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (global-standard (executable-find "standard"))
         (local-standard (expand-file-name "node_modules/.bin/standard"
                                         root))
         (standard (if (file-executable-p local-standard)
                     local-standard
                   global-standard)))
    (setq-local flycheck-javascript-standard-executable standard)))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  ;; (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  )

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

(defun css-imenu-make-index ()
  (save-excursion
    (imenu--generic-function '((nil "^ *\\([^ ]+\\) *{ *$" 1)))))
