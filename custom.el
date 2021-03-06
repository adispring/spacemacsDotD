;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(case-fold-search nil)
 '(clean-aindent-mode t)
 '(company-auto-commit-chars nil)
 '(company-auto-complete-chars nil)
 '(custom-safe-themes
   '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(electric-pair-open-newline-between-pairs nil)
 '(evil-want-Y-yank-to-eol nil)
 '(grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules"))
 '(helm-ag-insert-at-point 'thing-at-point)
 '(helm-ag-use-agignore t)
 '(helm-ag-use-grep-ignore-list t t)
 '(helm-ag-use-temp-buffer nil)
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f")))
 '(indent-tabs-mode nil)
 '(js-indent-level 2)
 '(js2-strict-trailing-comma-warning nil)
 '(lsp-enable-file-watchers nil)
 '(lsp-headerline-breadcrumb-enable nil)
 '(magit-commit-arguments nil)
 '(magit-diff-arguments '("-- src"))
 '(markdown-fontify-code-blocks-natively t)
 '(org-agenda-files '("~/org_work/work.org"))
 '(package-selected-packages
   '(cfrs posframe company-tabnine unicode-escape sphinx-doc treemacs-icons-dired org-superstar org-rich-yank lsp-pyright lsp-origami origami lsp-ivy ivy-avy emr clang-format list-utils vue-mode edit-indirect ssass-mode vue-html-mode dired-filter dired-narrow dired-hacks-utils diredfl ivy-yasnippet ivy-xref ivy-purpose counsel-css company-quickhelp-terminal company-quickhelp git-gutter keyfreq find-file-in-project utop tuareg caml ocp-indent ob-elixir mvn meghanada maven-test-mode lsp-java groovy-mode groovy-imports pcache gradle-mode git-gutter-fringe+ fringe-helper git-gutter+ flycheck-ocaml merlin flycheck-mix flycheck-credo emojify emoji-cheat-sheet-plus dune company-emoji browse-at-remote alchemist elixir-mode peep-dired js-react-redux-yasnippets vlf vterm treemacs-persp treemacs-magit terminal-here symbol-overlay rubocopfmt org-cliplink magit-section lsp-treemacs lsp-python-ms lsp-haskell helm-org helm-lsp lv helm-ls-git flycheck-package package-lint flycheck-elsa evil-textobj-line devdocs blacken attrap hybrid-mode lsp-ui company-lsp dap-mode bui tree-mode toml-mode racer flycheck-rust cargo rust-mode rainbow-mode writeroom-mode visual-fill-column treemacs-projectile treemacs pfuture transient lsp-go ht godoctor go-tag go-rename go-impl go-guru go-gen-test go-fill-struct go-eldoc flycheck-gometalinter flycheck-golangci-lint company-go go-mode sqlup-mode sql-indent thrift geiser stickyfunc-enhance pippel pipenv lsp-python lsp-mode importmagic epc ctable concurrent deferred helm-gtags helm-cscope xcscope ggtags counsel-gtags yasnippet-snippets spaceline-all-the-icons seeing-is-believing ruby-refactor ruby-hash-syntax overseer magit-svn json-navigator hierarchy helm-xref gitignore-templates treepy graphql evil-goggles evil-cleverparens all-the-icons memoize centered-cursor-mode dotenv-mode nameless font-lock+ helm-org-rifle helm-git-grep doom-modeline eldoc-eval shrink-path prettier-standard-js livedown org-mime hungry-delete ghub let-alist lcr prettier-js wgrep ivy-hydra counsel-projectile auctex-latexmk company-auctex auctex org2ctex hexo rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv rake minitest chruby bundler inf-ruby web-search ranger youdao-dictionary names chinese-word-at-point symon string-inflection password-generator org-brain helm-purpose window-purpose imenu-list evil-org evil-lion editorconfig easy-kill counsel swiper ivy impatient-mode tern-auto-complete dante ac-js2 org-category-capture company-cabal company-anaconda tide typescript-mode racket-mode faceup window-numbering reveal-in-osx-finder pbcopy osx-trash osx-dictionary launchctl nodejs-repl gmail-message-mode ham-mode html-to-markdown flymd edit-server intero hlint-refactor hindent helm-hoogle haskell-snippets flycheck-haskell company-ghci company-ghc ghc haskell-mode cmm-mode org-projectile org-present org-pomodoro alert log4e gntp org-download htmlize gnuplot yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode helm-pydoc cython-mode anaconda-mode pythonic smex paredit rjsx-mode unfill mwim react-snippets yaml-mode mmm-mode markdown-toc markdown-mode gh-md web-mode tagedit slim-mode scss-mode sass-mode pug-mode less-css-mode helm-css-scss haml-mode emmet-mode company-web web-completion-data xterm-color shell-pop multi-term flycheck-pos-tip pos-tip flycheck eshell-z eshell-prompt-extras esh-help helm-company helm-c-yasnippet fuzzy company-tern dash-functional tern company-statistics company auto-yasnippet ac-ispell auto-complete smeargle orgit magit-gitflow helm-gitignore gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link evil-magit magit magit-popup git-commit with-editor vimrc-mode dactyl-mode web-beautify livid-mode skewer-mode simple-httpd json-mode json-snatcher json-reformat js2-refactor yasnippet multiple-cursors js2-mode js-doc coffee-mode ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint info+ indent-guide hydra hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump f s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed dash aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async))
 '(pdf-view-midnight-colors '("#b2b2b2" . "#292b2e"))
 '(prettier-js-show-errors nil)
 '(projectile-globally-ignored-directories
   '(".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "lib"))
 '(rust-format-on-save t)
 '(scheme-program-name "chez" t)
 '(smex-save-file "~/.spacemacs.d/smex-items")
 '(tab-always-indent 'complete)
 '(vlf-application 'dont-ask)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-markup-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
