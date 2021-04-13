(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package no-littering
  :init
  (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  )


;; Theme
(straight-use-package 'vscode-dark-plus-theme)
;;(load-theme 'vscode-dark-plus t)

;; respect tile size
(setq frame-resize-pixelwise t)

;; (global-set-key (kbd "<f7>") 'consult-outline)
;; (global-set-key [C-tab] 'consult-buffer)
;; (global-set-key (kbd "C-x C-r") 'consult-recent-file)

;; referred from https://github.com/ashton314/.dotfiles/blob/master/.emacs

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (("C-x M-:" . consult-complex-command)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-keep-lines)
         ("C-c C-k" . consult-flush-lines)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r x" . consult-register)
         ("C-x r b" . consult-bookmark)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)       ;; "M-s o" is a good alternative.
         ("M-g l" . consult-line)          ;; "M-s l" is a good alternative.
         ("M-g m" . consult-mark)          ;; I recommend to bind Consult navigation
         ("M-g k" . consult-global-mark)   ;; commands under the "M-g" prefix.
         ("M-g r" . consult-git-grep)      ;; or consult-grep, consult-ripgrep
	 ("C-c r" . consult-ripgrep)
         ("M-g f" . consult-find)          ;; or consult-locate, my-fdfind
         ("M-g i" . consult-project-imenu) ;; or consult-imenu
         ("M-g e" . consult-error)
         ("M-s m" . consult-multi-occur)
         ("M-y" . consult-yank-pop)
         ("<help> a" . consult-apropos))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Custom command wrappers. It is generally encouraged to write your own
  ;; commands based on the Consult commands. Some commands have arguments which
  ;; allow tweaking. Furthermore global configuration variables can be set
  ;; locally in a let-binding.
  (defun my-fdfind (&optional dir)
    (interactive "P")
    (let ((consult-find-command '("fdfind" "--color=never" "--full-path")))
      (consult-find dir)))

  ;; Replace `multi-occur' with `consult-multi-occur', which is a drop-in replacement.
  (fset 'multi-occur #'consult-multi-occur)

  ;; Configure register preview function.
  ;; This gives a consistent display for both `consult-register' and
  ;; the register preview when editing registers.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config

  ;; Optionally configure narrowing key.
  (setq consult-narrow-key "<") ;; Another viable option: (kbd "C-+")

  ;; Optional configure a view library to be used by `consult-buffer'.
  ;; The view library must provide two functions, one to open the view by name,
  ;; and one function which must return a list of views as strings.
  ;; Example: https://github.com/minad/bookmark-view/
  ;; (setq consult-view-open-function #'bookmark-jump
  ;;       consult-view-list-function #'bookmark-view-names)

  ;; Optionally configure a function which returns the project root directory
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root))

(use-package marginalia
  :config
  (marginalia-mode)
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light)))


(use-package embark
  :bind
  (("C-S-a" . embark-act)	; ctrl-shift-a
   ("C-s-a" . embark-act)	; ctrl-super-a
   ("C-h B" . embark-bindings)) ; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))


;; Diminish
(use-package diminish)
(diminish 'eldoc-mode "")

;; Selectrum
(use-package selectrum
  :config
  (selectrum-mode +1))

(use-package selectrum-prescient
  :config
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

;; (use-package vscode-icon
;;   :commands (vscode-icon-for-file))

;; Company
(use-package company
  :defer t
  :diminish ""
  :config
  (global-company-mode +1)
  ;; For this, see https://github.com/jojojames/vscode-icon-emacs
  ;; (setq company-format-margin-function #'company-vscode-light-icons-margin)
  :bind (:map company-active-map
	      ("C-n" . 'company-select-next-or-abort)
	      ("C-j" . 'company-select-next-or-abort)
	      ("C-p" . 'company-select-previous-or-abort)
	      ("C-k" . 'company-select-previous-or-abort)))

(use-package company-prescient
  :config
  (company-prescient-mode +1))


(use-package company-box
  :diminish ""
  :hook (company-mode . company-box-mode))

(use-package ibuffer-projectile
  :after (projectile)
  :hook ibuffer . (lambda ()
		    (ibuffer-projectile-set-filter-groups)
		    (unless (eq ibuffer-sorting-mode 'recency)
		      (ibuffer-do-sort-by-recency))))

(use-package dockerfile-mode
  :defer t)
(use-package yaml-mode
  :defer t)
(use-package web-mode
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2))
(use-package json-mode
  :defer t)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  :config
  (setq
   lsp-headerline-breadcrumb-enable nil
   lsp-enable-symbol-highlighting t
   lsp-lens-enable nil
   lsp-ui-sideline-enable nil
   lsp-modeline-code-actions-enable nil
   lsp-modeline-diagnostics-enable nil
   lsp-completion-show-kind t
   )
  (setq lsp-signature-render-documentation nil)
  (setq lsp-enable-links nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-completion-enable-additional-text-edit nil)
  )

(use-package undo-tree)

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show nil)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

;; LSP goodies for python
(use-package lsp-pyright
  :hook
  (python-mode .
	       (lambda ()
		 (setq indent-tabs-mode nil)
		 (setq tab-width 4)
		 (setq python-indent-offset 4)
                 (require 'lsp-pyright)
                 (lsp-deferred))))


;; (require 'dap-python)

(use-package dap-mode
  :config
  (dap-ui-mode)
  (dap-ui-controls-mode 1)

  (require 'dap-lldb)
  (require 'dap-python)
  (require 'dap-gdb-lldb)
  ;; installs .extension/vscode
  )

(use-package python-black
  :after python
  :config
  (setq python-black-on-save-mode t))

;; https://github.com/wbolster/emacs-python-pytest
(use-package python-pytest)

(use-package pyvenv
  :config
  (pyvenv-mode 1))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package highlight-indent-guides
  :hook
  (python-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-responsive 'top))

;; Deft
(use-package deft
  :defer t
  :config
  (setq deft-extensions '("org" "md" "txt" "tex"))
  (setq deft-new-file-format "%Y-%m-%d")
  (setq deft-recursive t)
  (setq deft-use-filename-as-title t))

;; Ace, Avy
(use-package ace-window
  :config
  (setq aw-background nil)
  (ace-window-display-mode +1))

;; Multiple-cursors
(use-package multiple-cursors)
;; TODO: keybindings

;; Projectile
(use-package projectile
  :diminish ""
  :bind (("C-c p" . projectile-command-map))
  :config
  ;; (setq projectile-enable-caching t)
  ;; (setq projectile-sort-order 'recently-active)
  (setq projectile-completion-system 'default))


(use-package flycheck
  :hook (lsp-mode . flycheck-mode))

;; Searching/mass editing
(use-package counsel
  :defer t)
(use-package swiper
  :bind (("C-s" . 'swiper)))

(use-package counsel-projectile
  :defer t)

(use-package wgrep
  :defer t)

(use-package which-key
  :diminish ""
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 1))

(use-package magit
  :defer t
  :bind (("C-x g" . 'magit-status)
	 ("s-g" . 'magit-status)))

(use-package diff-hl
  :diminish ""
  :config
  (global-diff-hl-mode)
  )
(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(use-package git-timemachine
  :defer t)

;; Programming
(use-package smartparens
  :diminish ""
  :config
  (smartparens-global-mode +1))

(use-package racket-mode
  :defer t)

;; (use-package vterm)

;; Writing
(use-package olivetti)


;; Searching
;;(define-key global-map (kbd "C-c C-r") 'counsel-rg)
;;(define-key global-map (kbd "C-c r") 'counsel-projectile-rg-subdir)

;; Ace window
(define-key global-map (kbd "M-o") 'ace-window)

;; Avy
(define-key global-map (kbd "C-c j") 'avy-goto-line)
(define-key global-map (kbd "C-M-j") 'avy-goto-char)
(define-key global-map (kbd "C-c J") 'avy-goto-word-0)
(define-key global-map (kbd "s-j") 'avy-goto-char)
(define-key global-map (kbd "s-J") 'avy-goto-line)
(define-key global-map (kbd "C-s-j") 'avy-goto-word-0)

(straight-use-package 'orderless)
;; (straight-use-package 'selectrum)
;; (straight-use-package 'selectrum-prescient)
;; (straight-use-package 'consult)
;; (straight-use-package 'modus-themes)
;; (straight-use-package 'marginalia)
;; (straight-use-package 'embark)
;; (straight-use-package 'embark-consult)
;; (straight-use-package 'lsp-mode)
;; (straight-use-package 'lsp-ui)
;; (straight-use-package 'flycheck)
;; (straight-use-package 'company-mode)
(straight-use-package 'treemacs)
(straight-use-package 'lsp-treemacs)
;; (straight-use-package 'dap-mode)
;; (straight-use-package 'lsp-pyright)
;; (straight-use-package 'company)


;; elfeed configs
;; set default elfeed db direcntory
(use-package elfeed
  :config
  (setq elfeed-feeds
	'(("https://www.reddit.com/r/emacs/new.rss" emacs)
	  ("https://200ok.ch/atom.xml" emacs)
	  ("https://www.reddit.com/r/planetemacs/new.rss" emacs)
	  ("https://www.reddit.com/r/django/new.rss" python django)
	  ("https://www.reddit.com/r/archlinux/new.rss" linux arch)
	  ("https://news.ycombinator.com/rss" news)
	  ))
  )
(global-set-key (kbd "C-x w") 'elfeed)

(use-package company
  :diminish
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  ;; :bind (:map company-active-map
  ;; 	 ("<tab>" . company-complete-common-or-cycle))
  ;; 	(:map lsp-mode-map
  ;; 	 ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 1))

(add-hook 'after-init-hook 'global-company-mode)



;; font settings
;; (add-to-list 'default-frame-alist
;; 	     '(font . "Hack Nerd Font-11"))

(set-face-attribute 'default nil :font "Iosevka" :height 115)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Hack" :height 110)
(set-face-attribute 'mode-line nil :font "Iosevka" :height 110)
(set-face-attribute 'mode-line-inactive nil :font "Iosevka" :height 110)

;; cycle spacing blank characters
(global-set-key (kbd "M-SPC") 'cycle-spacing)
;; delete blank lines - by default it is bound to C-x C-o
(global-set-key (kbd "M-o") 'delete-blank-lines)

(require 'orderless)
(setq completion-styles '(orderless))
(selectrum-mode +1)
(selectrum-prescient-mode +1)
(prescient-persist-mode +1)
;; check docs for consult package
;;(require 'consult)

;; config for marginalia and embark
;; (require 'marginalia)
;; (marginalia-mode)

(advice-add #'marginalia-cycle :after
	    (lambda () when (bound-and-true-p selectrum-mode) (selectrum-exhibit
							       'keep-selected)))
;; (require 'embark)
;; (global-set-key (kbd "C-S-a") 'embark-act)

;; (require 'embark-consult)
;; (add-hook 'embark-collect-mode 'embark-consult-preview-minor-mode)



(use-package modus-themes
  :init
  (setq modus-themes-slanted-constructs t
        modus-themes-bold-constructs nil
        modus-themes-fringes nil ; {nil,'subtle,'intense}
        ;; Options for `modus-themes-lang-checkers': nil,
        ;; 'straight-underline, 'subtle-foreground,
        ;; 'subtle-foreground-straight-underline, 'intense-foreground,
        ;; 'intense-foreground-straight-underline, 'colored-background
        modus-themes-lang-checkers 'straight-underline
        modus-themes-mode-line 'borderless
        ;; Options for `modus-themes-syntax': nil, 'faint,
        ;; 'yellow-comments, 'green-strings,
        ;; 'yellow-comments-green-strings, 'alt-syntax,
        ;; 'alt-syntax-yellow-comments
        modus-themes-syntax 'alt-syntax
        modus-themes-intense-hl-line 'underline-accented
        modus-themes-paren-match 'subtle-bold ; {nil,'subtle-bold,'intense,'intense-bold}
        ;; Options for `modus-themes-links': nil, 'faint,
        ;; 'neutral-underline, 'faint-neutral-underline, 'no-underline,
        ;; 'underline-only
        modus-themes-links 'underline-only
        modus-themes-no-mixed-fonts nil
        modus-themes-prompts 'intense ; {nil,'subtle,'intense}
        modus-themes-completions 'opinionated ; {nil,'moderate,'opinionated}
        modus-themes-region 'bg-only ; {nil,'no-extend,'bg-only,'bg-only-no-extend}
        modus-themes-diffs 'fg-only ; {nil,'desaturated,'fg-only,'bg-only}
        modus-themes-org-blocks 'rainbow ; {nil,'grayscale,'rainbow}
        modus-themes-org-habit 'traffic-light ; {nil,'simplified,'traffic-light}
        modus-themes-variable-pitch-ui nil
        modus-themes-variable-pitch-headings nil
        modus-themes-scale-headings nil
        modus-themes-scale-1 1.1
        modus-themes-scale-2 1.15
        modus-themes-scale-3 1.21
        modus-themes-scale-4 1.27
        modus-themes-scale-5 1.33)
  (load-theme 'modus-vivendi t)
  :config
  (set-face-attribute 'cursor nil :background (modus-themes-color-alts 'blue 'blue))  
  :bind ("<f5>" . modus-themes-toggle))




(use-package rustic
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  (setq lsp-eldoc-hook nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm
  (setq-local buffer-save-without-query t))

(use-package yasnippet
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

;; todo

;; hydra
;; session restore
;; dap mode
;; tramp
;; workspaces

