(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
;; (tooltip mode -1)
(menu-bar-mode -1)

;; cycle spacing blank characters
(global-set-key (kbd "M-SPC") 'cycle-spacing)
;; delete blank lines - by default it is bound to C-x C-o
(global-set-key (kbd "M-o") 'delete-blank-lines)

(use-package ace-window
  :ensure t
  )

(global-set-key (kbd "C-x o") 'ace-window)

(global-set-key (kbd "C-.") #'other-window)
(global-set-key (kbd "C-,") #'prev-window)

(defun prev-window ()
  (interactive)
  (other-window -1))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;; elfeed configs
;; set default elfeed db directory
(use-package elfeed
  :ensure t
  :config
  (setq elfeed-feeds
	'(("https://www.reddit.com/r/emacs/new.rss" emacs)
	  ("https://hunterx-hunter.com/feed" manga hunter)
	  ("https://www.reddit.com/r/planetemacs/new.rss" emacs)
	  ("https://www.reddit.com/r/django/new.rss" python django)
	  ("https://www.reddit.com/r/archlinux/new.rss" linux arch)
	  ("https://news.ycombinator.com/rss" news)
	  ))
  )
(global-set-key (kbd "C-x w") 'elfeed)

;; checkout elfeed-org and other related packages


(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode 1)
  (setq projectile-enable-caching t)
  (setq projectile-sort-order 'recently-active)
  (setq projectile-completion-system 'ivy)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/devc/")
    (setq projectile-project-search-path '("~/devc/")))
  (setq projectile-switch-project-action #'projectile-dired))

(global-hl-line-mode +1)
;; (add-hook 'prog-mode-hook 'linum-mode)
(delete-selection-mode 1)

(setq backup-directory-alist '(("." . "~/custom/.saves")))

(display-line-numbers-mode 1)
(setq display-line-numbers 'relative)

(show-paren-mode 1)

;; (setq ido-everywhere t)
;; (setq ido-enable-flex-matching t)
;; (ido-mode t)


(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region)
  ("C--" . er/expands-region))

(use-package company
  :diminish company-mode
  :ensure t
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-common-or-cycle))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 1))
;; check if company can use tab to cycle and insert the completion as well at the same time...

(add-hook 'after-init-hook 'global-company-mode)

(use-package company-box
  :ensure t
  :diminish company-box
  :hook (company-mode . company-box-mode))


;; (use-package modus-themes
;;   :ensure t
;;   :init
;;   ;; Add all your customizations prior to loading the themes
;;   (setq modus-themes-slanted-constructs t
;; ;;	set-face-attribute 'cursor nil :background (modus-themes-color-alts 'blue 'blue)
;; 	modus-themes-mode-line '2d
;;         modus-themes-bold-constructs nil)
;;   ;;:config
;;   ;; Load the theme of your choice
;;   ;; (modus-themes-load-operandi)
;;   ;; ;; OR
;;   (load-theme 'modus-vivendi t)
;;   :bind ("<f5>" . modus-themes-toggle))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-vibrant t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
  
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)

;; add this into above config of modus themes
;; (set-face-attribute 'cursor nil :background (modus-themes-color-alts 'blue 'blue))

(use-package swiper
  :ensure t
  :bind ("C-c s" . swiper))

(use-package ivy
  :ensure t
  :diminish
  :bind (("C-c s" . swiper)
	 :map ivy-minibuffer-map
	 ;;("TAB" . ivy-alt-done)
	 ("C-L" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1)
  (setq ivy-display-style 'fancy)
  (setq enable-recursive-minibuffers t)
  (setq ivy-use-selectable-prompt t)
  (setq ivy-use-virtual-buffers t))

;; ibuffer configs
;;(setq ibuffer-display-summary nil)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; line number toggle for current buffer
(global-set-key [f7] 'display-line-numbers-mode)

;; ibuffer projectile
(use-package ibuffer-projectile
  :ensure t
  :after (projectile)
  :hook ibuffer . (lambda ()
		    (ibuffer-projectile-set-filter-groups)
		    (unless (eq ibuffer-sorting-mode 'recency)
		      (ibuffer-do-sort-by-recency))))


(use-package ivy-rich
  :ensure t
  :init
  (ivy-rich-mode 1))

;; change the position of window in the frame
(use-package ivy-posframe
  :ensure t
  :delight
  :custom
  (ivy-posframe-height-alist
   '((swiper . 15)
     (t . 10)))
  (ivy-posframe-display-functions-alist
   '((complete-symbol . ivy-posframe-display-at-point)
     (counsel-describe-function . nil)
     (counsel-describe-variable . nil)
     (swiper . nil)
     (swiper-isearch . nil)
     (t . ivy-posframe-display-at-frame-center)))
  :config
  (ivy-posframe-mode -1))

(use-package counsel
  :ensure t
  :bind (("C-M-j" . 'counsel-switch-buffer)
	 ("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1))

(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package counsel-projectile
  :ensure t
  :config (counsel-projectile-mode))


(set-face-attribute 'default nil :font "Hack" :height 110)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Hack" :height 110)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Fira Sans Book" :height 120 :weight 'regular)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom-face (mode-line ((t (:height 0.95))))
  )

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
  :init (rainbow-delimiters-mode -1))

(use-package all-the-icons
  :ensure t) ;; M-x all-the-icons-install-fonts

(use-package magit
  :ensure t)

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq
   lsp-headerline-breadcrumb-enable nil
   lsp-enable-symbol-highlighting t
   lsp-lens-enable t
   lsp-ui-sideline-enable t
   lsp-modeline-code-actions-enable t
   lsp-ui-sideline-enable t
   lsp-modeline-diagnostics-enable t
   lsp-completion-show-kind t
   )
  (lsp-enable-which-key-integration t))

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred))


(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))

(use-package python-black
  :ensure t
  :after python
  :config
  (setq python-black-on-save-mode t))

;; https://github.com/wbolster/emacs-python-pytest
(use-package python-pytest
  :ensure t)


(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode 1))

(use-package yasnippet-snippets
  :ensure t)

;; vterm is not working check the documentation once.
;; https://github.com/akermu/emacs-libvterm
(use-package vterm
  :ensure t
  :load-path  "/home/mars/.emacs.d/elpa/vterm-20210108.132/build/")

;;(use-package all-the-icons
;;  :ensure t)

;; (use-package all-the-icons-ivy
;;   :ensure t
;;   :init (all-the-icons-ivy-setup))

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package ivy-prescient
  :ensure t
  :config
  (ivy-prescient-mode 1)
  (prescient-persist-mode 1))

(setq ivy-prescient-retain-classic-highlighting t)

(use-package company-prescient
  :ensure t
  :after company
  :config
  (company-prescient-mode 1))

;; probably same as prescient, check once
;; (use-package amx
;;   :ensure t
;;   :after ivy
;;   :custom
;;   (amx-backend 'auto)
;;   (amx-save-file "~/.emacs/amx-items")
;;   (amx-history-length 50)
;;   (amx-show-keybindings nil)
;;   :config
;;   (amx-mode 1))

(use-package powerline
  :ensure t)

(use-package eyebrowse
 :ensure t
 :diminish eyebrowse-mode
 :config (progn
           (eyebrowse-mode t)
           (setq eyebrowse-new-workspace t)))

(use-package minions
  :ensure t
  :config (minions-mode 1))

(use-package diminish
  :ensure t)

;; cursor changes
(blink-cursor-mode -1)

;; icomplete
;; (use-package icomplete-vertical
;;   :ensure t
;;   :demand t
;;   :custom
;;   (completion-styles '(partial-completion substring))
;;   (completion-category-overrides '((file (styles basic substring))))
;;   (read-file-name-completion-ignore-case t)
;;   (read-buffer-completion-ignore-case t)
;;   (completion-ignore-case t)
;;   :config
;;   (icomplete-mode -1)
;;   (icomplete-vertical-mode)
;;   :bind (:map icomplete-minibuffer-map
;;               ("<down>" . icomplete-forward-completions)
;;               ("C-n" . icomplete-forward-completions)
;;               ("<up>" . icomplete-backward-completions)
;;               ("C-p" . icomplete-backward-completions)
;;               ("C-v" . icomplete-vertical-toggle)))

(use-package orderless
  :ensure t
  :init (icomplete-mode) ; optional but recommended!
  :custom (completion-styles '(orderless)))

;; dap debugger config
;; read org mode to maintain todos and other things.
;; know more shortcuts in emacs for easy buffer movement.
;; autocompletion for filters in elfeed.
;; shortcut for eww.
;; web, javascript packages for web dev.
;; configure modeline to be more clean - currently using minion package for this.
;; read about workgroups2 so that it can be replaced with eyebrowse.
;; embark
;; check for the best way to maintain this config.
;; vterm is not working.
;; saving current workspace or desktop state - check for this package
;; imenu and imenu list
;; checkout below repos for code folding
  ;; https://github.com/gregsexton/origami.el/tree/e558710a975e8511b9386edc81cd6bdd0a5bda74
  ;; https://github.com/matsievskiysv/vimish-fold/tree/a6501cbfe3db791f9ca17fd986c7202a87f3adb8
  ;; https://github.com/emacsorphanage/yafolding/tree/4c1888ae45f9241516519ae0ae3a899f2efa05ba
;; add keymap toggle for commenting and uncommenting code block
;; run pytest tests from emacs
;; configure elfeed for easy access and play youtube video feeds, if not possible move to newsboat.
;; check if we can find the package or pyright settings for getting the list member variables in current python buffer
;; add a shortcut to configure vertical split into horizontal split with predefined height for it
;; relative line number config is not working as expected.
;; magit and dired configs
;; zen mode for coding.



;; (use-package workgroups2
;;   :ensure t
;;   :config
;;   (setq workgroups-mode 1
;; 	wg-mode-line-display-on t
;; 	wg-flag-modified t
;; 	wg-mode-line-decor-left-brace "["
;;         wg-mode-line-decor-right-brace "]"  ; how to surround it
;;         wg-mode-line-decor-divider ":"))

