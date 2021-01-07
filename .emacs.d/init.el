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

;;(set-frame-font "Hack 11" nil t)

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

;;(global-hl-line-mode +1)

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
  (company-idle-delay 0.0))
;; check if company can use tab to cycle and insert the completion as well at the same time...

(add-hook 'after-init-hook 'global-company-mode)

(use-package company-box
  :ensure t
  :diminish company-box
  :hook (company-mode . company-box-mode))


(use-package modus-themes
  :ensure t
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-slanted-constructs t
;;	set-face-attribute 'cursor nil :background (modus-themes-color-alts 'blue 'blue)
	modus-themes-mode-line '2d
        modus-themes-bold-constructs nil)
  :config
  ;; Load the theme of your choice
  ;; (modus-themes-load-operandi)
  ;; ;; OR
  (load-theme 'modus-vivendi t)
  :bind ("<f5>" . modus-themes-toggle))

(set-face-attribute 'cursor nil :background (modus-themes-color-alts 'blue 'blue))

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
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 110)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 110 :weight 'regular)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode -1)
  :custom-face (mode-line ((t (:height 0.95))))
  )

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

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
  (lsp-enable-which-key-integration t))

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred))


(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))

(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode 1))

(use-package yasnippet-snippets
  :ensure t)

(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-ivy
  :ensure t
  :init (all-the-icons-ivy-setup))

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
(use-package icomplete-vertical
  :ensure t
  :demand t
  :custom
  (completion-styles '(partial-completion substring))
  (completion-category-overrides '((file (styles basic substring))))
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  :config
  (icomplete-mode -1)
  (icomplete-vertical-mode)
  :bind (:map icomplete-minibuffer-map
              ("<down>" . icomplete-forward-completions)
              ("C-n" . icomplete-forward-completions)
              ("<up>" . icomplete-backward-completions)
              ("C-p" . icomplete-backward-completions)
              ("C-v" . icomplete-vertical-toggle)))

(use-package orderless
  :ensure t
  :init (icomplete-mode) ; optional but recommended!
  :custom (completion-styles '(orderless)))





;; imenu and imenu list

;; checkout below repos for code folding
;; https://github.com/gregsexton/origami.el/tree/e558710a975e8511b9386edc81cd6bdd0a5bda74
;; https://github.com/matsievskiysv/vimish-fold/tree/a6501cbfe3db791f9ca17fd986c7202a87f3adb8
;; https://github.com/emacsorphanage/yafolding/tree/4c1888ae45f9241516519ae0ae3a899f2efa05ba

;; (use-package workgroups2
;;   :ensure t
;;   :config
;;   (setq workgroups-mode 1
;; 	wg-mode-line-display-on t
;; 	wg-flag-modified t
;; 	wg-mode-line-decor-left-brace "["
;;         wg-mode-line-decor-right-brace "]"  ; how to surround it
;;         wg-mode-line-decor-divider ":"))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(global-hl-line-mode t)
 '(package-selected-packages
   '(orderless icomplete-vertical minions diminish ibuffer-projectile yasnippet-snippets which-key use-package rainbow-delimiters pyvenv python-mode powerline modus-themes magit lsp-pyright ivy-rich ivy-prescient helpful eyebrowse expand-region doom-modeline counsel-projectile company-prescient company-box all-the-icons-ivy all-the-icons-dired)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:height 0.95)))))
