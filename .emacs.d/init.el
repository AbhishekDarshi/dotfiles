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

(set-frame-font "Iosevka Comfy 11" nil t)

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
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/fun/")
    (setq projectile-project-search-path '("~/fun/")))
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

;; checkout helm as well

;;(use-package projectile
;;  :ensure t
;;  :diminish projectile-mode
;;  :config
;;  (projectile-mode)
;;  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
;;  (projectile-mode +1))

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


(use-package company-box
  :ensure t
  :diminish company-box
  :hook (company-mode . company-box-mode))


(use-package modus-themes
  :ensure t
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-slanted-constructs t
	modus-themes-mode-line '2d
        modus-themes-bold-constructs nil)
  :config
  ;; Load the theme of your choice
  ;; (modus-themes-load-operandi)
  ;; ;; OR
  (load-theme 'modus-operandi t)
  :bind ("<f5>" . modus-themes-toggle))

(set-face-attribute 'cursor nil :background (modus-themes-color-alts 'blue 'blue))

(use-package swiper
  :ensure t)

(use-package ivy
  :ensure t
  :diminish
  :bind (("C-s" . swiper)
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
  (ivy-mode 1))

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


;;(use-package minions
;;  :ensure t
;;  :config (minions-mode 1))

;;(use-package diminish
;;  :ensure t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(global-display-line-numbers-mode t)
 '(global-hl-line-mode t)
 '(package-selected-packages
   '(spaceline-config spaceline-all-the-icons yasnippet-snippets which-key use-package swiper rainbow-delimiters pyvenv python-mode projectile modus-themes magit lsp-pyright ivy-rich ivy-prescient expand-region doom-modeline diminish company-prescient company-box all-the-icons-ivy all-the-icons-dired)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:height 0.95)))))
