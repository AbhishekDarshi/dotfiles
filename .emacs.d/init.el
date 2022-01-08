(define-advice define-obsolete-function-alias (:filter-args (ll) fix-obsolete)
  (let ((obsolete-name (pop ll))
        (current-name (pop ll))
        (when (if ll (pop ll) "1"))
        (docstring (if ll (pop ll) nil)))
    (list obsolete-name current-name when docstring)))


(when (fboundp 'native-compile-async)
  (setq comp-deferred-compilation t
        comp-deferred-compilation-black-list '("/mu4e.*\\.el$")))
;;(native-compile-async "~/.emacs.d/elpa/" 6 t)
;;(native-compile-async "~/.emacs.d/straight/" 6 t)
;;(native-compile-async "~/.emacs.d/var/" 6 t)

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
  ;; (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  )

(add-to-list 'load-path "~/.emacs.d/custom_packages/")
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;; Diminish
(use-package diminish)
(diminish 'eldoc-mode "")

;; garbage collector
(use-package gcmh
  :demand
  :config
  (gcmh-mode 1))

(setq gc-cons-threshold 100000000)

(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; respect tile size
(setq frame-resize-pixelwise t)

;; ;; Example configuration for Consult
(use-package consult
  :disabled
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ;; ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ;; ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ;; ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ;; ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ;; ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ;; ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ;; ("M-#" . consult-register-load)
         ;; ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ;; ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ;; ("M-g e" . consult-compile-error)
         ;; ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ;; ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-project-imenu)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ;; ("M-s L" . consult-locate)
         ;; ("M-s g" . consult-grep)
         ;; ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-g l" . consult-line)
         ("M-g M-l" . consult-line)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ;; ("M-s u" . consult-focus-lines)
         ;; ;; Isearch integration
         ;; ("M-s e" . consult-isearch)
         :map isearch-mode-map
         ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
         ("M-g l" . consult-line)                 ;; required by consult-line to detect isearch
         )

  ;; Enable automatic preview at point in the *Completions* buffer.
  ;; This is relevant when you use the default completion UI,
  ;; and not necessary for Selectrum, Vertico etc.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme consult-ripgrep consult-grep consult-bookmark consult-recent-file
   consult--source-file consult--source-project-file consult--source-bookmark consult-buffer
   :preview-key (kbd "C-u"))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (project-roots)
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-root-function #'projectile-project-root)
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-root-function #'vc-root-dir)
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
)

;; ;; https://github.com/minad/consult/wiki
;; (defun consult-line-symbol-at-point ()
;;   (interactive)
;;   (consult-line (thing-at-point 'symbol)))

;; ;; referred from https://github.com/ashton314/.dotfiles/blob/master/.emacs
;; (use-package marginalia
;;   ;; :disabled
;;   :config
;;   (marginalia-mode)
;;   (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light)))

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :disabled
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package embark
  :disabled
  :bind
  (("C-S-a" . embark-act)       ; ctrl-shift-a
   ("C-," . embark-act)
   ("C-h B" . embark-bindings)) ; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; (setq embark-action-indicator
  ;;     (lambda (map &optional _target)
  ;;       (which-key--show-keymap "Embark" map nil nil 'no-paging)
  ;;       #'which-key--hide-popup-ignore-command)
  ;;     embark-become-indicator embark-action-indicator)
  ;; :hook
  ;; (embark-collect-post-revert . resize-embark-collect-completions)
  ;; :config
  ;; ;; (add-hook 'embark-collect-post-revert-hook
  ;; ;;         (defun resize-embark-collect-window (&rest _)
  ;; ;;           (when (memq embark-collect--kind '(:live :completions))
  ;; ;;             (fit-window-to-buffer (get-buffer-window)
  ;; ;;                                   (floor (frame-height) 2) 1))))
  ;; (defun resize-embark-collect-completions (&rest _)
  ;;   (fit-window-to-buffer (get-buffer-window)
  ;;                         (floor (* 0.4 (frame-height))) 1))
  ;; (add-hook 'embark-post-action-hook #'embark-collect--update-linked)
  )

;; https://github.com/oantolin/embark/wiki/Additional-Configuration
(defun embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (caar targets) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t))))

(setq embark-indicators
  '(embark-which-key-indicator
    embark-highlight-indicator
    embark-isearch-highlight-indicator))

;; ;; (use-package avy-embark-collect
;; ;;   :bind
;; ;;   (:map minibuffer-local-completion-map
;; ;;         ("C-'" . avy-embark-collect-choose)
;; ;;         ("C-\"" . avy-embark-collect-act)))

;; ;; Hide the mode line of the Embark live/completions buffers
;; (setq display-buffer-alist
;;       '(
;;         ;; ("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
;;         ;;        nil
;;         ;;        (window-parameters (mode-line-format . none)))))
;;         ("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
;;            (display-buffer-in-side-window)
;;            (side . bottom)
;;            (slot . 0)
;;            (window-parameters . ((no-other-window . t)
;;                               ;; (mode-line-format . none)
;;                               )))))

;; (global-set-key (kbd "C-.") 'embark-collect-completions)

;; ;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :disabled
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


;;Selectrum

(use-package selectrum
  :disabled
  :bind (("C-M-r" . selectrum-repeat)
         :map selectrum-minibuffer-map
         ("C-r" . selectrum-select-from-history)
         ;; ("C-j" . selectrum-next-candidate)
         ;; ("C-k" . selectrum-previous-candidate)
         :map minibuffer-local-map
         ("M-h" . backward-kill-word))
  :custom
  (selectrum-fix-minibuffer-height t)
  (selectrum-num-candidates-displayed 12)
  (selectrum-refine-candidates-function #'orderless-filter)
  (selectrum-highlight-candidates-function #'orderless-highlight-matches)
  ;; :custom-face
  ;; (selectrum-current-candidate ((t (:background "#3a3f5a"))))
  :init
  (selectrum-mode 1))

(global-set-key (kbd "<f6>") 'selectrum-cycle-display-style)
;; (setq magit-completing-read-function #'selectrum-completing-read)

(use-package selectrum-prescient
  :disabled
  :config
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

;; (defun shrink-selectrum ()
;;   (when (eq embark-collect--kind :live)
;;     (with-selected-window (active-minibuffer-window)
;;       (setq-local selectrum-num-candidates-displayed 1)
;;       (setq-local selectrum-display-style
;;                   '(horizontal :before-candidates "[" :after-candidates "]"
;;                                :more-candidates "" :candidates-separator "")))))

;; (add-hook 'embark-collect-mode-hook #'shrink-selectrum)

;; (defun refresh-selectrum ()
;;   (setq selectrum--previous-input-string nil))

;; (add-hook 'embark-pre-action-hook #'refresh-selectrum)



;; (use-package live-completions)
;; (defvar old-max-height-function temp-buffer-max-height)

;; (defun max-completions-height (buffer)
;;   (if (string= (buffer-name buffer) "*Completions*")
;;       20
;;     (funcall old-max-height-function temp-buffer-max-height)))

;; (setq temp-buffer-max-height #'max-completions-height)
;; (temp-buffer-resize-mode)
;; (setq live-completions-columns 'single)
;; (setq live-completions-sort-order 'cycle)

;; (icomplete-vertical-mode)
;; (icomplete-mode nil)
;; (use-package ido
;;   :config
;;   (ido-mode 1)
;;   (setq ido-enable-flex-matching t)
;;   (setq ido-everywhere t)
;;   (setq ido-all-frames nil)
;;   (setq ido-buffer-disable-smart-matches nil)
;;   (setq ido-completion-buffer "*Ido Completions*")
;;   (setq ido-completion-buffer-all-completions nil)
;;   (setq ido-confirm-unique-completion nil)
;;   (setq ido-create-new-buffer nil)
;;   (setq ido-default-buffer-method 'selected-window)
;;   (setq ido-default-file-method 'selected-window)
;;   (setq ido-enable-last-directory-history t)
;;   (setq ido-use-virtual-buffers t)
;;   (setq ido-use-faces t)
;;   (setq ido-max-window-height 1)
;;   )

;; (ido-mode 1)
;; (ido-everywhere 1)

;; (use-package ido-completing-read+)
;; (ido-ubiquitous-mode 1)
;; (setq magit-completing-read-function 'magit-ido-completing-read)


(use-package helm
  ;; :disabled
  :bind
  (("M-x" . helm-M-x)
   ;; ("C-x r b" . helm-filtered-bookmarks)
   ("C-x C-f" . helm-find-files)
   ("C-x b" . helm-buffers-list)
   ("C-x C-r" . helm-recentf)
   ("C-c i"   . helm-imenu)
   ("M-y" . helm-show-kill-ring)
   )
  :config
  (setq
   helm-split-window-inside-p           t ; open helm buffer inside current window, not occupy whole other window
      ;; helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
   helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      ;; helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
   helm-ff-file-name-history-use-recentf t
   helm-echo-input-in-header-line nil
     )
  (setq helm-autoresize-max-height 30)
  (setq helm-autoresize-min-height 10)
  (setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)
  (setq helm-idle-delay 0.0
     helm-input-idle-delay 0.01
     helm-quick-update t
     helm-M-x-requires-pattern nil
        helm-ff-skip-boring-files t
     )
  (setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
  (helm-autoresize-mode t)
  (setq helm-mini-default-sources '(helm-source-buffers-list
                                    helm-source-recentf
                                    helm-source-bookmarks
                                    helm-source-bookmark-set
                                    helm-source-buffer-not-found))
  (helm-mode 1)
  )


;; (use-package helm-swoop)
;; (use-package helm-ls-git)
(use-package rg)

(use-package helm-rg
  ;; :disabled
  )
(use-package ripgrep)

(use-package helm-xref
  ;; :disabled
  )

;; (use-package helm-ag)
;; (custom-set-variables
;;  '(helm-ag-base-command "rg --no-heading")
;;  `(helm-ag-success-exit-status '(0 2)))

(use-package helm-lsp)
(use-package helm-projectile
  :config
  (helm-projectile-on)
  )
(global-set-key (kbd "C-x p h") 'helm-projectile-find-file)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
;; (global-set-key (kbd "M-i") 'helm-swoop)
;; (global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
;; (global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
;; (global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)
(global-set-key (kbd "M-g l") 'helm-occur)
(global-set-key (kbd "C-x C-d") 'helm-browse-project)
(global-set-key (kbd "C-c r") 'rg-project--transient)
(global-set-key (kbd "C-x p r") 'helm-projectile-rg)
(global-set-key (kbd "M-s r") 'helm-rg)
(global-set-key (kbd "s-x") 'helm-register)
(global-set-key (kbd "s-y") 'helm-show-kill-ring)

;; ;; Projectile
;; (use-package projectile
;;   :bind (("C-c p" . projectile-command-map))
;;   :config
;;   ;; (setq projectile-enable-caching nil)
;;   (setq projectile-sort-order 'recently-active)
;;   (setq projectile-completion-system 'auto))

;; (use-package windmove
;;   :bind
;;   (("M-p f" . windmove-right)
;;    ("M-p b" . windmove-left)
;;    ("M-p p" . windmove-up)
;;    ("M-p n" . windmove-down)
;;    ))

;; (defun my/setup-color-theme ()
;;   (interactive)
;;   (when (display-graphic-p)
;;     (color-theme-sanityinc-solarized-dark))
;;   (set-background-color "black")
;;   (set-face-foreground 'secondary-selection "darkblue")
;;   (set-face-background 'secondary-selection "lightblue")
;;   (set-face-background 'font-lock-doc-face "black")
;;   (set-face-foreground 'font-lock-doc-face "wheat")
;;   (set-face-background 'font-lock-string-face "black"))
;; (use-package color-theme-sanityinc-solarized :config (my/setup-color-theme))

;; (when window-system
;;   (custom-set-faces
;;    '(erc-input-face ((t (:foreground "antique white"))))
;;    '(helm-selection ((t (:background "ForestGreen" :foreground "black"))))
;;    '(org-agenda-clocking ((t (:inherit secondary-selection :foreground "black"))) t)
;;    '(org-agenda-done ((t (:foreground "dim gray" :strike-through nil))))
;;    '(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t))))
;;    '(org-clock-overlay ((t (:background "SkyBlue4" :foreground "black"))))
;;    '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon" :strike-through t))))
;;    '(outline-1 ((t (:inherit font-lock-function-name-face :foreground "cornflower blue"))))))

;; (use-package corfu
;;   ;; Optional customizations
;;   :custom
;;   (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
;;   (corfu-auto t)                 ;; Enable auto completion
;;   ;; (corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
;;   (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
;;   (corfu-quit-no-match t)        ;; Automatically quit if there is no match
;;   (corfu-auto-delay 0.1)
;;   ;; (corfu-echo-documentation nil) ;; Do not show documentation in the echo area

;;   ;; You may want to enable Corfu only for certain modes.
;;   ;; :hook ((prog-mode . corfu-mode)
;;   ;;        (shell-mode . corfu-mode)
;;   ;;        (eshell-mode . corfu-mode))
;;   :init
;;   (corfu-global-mode))


;; Company
(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  ;; :bind (:map company-active-map
  ;;     ("<tab>" . company-complete-common-or-cycle))
  ;;    (:map lsp-mode-map
  ;;     ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.25))

;; (setq company-format-margin-function #'company-vscode-light-icons-margin)
;; ;; (company-tng-configure-default)

;; (add-hook 'after-init-hook 'global-company-mode)
(use-package company-prescient
  :config
  (company-prescient-mode +1))

;; (use-package company-box
;;   :diminish ""
;;   :hook (company-mode . company-box-mode))


;; (use-package ibuffer-projectile
;;   :after (projectile)
;;   :hook ibuffer . (lambda ()
;;                  (ibuffer-projectile-set-filter-groups)
;;                  (unless (eq ibuffer-sorting-mode 'recency)
;;                    (ibuffer-do-sort-by-recency))))

;; docker
;; https://www.emacswiki.org/emacs/TrampAndDocker
(use-package dockerfile-mode
  :defer t)

(use-package docker
  :bind ("C-c d" . docker))

(use-package docker-tramp)

;; Open files in Docker containers like so: /docker:drunk_bardeen:/etc/passwd
(push
 (cons
  "docker"
  '((tramp-login-program "docker")
    (tramp-login-args (("exec" "-it") ("%h") ("/bin/bash")))
    (tramp-remote-shell "/bin/sh")
    (tramp-remote-shell-args ("-i") ("-c"))))
 tramp-methods)

(defadvice tramp-completion-handle-file-name-all-completions
  (around dotemacs-completion-docker activate)
  "(tramp-completion-handle-file-name-all-completions \"\" \"/docker:\" returns
    a list of active Docker container names, followed by colons."
  (if (equal (ad-get-arg 1) "/docker:")
      (let* ((dockernames-raw (shell-command-to-string "docker ps | awk '$NF != \"NAMES\" { print $NF \":\" }'"))
             (dockernames (cl-remove-if-not
                           #'(lambda (dockerline) (string-match ":$" dockerline))
                           (split-string dockernames-raw "\n"))))
        (setq ad-return-value dockernames))
    ad-do-it))

(use-package yaml-mode
  :defer t)

;; (use-package web-mode
;;   :config
;;   (setq web-mode-markup-indent-offset 2)
;;   (setq web-mode-code-indent-offset 2)
;;   (setq web-mode-css-indent-offset 2))

(use-package json-mode
  :defer t)

(use-package pylint)
(use-package python-docstring
  :config
  (python-docstring-install))

;; https://github.com/Atman50/emacs-config#python
;; (use-package eglot
;;   :config
;;   (setq eglot-ignored-server-capabilites (quote (:signatureHelpProvider))))

;; (add-to-list 'eglot-server-programs
;;              '(python-mode "pylsp"))
;; (define-key eglot-mode-map (kbd "C-x l r r") 'eglot-rename)
;; (define-key eglot-mode-map (kbd "C-c l h") 'eldoc)
;; (use-package eldoc
;;   :custom
;;   (eldoc-echo-area-use-multiline-p nil))
;; ;; (global-set-key (kbd "s-c") ')

;; (use-package eldoc-box)
;; (add-hook 'eglot--managed-mode-hook #'eldoc-box-hover-mode t)
;; (define-key global-map (kbd "s-c") 'eldoc-box-eglot-hover-at-point)

;; (use-package python
;;   :init
;;   (setq-default indent-tabs-mode nil)
;;   :hook
;;   (python-mode . (lambda ()
;;                    (eglot-ensure)
;;                    )))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-x l")
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all nil)
  (lsp-idle-delay 0.5)
  ;; (lsp-rust-analyzer-server-display-inlay-hints t)
  :config
  (setq
   lsp-headerline-breadcrumb-enable nil
   lsp-enable-symbol-highlighting t
   lsp-modeline-diagnostics-enable t
   lsp-lens-enable t
   lsp-ui-sideline-enable t
   lsp-modeline-code-actions-enable t
   lsp-completion-show-kind nil
   )
  (setq lsp-signature-auto-activate nil) ;; you could manually requiest them via `lsp-signature-activate`
  (setq lsp-eldoc-enable-hover nil)
  ;; (setq lsp-eldoc-hook nil)
  (setq lsp-enable-links nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-completion-enable-additional-text-edit nil)
  )

(setq lsp-log-io nil) ; if set to true can cause a performance hit

;; (defun disable-lsp-conn-msg-advice (func &rest r)
;;   (unless (string-prefix-p "Connected to" (car r))
;;     (apply func r)))

;; (advice-add 'lsp--info :around #'disable-lsp-conn-msg-advice)


(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

;; (use-package dimmer
;;   :custom (dimmer-fraction 0.1)
;;   :config (dimmer-mode))

;; (use-package lsp-ui
;;   :commands lsp-ui-mode
;;   :custom
;;   (lsp-ui-peek-always-show nil)
;;   (lsp-ui-sideline-show-hover nil)
;;   (lsp-ui-doc-enable nil))

;; supposedly better syntax highlighting compared to default regex based one.
;; but modus themes look more rainbowish when using this mode

(use-package tree-sitter
  ;; :custom-face
  ;; (tree-sitter-hl-face:method.call   ((t (:inherit font-lock-function-name-face))))
  ;; (tree-sitter-hl-face:function.call ((t (:inherit font-lock-function-name-face))))
  ;; (tree-sitter-hl-face:operator      ((t (:inherit default))))
  ;; (tree-sitter-hl-face:type.builtin  ((t (:inherit font-lock-type-face))))
  ;; (tree-sitter-hl-face:number        ((t (:inherit highlight-numbers-number))))
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs)


(use-package lsp-pyright
  :init
  (setq lsp-pyright-typechecking-mode "basic")
  :hook
  (python-mode .
               (lambda ()
                 ;; (setq indent-tabs-mode nil)
                 (setq tab-width 4)
                 ;; (setq python-indent-offset 4)
                 (require 'lsp-pyright)
                 (lsp))))


;; (use-package lsp-ui)
;; (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
;; (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

;; (use-package dap-mode
;;   :config
;;   (dap-ui-mode)
;;   (dap-ui-controls-mode 1)

;;   (require 'dap-lldb)
;;   (require 'dap-python)
;;   (require 'dap-gdb-lldb)
;;   ;; installs .extension/vscode
;;   )

;; (use-package python-black
;;   :demand t
;;   :after python
;;   ;; :config
;;   ;; (setq python-black-on-save-mode t)
;;   )

;; https://github.com/wbolster/emacs-python-pytest
;;(use-package python-pytest)

(use-package pyvenv
  :config
  ;; (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  (add-hook 'pyvenv-post-activate-hooks
            #'(lambda ()
                (call-interactively #'lsp-workspace-restart)))
  (pyvenv-mode +1))


(use-package highlight-indent-guides
  :hook
  (python-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-responsive 'top))


;; Ace, Avy
(use-package ace-window
  :config
  (setq aw-background nil)
  (ace-window-display-mode +1))

;; Multiple-cursors
;;(use-package multiple-cursors)
;; TODO: keybindings


;; (use-package flycheck
;;   :hook ((prog-mode . flycheck-mode))
;;   :config
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled newline))
;;   (setq flycheck-display-errors-delay 0.1)
;;   (setq flycheck-flake8rc "~/.config/flake8"))


;; (use-package flycheck
;;   :hook
;;   (python-mode . flycheck-mode)
;;   :config
;;   ;; (setq flycheck-display-errors-function nil)
;;   (flycheck-set-indication-mode 'left-margin)
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled newline))
;;   (setq flycheck-display-errors-delay 0.1)
;;   )

;; (use-package eldoc-overlay
;;   :init (eldoc-overlay-mode 1))



(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package which-key
  :diminish ""
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 1))


(use-package magit)

(use-package diff-hl
  :diminish ""
  :config
  (global-diff-hl-mode)
  )
(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; (use-package git-timemachine
;;   :defer t)

;; Programming
;; (use-package smartparens
;;   :diminish ""
;;   :defer t
;;   :config
;;   (smartparens-global-mode -1))

;; (use-package racket-mode
;;   :defer t)


;; Ace window
(define-key global-map (kbd "C-c o") 'ace-window)

;; Avy
(define-key global-map (kbd "C-c j") 'avy-goto-line)
(define-key global-map (kbd "C-M-j") 'avy-goto-char)
(define-key global-map (kbd "C-c J") 'avy-goto-word-0)
(define-key global-map (kbd "s-j") 'avy-goto-char)
;; (define-key global-map (kbd "s-J") 'avy-goto-line)
(define-key global-map (kbd "C-s-j") 'avy-goto-word-0)

;; (setq completion-styles '(orderless))

(use-package orderless
  :disabled
  :init
  (setq completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

;; (advice-add #'marginalia-cycle :after
;;          (lambda () when (bound-and-true-p selectrum-mode) (selectrum-exhibit
;;                                                             'keep-selected)))

;; elfeed configs
;; set default elfeed db directory
;; (use-package elfeed
;;   :config
;;   (setq elfeed-feeds
;;      '(("https://www.reddit.com/r/emacs/new.rss" emacs)
;;        ("https://200ok.ch/atom.xml" emacs)
;;        ("https://www.reddit.com/r/planetemacs/new.rss" emacs)
;;        ("https://www.reddit.com/r/django/new.rss" python django)
;;        ("https://www.reddit.com/r/archlinux/new.rss" linux arch)
;;        ("https://news.ycombinator.com/rss" news)
;;        ))
;;   )
;; (global-set-key (kbd "C-x w") 'elfeed)



;; cycle spacing blank characters
;;(global-set-key (kbd "M-SPC") 'cycle-spacing)
;; delete blank lines - by default it is bound to C-x C-o
;;(global-set-key (kbd "M-o") 'delete-blank-lines)

;; (use-package ujelly-theme
;;    :config
;;    (load-theme 'ujelly t)
;;    (set-frame-parameter (selected-frame) 'alpha '(90 90))
;;    (add-to-list 'default-frame-alist '(alpha 90 90))
;;    (set-cursor-color "#dc322f")
;;    (set-face-attribute 'region nil :background "#666" :foreground "#ffffff")

;;    (set-face-attribute 'mode-line nil :foreground "#bdc3ce" :background "#000")
;;    (set-face-attribute 'default nil :background "#000" :foreground "#eee"))

(use-package modus-themes
  :straight (modus-themes :type git :host gitlab :repo "protesilaos/modus-themes")
  :init
  (setq modus-themes-italic-constructs nil
        modus-themes-bold-constructs nil
	modus-themes-tabs-accented t
	modus-themes-intense-markup t
        modus-themes-fringes 'subtle ; {nil,'subtle,'intense}
        ;; Options for `modus-themes-lang-checkers': nil,
        ;; 'straight-underline, 'subtle-foreground,
        ;; 'subtle-foreground-straight-underline, 'intense-foreground,
        ;; 'intense-foreground-straight-underline, 'colored-background
        modus-themes-lang-checkers 'straight-underline
        modus-themes-mode-line '(3d borderless)
        ;; Options for `modus-themes-syntax': nil, 'faint,
        ;; 'yellow-comments, 'green-strings,
        ;; 'yellow-comments-green-strings, 'alt-syntax,
        ;; 'alt-syntax-yellow-comments
        modus-themes-success-deuteranopia t
        modus-themes-syntax '(green-strings)
        modus-themes-hl-line '()
        modus-themes-paren-match 'intense ; {nil,'subtle-bold,'intense,'intense-bold}
        ;; Options for `modus-themes-links': nil, 'faint,
        ;; 'neutral-underline, 'faint-neutral-underline, 'no-underline,
        ;; 'underline-only
        modus-themes-links 'neutral-underline
        modus-themes-subtle-line-numbers nil
        ;; Options for `modus-themes-prompts' are either nil (the
	;; default), or a list of properties that may include any of those
	;; symbols: `background', `bold', `gray', `intense', `italic'
	modus-themes-prompts '(intense bold)

	modus-themes-completions 'moderate ; {nil,'moderate,'opinionated}

        ;; Options for `modus-themes-region' are either nil (the default),
	;; or a list of properties that may include any of those symbols:
	;; `no-extend', `bg-only', `accented'
	modus-themes-region '(bg-only no-extend)

        ;; Options for `modus-themes-diffs': nil, 'desaturated,
	;; 'bg-only, 'deuteranopia, 'fg-only-deuteranopia
	modus-themes-diffs nil

	modus-themes-subtle-line-numbers t
        modus-themes-inhibit-reload nil
        ;; modus-themes-success-deuteranopia nil
        modus-themes-scale-1 1.1
        modus-themes-scale-2 1.15
        modus-themes-scale-3 1.21
        modus-themes-scale-4 1.27
        modus-themes-scale-5 1.33
        )
  (load-theme 'modus-vivendi t)
  :config
  (set-face-attribute 'cursor nil :background (modus-themes-color-alts 'blue 'fg-special-cold))
  :bind ("<f5>" . modus-themes-toggle))


;; https://protesilaos.com/modus-themes/
(defun my-modus-themes-custom-faces ()
  (set-face-attribute 'cursor nil :background (modus-themes-color-alts 'blue 'fg-special-cold))
  ;; (Set-Face-attribute 'font-lock-type-face nil :foreground (modus-themes-color 'magenta-alt))
  )

(add-hook 'modus-themes-after-load-theme-hook #'my-modus-themes-custom-faces)

;; (use-package mixed-pitch
;;   :hook
;;   ;; If you want it in all text modes:
;;   (text-mode . mixed-pitch-mode))

;; (use-package moody
;;   :config
;;   (setq x-underline-at-descent-line t
;;      moody-mode-line-height 18
;;      )
;;   (moody-replace-mode-line-buffer-identification)
;;   (moody-replace-vc-mode))

(use-package rainbow-mode)

;; (use-package spacemacs-theme)

;; (set-face-attribute 'mode-line nil
;;                     :background "#10387c"
;;                     :foreground "white"
;;                     :box '(:line-width 1 :color "#10387c")
;;                     :overline nil
;;                     :underline nil)

;; (set-face-attribute 'mode-line-inactive nil
;;                     :background "#222222"
;;                     :foreground "white"
;;                     :overline nil
;;                     :underline nil)

;; gap between lang checker underline and text
(setq underline-minimum-offset 3)
;; (set-background-color "honeydew")
;; (set-background-color "white")
;; (set-background-color "mint cream")



;; Theme
;; (straight-use-package 'vscode-dark-plus-theme)
;; (load-theme 'vscode-dark-plus t)

;; (use-package doom-themes
;;   :config
;;    (setq doom-themes-enable-bold nil    ; if nil, bold is universally disabled
;;          doom-themes-enable-italic nil)
;;    (load-theme 'doom-dracula t)
;;    )

;; (use-package doom-modeline
;;    :init (doom-modeline-mode 1)
;;    )

;;(use-package mood-line
;;  :init (mood-line-mode -1))
;;(use-package telephone-line)

;; (use-package kaolin-themes
;;   :config
;;   (load-theme 'kaolin-dark t)
;;   (kaolin-treemacs-theme))

;; (load-theme 'word-perfect t)

;; font test
;; {([])}<> {}
;; 1 l L 0 O m M a o c 8 B  mmm ' "" ; :   Illegal1 = O0
;; (set-face-attribute 'default nil :font "Noto Sans Mono" :height 100)
;; (set-face-attribute 'default nil :font "Iosevka Comfy" :height 110)
;; (set-face-attribute 'default nil :font "Iosevka Term" :height 105)
;; (set-face-attribute 'default nil
;; 		    :font "InputMonoCondensed ExLight"
;; 		    :height 108
;; 		    :weight 'normal
;; 		    :width 'normal)

;; (set-frame-font "Input Mono Condensed 10")

;; (set-face-attribute 'default nil :font "Hack Nerd Font" :height 105)
;; (set-face-attribute 'default nil :font "Input Mono" :height 105)
;; (set-face-attribute 'default nil :font "Menlo" :height 105)
;; (set-face-attribute 'default nil :font "Monoid Nerd Font" :height 90)
;; (set-face-attribute 'default nil :font "B612 Mono" :height 100)
;; (set-face-attribute 'default nil :font "JuliaMono" :height 105)
(set-face-attribute 'default nil :font "monospace" :height 105)
;; (set-face-attribute 'default nil :font "FiraCode Nerd Font" :height 110)
;; (set-face-attribute 'default nil :font "Victor Mono" :height 105)
;; (set-face-attribute 'default nil :font "SauceCodePro Nerd Font" :height 105)
;; (set-face-attribute 'default nil :font "Ubuntu Mono" :height 120)
;; (set-face-attribute 'default nil :font "Cascadia Code PL" :height 105)
;; (set-face-attribute 'default nil :font "Envy Code R" :height 110)
(set-face-attribute 'default nil :font "RobotoMono Nerd Font" :height 105)
;; (set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 105)
;; (set-face-attribute 'default nil :font "Liberation Mono" :height 105)
;; (set-face-attribute 'default nil :font "Jetbrains Mono Nerd Font Mono" :height 105)
;; (set-face-attribute 'default nil :font "Go Mono" :height 105)
;; (set-face-attribute 'default nil :font "ProggyCrossed" :height 105)
;; (set-face-attribute 'default nil :font "Mononoki Nerd Font" :height 110)
;; (set-face-attribute 'default nil :font "Consolas Ligaturized V2" :height 110)

;; (set-face-attribute 'default nil :font "Ohsnap" :height 105)
;; very big size
;; (set-face-attribute 'default nil :font "knxt" :height 105)
;; (set-face-attribute 'default nil :font "Cozette" :height 110)
;; (set-face-attribute 'default nil :font "envypn" :height 110)
;; (set-face-attribute 'default nil :font "boxxy" :height 110)
;; (set-face-attribute 'default nil :font "Ttyp0" :height 110)
;; (set-face-attribute 'default nil :font "smoothansi" :height 90)
;; (set-face-attribute 'default nil :font "terminusmodx" :height 90)
;; (set-face-attribute 'default nil :font "Tamzen" :height 105)
;; (set-face-attribute 'default nil :font "Dina" :height 90)
;; (set-face-attribute 'default nil :font "ctrld" :height 110)
;; (set-face-attribute 'default nil :font "Gohu GohuFont" :height 150)
;; (set-face-attribute 'default nil :font "Fixed" :height 105)

;; (set-face-attribute 'default nil :font "-xos4-terminus-*-*-*-*-14-*-*-*-*-*-*-*")
;; (set-face-attribute 'default nil :font "-xos4-terminus-*-*-*-*-12-*-*-*-*-*-*-*")
;; Set the fixed pitch face
;; (set-face-attribute 'fixed-pitch nil :font "Input Sans" :height 90)
(set-face-attribute 'variable-pitch nil :font "Go" :height 110)
;; (set-face-attribute 'mode-line nil :font "B612" :height 105)
;; (set-face-attribute 'mode-line-inactive nil :font "B612" :height 105)
;; (set-face-attribute 'mode-line nil :font "FiraGo Medium" :height 105)
;; (set-face-attribute 'mode-line-inactive nil :font "FiraSans" :height 105)
(set-face-attribute 'mode-line nil :font "Go" :height 110)
(set-face-attribute 'mode-line-inactive nil :font "Go" :height 110)

;; (set-face-attribute 'which-key-group-description-face nil :font "Input Mono")
;; (set-face-attribute 'which-key-command-description-face nil :font "Input Mono")
;; (set-face-attribute 'which-key-key-face nil :font "Input Mono")
;; (set-face-attribute 'which-key-special-key-face nil :font "Input Mono")
;; (set-face-attribute 'which-key-local-map-description-face nil :font "Input Mono")
;; (set-face-attribute 'which-key-note-face nil :font "Input Mono")


;; (use-package company-posframe
;;   :diminish ""
;;   :config
;;   ;; (setq company-posframe-font "Ttyp0")
;;   (company-posframe-mode 1))

;; (use-package centaur-tabs
;;   :hook (emacs-startup . centaur-tabs-mode)
;;   :init
;;   (setq centaur-tabs-set-icons t)
;;   (setq centaur-tabs-set-modified-marker t
;;         centaur-tabs-modified-marker "*"
;;         centaur-tabs-cycle-scope 'tabs)
;;   (setq centaur-tabs-set-close-button t)
;;   :config
;;   (centaur-tabs-mode t)
;;   ;; (centaur-tabs-headline-match)
;;   (centaur-tabs-group-by-projectile-project)
;;   )

;; (use-package jupyter)
;; (use-package ein)

;;(use-package centered-cursor-mode)

;; (use-package rustic
;;   :bind (:map rustic-mode-map
;;               ("M-j" . lsp-ui-imenu)
;;               ("M-?" . lsp-find-references)
;;               ("C-c C-c l" . flycheck-list-errors)
;;               ("C-c C-c a" . lsp-execute-code-action)
;;               ("C-c C-c r" . lsp-rename)
;;               ("C-c C-c q" . lsp-workspace-restart)
;;               ("C-c C-c Q" . lsp-workspace-shutdown)
;;               ("C-c C-c s" . lsp-rust-analyzer-status))
;;   :config
;;   ;; uncomment for less flashiness
;;   (setq lsp-eldoc-hook nil)
;;   (setq lsp-enable-symbol-highlighting nil)
;;   (setq lsp-signature-auto-activate nil)

;;   ;; comment to disable rustfmt on save
;;   (setq rustic-format-on-save t)
;;   (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

;; (defun rk/rustic-mode-hook ()
;;   ;; so that run C-c C-c C-r works without having to confirm
;;   (setq-local buffer-save-without-query t))

;; (use-package yasnippet
;;   :config
;;   (yas-reload-all)
;;   (add-hook 'prog-mode-hook 'yas-minor-mode)
;;   (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package vterm)
(use-package multi-vterm)

;; (use-package perspective
;;   :bind ("C-x b" . persp-switch-to-buffer*)
;;   :config
;;   (persp-mode))

;; (global-set-key (kbd "C-x C-b") (lambda (arg)
;;                                   (interactive "P")
;;                                   (if (fboundp 'persp-ibuffer)
;;                                       (persp-ibuffer arg)
;;                                     (ibuffer "all"))))
;; tab bar settings
(defun prot-tab-tab-bar-toggle ()
  "Toggle `tab-bar' presentation."
  (interactive)
  (if (bound-and-true-p tab-bar-mode)
      (progn
        (setq tab-bar-show nil)
        (tab-bar-mode -1))
    (setq tab-bar-show t)
    (tab-bar-mode 1)))

;; (setq tab-bar-show nil)
;; (setq tab-bar-position nil)
(global-set-key (kbd "<f8>") 'prot-tab-tab-bar-toggle)

;; https://protesilaos.com/dotemacs/#h:88872e4c-7143-4f93-b8a0-9e92cf36fb78
;; (use-package tab-bar-echo-area
;;   :config
;;   (tab-bar-echo-area-mode 1))
;; another one which might be useful - https://github.com/fritzgrabo/tab-bar-groups

;; ;; https://github.com/fritzgrabo/project-mode-line-tag
;; (use-package project-mode-line-tag
;;   :config
;;   (project-mode-line-tag-mode))


(use-package lua-mode)
(use-package haskell-mode)

(use-package treemacs
  :defer t
  :bind
  (:map global-map
        ("C-x t t"   . treemacs)))

(use-package lsp-treemacs)

;; (use-package treemacs-projectile
;;   :after (treemacs projectile))


;; postgres port option enablement
;;(setq sql-postgres-options (list "-P <port number>"))

;; to check
;; https://github.com/DiegoVicen/my-emacs#sql-console

;; todo
;; hydra
;; dap mode - add keybindings
;; https://luca.cambiaghi.me/vanilla-emacs/readme.html#h:5F1EC880-5646-4E50-A460-C2F23BD864FC

(put 'narrow-to-region 'disabled nil)

(setq org-todo-keywords
      '((sequence "TODO" "PLANNED" "IN_PROGRESS" "|" "DONE" "WONT_DO")))

(add-hook 'org-mode-hook 'variable-pitch-mode)
(setq org-hide-emphasis-markers t)
;; (add-hook 'org-mode-hook 'visual-line-mode)

;; newly added config, testing out few things

;; (defun pulse-line (&rest _)
;;       "Pulse the current line."
;;       (pulse-momentary-highlight-one-line (point)))

;; (dolist (command '(scroll-up-command scroll-down-command
;;                    recenter-top-bottom other-window))
;;   (advice-add command :after #'pulse-line))

;; (use-package popper
;;   :bind (("C-`"   . popper-toggle-latest)
;;          ("M-`"   . popper-cycle)
;;          ("C-M-`" . popper-toggle-type))
;;   :init
;;   (setq popper-reference-buffers
;;         ;; '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*")
;;         '("\\*Messages\\*"
;;           "Output\\*$"
;;           help-mode
;; 	  *xref*
;;           compilation-mode)
;;      )
;;   (popper-mode +1))

;; (setq-default cursor-type '(hbar . 4))
(setq-default cursor-in-non-selected-windows 'hollow)
(setq-default blink-cursor-blinks 60)
(setq-default blink-cursor-interval 0.15)
(setq-default blink-cursor-delay 0.15)

(setq show-paren-style 'parenthesis)
(setq show-paren-when-point-in-periphery nil)
(setq show-paren-when-point-inside-paren nil)
(add-hook 'after-init-hook #'show-paren-mode)

(setq desktop-auto-save-timeout 300)
(setq desktop-path `(,user-emacs-directory))
(setq desktop-base-file-name "desktop")
(setq desktop-files-not-to-save nil)
(setq desktop-globals-to-clear nil)
(setq desktop-load-locked-desktop t)
(setq desktop-missing-file-warning nil)
(setq desktop-restore-eager 0)
(setq desktop-restore-frames nil)
(setq desktop-save 'ask-if-new)
(desktop-save-mode 1)

(setq savehist-file (locate-user-emacs-file "savehist"))
(setq history-length 10000)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history t)
(add-hook 'after-init-hook #'savehist-mode)

(setq save-place-file (locate-user-emacs-file "saveplace"))
(setq save-place-forget-unreadable-files t)
(save-place-mode 1)

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backup/"))))
(setq backup-by-copying t)
(setq version-control t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq create-lockfiles nil)

;; add a hook to call this only when I press tab and there are multiple candidates to
;; choose from for a given action/command.

;; (use-package live-completions
;;   ;; :bind (:map minibuffer-local-completion-map
;;   ;;             ("C-v" . live-completions-set-columns))
;;   :config (live-completions-mode -1))


;; (global-set-key (kbd "M-s p") (lambda()
;;                              (interactive)
;;                              (live-completions-do
;;                               (:columns 'single
;;                                         :height 30)
;;                               (consult-line))))

;; (global-set-key (kbd "C-c e") (lambda()
;;                              (interactive)
;;                              (live-completions-do
;;                               (:columns 'single
;;                                         :height 30)
;;                               (consult-ripgrep))))

;; (let ((map minibuffer-local-completion-map))
;;     (define-key map (kbd "SPC") nil)
;;     ;; (define-key map (kbd "?") nil)
;;     )

(global-auto-revert-mode t)

(setq completion-styles
      ;; '(substring initials flex partial-completion orderless))
      '(substring initials partial-completion orderless))
(setq completion-ry-overrides
      '((file (styles . (partial-completion orderless)))))

;; allow tab to cycle through completions when true
(setq completion-cycle-threshold 3)

(setq completion-flex-nospace t)
;; (setq completion-pcm-complete-word-inserts-delimiters t)
;; (setq completion-pcm-word-delimiters "-_./:| ")
;; (setq completion-show-help nil)
(setq completion-auto-help t)
(setq completion-ignore-case t)
(setq-default case-fold-search t)   ; For general regexp

;; The following two are updated in Emacs 28.  They concern the
;; *Completions* buffer.
(setq completions-format 'one-column)
;; (setq completions-detailed nil)

(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

(setq enable-recursive-minibuffers t)
(setq read-answer-short t) ; also check `use-short-answers' for Emacs28

;; resizes minuffer based on the candidates
(setq resize-mini-windows 1)
(setq minibuffer-eldef-shorten-default t)

(setq echo-keystrokes 0.25)           ; from the C source code

(file-name-shadow-mode 1)
(minibuffer-depth-indicate-mode 1)
(minibuffer-electric-default-mode 1)
(electric-pair-mode 1)

;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/custom_lisp/"))

;; (use-package minibuffer-extras
;;   :straight nil
;;   :bind
;;   (:map minibuffer-local-completion-map
;;         ("RET" . exit-with-top-completion))
;;   (:map minibuffer-local-must-match-map
;;         ("RET" . exit-with-top-completion))
;;   (:map minibuffer-local-filename-completion-map
;;         ("RET" . exit-with-top-completion)
;;         ("<C-backspace>" . up-directory)
;;         ("M-." . cd-bookmark)))

(use-package nginx-mode)

(use-package prescient
  :config
  (setq prescient-filter-method '(literal regexp initialism fuzzy))
  (prescient-persist-mode +1))


;; (add-to-list 'load-path "/home/use/packages/scratch.el")
;; (require 'scratch)
;; (global-set-key (kbd "C-c s") 'scratch-create)
;; ;; (use-package scratch
;; ;;   :bind ("C-c s" . scratch-create)
;; ;;   )

(use-package wgrep)
;; (setq window-combination-resize -1)

;; (use-package cyberpunk-theme)
(global-so-long-mode 1)

;; (use-package amx
;;   :defer t
;;   )

;; (fido-mode -1)

;; (use-package icomplete
;;   :config
;;   (setq icomplete-in-buffer t)
;;   (setq icomplete-show-matches-on-no-input t)
;;   (setq icomplete-max-delay-chars 0)
;;   (setq icomplete-compute-delay 0)
;;   (setq icomplete-prospects-height 1)
;;   (setq icomplete-with-completion-tables t)
;;   (setq icomplete-separator (propertize " · " 'face 'shadow))
;;   (fido-mode -1)
;;   (icomplete-mode 1)
;;   :bind
;;   (
;;    :map icomplete-fido-mode-map
;;      ("C-n" . icomplete-forward-completions)
;;      ("C-p" . icomplete-backward-completions)
;;          ("C-v" . icomplete-vertical-toggle)
;;      )
;;   )

;; C-j to select first candidate
;; (icomplete-mode +1)
;; (add-hook 'icomplete-mode-hook #'icomplete-vertical-mode)
;; (icomplete-vertical-mode t)

;; (setq icomplete-scroll t)
;; (setq icomplete-show-matches-on-no-input t)

;; ;; https://github.com/oantolin/icomplete-vertical
;; (use-package icomplete-vertical
;;   :demand t
;;   :custom
;;   (completion-styles '(partial-completion substring))
;;   (completion-category-overrides '((file (styles basic substring))))
;;   (read-file-name-completion-ignore-case t)
;;   (read-buffer-completion-ignore-case t)
;;   (completion-ignore-case t)
;;   (icomplete-vertical-prospects-height 15)
;;   :config
;;   (icomplete-mode)
;;   (icomplete-vertical-mode -1)
;;   :bind (:map icomplete-minibuffer-map
;;               ("<down>" . icomplete-forward-completions)
;;               ("C-n" . icomplete-forward-completions)
;;               ("<up>" . icomplete-backward-completions)
;;               ("C-p" . icomplete-backward-completions)
;;               ("C-v" . icomplete-vertical-toggle)))

;; ;; (defun insert-kill-ring-item ()
;; ;;   "Insert item from kill-ring, selected with completion."
;; ;;   (interactive)
;; ;;   (icomplete-vertical-do (:separator 'dotted-line :height 20)
;; ;;     (insert (completing-read "Yank: " consult-ripgrep nil t))))

;; (global-set-key (kbd "M-g l") (lambda()
;;                              (interactive)
;;                              (icomplete-vertical-do
;;                               (:height 20)
;;                               (consult-line))))

;; (global-set-key (kbd "C-c r") (lambda()
;;                              (interactive)
;;                              (icomplete-vertical-do
;;                               (:height 20)
;;                               (consult-ripgrep))))

;; (vcomplete-mode -1)
;; (setq vcomplete-auto-update nil)
;; (setq vcomplete-no-update-commands '(switch-to-buffer))


;; (setq flymake-fringe-indicator-position 'left-fringe)
;; ;; (setq flymake-suppress-zero-counters t)
;; (setq flymake-start-on-flymake-mode t)
;; (setq flymake-no-changes-timeout nil)
;; (setq flymake-start-on-save-buffer t)
;; (setq flymake-proc-compilation-prevents-syntax-check t)
;; (setq flymake-wrap-around nil)

;; modeline changes from prot
;; (setq mode-line-percent-position '(-3 "%p"))
;; (setq mode-line-position-column-line-format '(" %l,%c")) ; Emacs 28
;; (setq mode-line-defining-kbd-macro
;;       (propertize " Macro" 'face 'mode-line-emphasis))

;; (setq mode-line-compact nil)
;; (setq-default mode-line-format
;;               '("%e"
;;                 mode-line-front-space
;;                 mode-line-mule-info
;;                 mode-line-client
;;                 mode-line-modified
;;                 mode-line-remote
;;                 mode-line-frame-identification
;;                 mode-line-buffer-identification
;;                 "  "
;;                 mode-line-position
;;                 mode-line-modes
;;                 "  "
;;                 (vc-mode vc-mode)
;;                 "  "
;;                 mode-line-misc-info
;;                 mode-line-end-spaces))

(use-package minions
  :config
  (setq minions-mode-line-lighter ";")
  ;; NOTE: This will be expanded whenever I find a mode that should not
  ;; be hidden
  (setq minions-mode-line-delimiters '("" . ""))
  (setq minions-direct (list 'defining-kbd-macro
                             'flycheck-mode
                             ;; 'flymake-mode
                             ))
  (minions-mode 1))

;; (use-package smart-mode-line
;;   :config
;;   (setq sml/no-confirm-load-theme t)
;;   (setq sml/modified-char "*")
;;   (setq sml/theme 'respectful)
;;   (sml/setup))

;; (setq window-divider-default-right-width 1)
;; (setq window-divider-default-bottom-width 1)
;; (setq window-divider-default-places 'right-only)
;; (add-hook 'after-init-hook #'window-divider-mode)

;; (setq-default fringes-outside-margins nil)
;; (setq-default indicate-buffer-boundaries nil)
;; (setq-default indicate-empty-lines nil)
;; (setq-default overflow-newline-into-fringe t)


;; (add-to-list 'load-path "~/packages/snails")
;; (require 'snails)
;; (setq snails-show-with-frame t)
;; (setq snails-input-buffer-face 90)


;; git clone https://gitlab.com/protesilaos/mct.el.git
;; https://protesilaos.com/emacs/mct
(use-package mct
  :disabled
  :straight (mct :type git :host gitlab :repo "protesilaos/mct")
  :config
  (setq mct-remove-shadowed-file-names t)
  (setq mct-apply-completion-stripes t)
  (setq mct-hide-completion-mode-line t)
  (setq mct-live-update-delay 0.2)
  (mct-mode 1)
  )

;; This is for commands that should always pop up the completions'
;; buffer.  It circumvents the default method of waiting for some user
;; input (see `mct-minimum-input') before displaying and updating the
;; completions' buffer.
(setq mct-completion-passlist
      '(imenu
        Info-goto-node
        Info-index
        Info-menu
        vc-retrieve-tag))

;; You can place the Completions' buffer wherever you want, by following
;; the syntax of `display-buffer'.  For example, try this:
;; TOCHECK: if i can pop up buffer below only current window
(setq mct-display-buffer-action
      (quote ((display-buffer-reuse-window
               display-buffer-in-side-window)
              (side . bottom)
              (slot . 99)
              (window-width . 0.3))))

;; (use-package vertico
;;   :init
;;   (vertico-mode 1)
;;   )

;; (require 'vertico-flat)
;; ;; (require 'vertico-buffer)
;; ;; (require 'vertico-repeat)
;; (global-set-key (kbd "<f6>") 'vertico-flat-mode)

;; (use-package mini-frame
;;   :config
;;   (custom-set-variables
;;    '(mini-frame-show-parameters
;;      '((top . 0.4)
;;        (width . 0.7)
;;        (left . 0.5))))
;;   (mini-frame-mode 1)
;;   )


;; (use-package fzf)
;; (global-set-key (kbd "C-c c b") 'fzf-switch-buffer)
;; (global-set-key (kbd "C-c c f") 'fzf-find-file)
;; (global-set-key (kbd "C-c c g") 'fzf-git-grep)
;; (global-set-key (kbd "C-c c c") 'fzf-git-files)
;; (global-set-key (kbd "C-c c r") 'fzf-recentf)


(use-package savehist
  :init
  (savehist-mode))

;; (use-package emacs
;;   :init
;;   ;; Add prompt indicator to `completing-read-multiple'.
;;   (defun crm-indicator (args)
;;     (cons (concat "[CRM] " (car args)) (cdr args)))
;;   (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;;   ;; Grow and shrink minibuffer
;;   ;;(setq resize-mini-windows t)

;;   ;; Do not allow the cursor in the minibuffer prompt
;;   (setq minibuffer-prompt-properties
;;         '(read-only t cursor-intangible t face minibuffer-prompt))
;;   (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;   ;; Enable recursive minibuffers
;;   (setq enable-recursive-minibuffers t))

;; (straight-use-package 'ctrlf)
;; (ctrlf-mode 1)

;; (defun my-run-some-commands ()
;;   "Run `some-command' and `some-other-command' in sequence."
;;   (interactive)
;;   ;; (selectrum-cycle-display-style)
;;   (consult-buffer)
;;   (selectrum-cycle-display-style)
;;   )

;; (global-set-key (kbd "C-c a") 'my-run-some-commands)

;; (global-set-key (kbd "C-x C-b") 'ibuffer)
(use-package rg)

;; (use-package ivy
;;   :config
;;   (setq ivy-display-style 'fancy)
;;   (setq enable-recursive-minibuffers t)
;;   (setq ivy-use-selectable-prompt t)
;;   (setq ivy-use-virtual-buffers t)
;;   (setq ivy-count-format "%d/%d ")
;;   (ivy-mode 1))

;; (use-package swiper
;;   :bind ("M-g l" . swiper))

;; (use-package counsel
;;   :diminish
;;   :bind (
;;       ;; ("C-M-j" . 'counsel-switch-buffer)
;;       ("M-x" . counsel-M-x)
;;       ("M-s g" . 'counsel-git-grep)
;;       ("M-g l" .  swiper-thing-at-point)
;;       ("M-s r". counsel-rg)
;;       ("C-c R" . counsel-register)
;;       ("M-p". counsel-git)
;;       ("C-x C-f" . 'counsel-find-file)
;;       ("M-y". 'counsel-yank-pop)
;;          :map minibuffer-local-map
;;          ("C-r" . 'counsel-minibuffer-history))
;;   :config
;;   (counsel-mode 1))

;; ;; (setq ivy-re-builders-alist
;; ;;       '((swiper . ivy--regex)
;; ;;         (t      . ivy--regex-fuzzy)))

;; (setq ivy-re-builders-alist
;;       '((t . ivy--regex-plus)))


;; (use-package ivy-rich
;;   :config
;;   (setq ivy-rich-display-transformers-list
;;         (plist-put ivy-rich-display-transformers-list
;;                    'ivy-switch-buffer
;;                    '(:columns
;;                      ((ivy-switch-buffer-transformer (:width 40))
;;                       (ivy-rich-switch-buffer-project
;;                        (:width 15 :face success))
;;                       (ivy-rich-switch-buffer-path
;;                        (:width (lambda (x)
;;                                  (ivy-rich-switch-buffer-shorten-path
;;                                   x (ivy-rich-minibuffer-width 0.3))))))
;;                      :predicate (lambda (cand) (get-buffer cand)))))
;;   (ivy-rich-mode 1))


;; (use-package ivy-prescient
;;   :config
;;   (ivy-prescient-mode 1)
;;   (prescient-persist-mode 1))

;; ;; (use-package ivy-posframe)

;; ;; (require 'ivy-posframe)
;; ;; ;; display at `ivy-posframe-style'
;; ;; ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
;; ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
;; ;; ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
;; ;; ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-bottom-left)))
;; ;; ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-bottom-left)))
;; ;; ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
;; ;; (ivy-posframe-mode 1)
;; ;; (setq ivy-posframe-height-alist '((swiper . 20)
;; ;;                                   (t      . 20)))

;; ;; (setq ivy-posframe-display-functions-alist
;; ;;       '((swiper          . ivy-display-function-fallback)
;; ;; 	(counsel-rg          . ivy-display-function-fallback)
;; ;; 	(ivy-xref-show-xrefs          . ivy-display-function-fallback)
;; ;; 	(ivy-xref-show-defs          . ivy-display-function-fallback)
;; ;;         (complete-symbol . ivy-posframe-display-at-point)
;; ;;         ;; (counsel-M-x     . ivy-posframe-display-at-window-bottom-left)
;; ;;         (t               . ivy-posframe-display-at-frame-center)
;; ;; 	))

;; (use-package ivy-xref
;;   :init
;;   ;; xref initialization is different in Emacs 27 - there are two different
;;   ;; variables which can be set rather than just one
;;   (when (>= emacs-major-version 27)
;;     (setq xref-show-definitions-function #'ivy-xref-show-defs))
;;   ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
;;   ;; commands other than xref-find-definitions (e.g. project-find-regexp)
;;   ;; as well
;;   (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

;; ;; (setf (alist-get 'counsel-rg ivy-re-builders-alist) #'ivy--regex-plus) ;; Second, overwrite ivy-prescient-re-builder by ivy--regex-plusp
;; ;; (use-package flx)

;; ;; (global-set-key (kbd "C-s") 'swiper-isearch)
;; (global-set-key (kbd "M-x") 'counsel-M-x)
;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;; (global-set-key (kbd "M-y") 'counsel-yank-pop)
;; (global-set-key (kbd "<f1> f") 'counsel-describe-function)
;; (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;; (global-set-key (kbd "<f1> l") 'counsel-find-library)
;; (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;; (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;; (global-set-key (kbd "<f2> j") 'counsel-set-variable)
;; (global-set-key (kbd "C-x b") 'ivy-switch-buffer)
;; (global-set-key (kbd "C-c v") 'ivy-push-view)
;; (global-set-key (kbd "C-c V") 'ivy-pop-view)


(global-set-key (kbd "M-s ,") 'json-reformat-region)
(global-set-key (kbd "M-s l") 'vc-refresh-state)

;; (use-package goto-chg
;;   :bind
;;   ("C-c g" . goto-last-change))


;; (use-package multiple-cursors
;;   :bind
;;   (("C-c n" . mc/mark-next-like-this)
;;    ("C-c p" . mc/mark-previous-like-this)))

;; (defun remap-faces-default-attributes ()
;;    (let ((family (face-attribute 'default :family))
;;          (height (face-attribute 'default :height)))
;;      (mapcar (lambda (face)
;;               (face-remap-add-relative
;;                face :family family :weight 'normal :height height))
;;           (face-list))))

;; ( (display-graphic-p)
;;    (add-hook 'minibuffer-setup-hook 'remap-faces-default-attributes)
;;    (add-hook 'change-major-mode-after-body-hook 'remap-faces-default-attributes))


;; keep this at last or load after theme reloads
;; disable bold face everywhere, done mainly for cozette
(set-face-bold-p 'bold nil)
;; (set-face-italic-p 'italic nil)

(setq x-stretch-cursor nil)
(setq visible-bell nil)


;; https://github.com/raxod502/selectrum/wiki/Additional-Configuration
;; (defun multi-occur (bufs regexp &optional nlines)
;;   (interactive (cons
;;                 (mapcar #'get-buffer
;;                         (completing-read-multiple "Buffer: "
;;                                                   #'internal-complete-buffer))
;;                 (occur-read-primary-args)))
;;   (occur-1 regexp nlines bufs))

;; (setq mouse-wheel-progressive-speed nil)
;; (setq mouse-wheel-follow-mouse 't)
;; keeps cursor at bottom only instead of centering to screen.
(setq scroll-step 1)

;; ;; battery indicator
;; (setq battery-mode-line-format " [%b%p%%]")
;; (setq battery-mode-line-limit 95)
;; (setq battery-update-interval 180)
;; (setq battery-load-low 20)
;; (setq battery-load-critical 10)
;; (add-hook 'after-init-hook #'display-battery-mode)

;; scroll behaviour
(setq-default scroll-preserve-screen-position t)
(setq-default scroll-conservatively 1) ; affects `scroll-step'
(setq-default scroll-margin 0)
(setq-default next-screen-context-lines 0)

;; ;; icicles config
;; (add-to-list 'load-path "~/.emacs.d/icicles/")
;; (require 'icicles)
;; (icy-mode -1)

;; (use-package shackle)
;; ;; not working, should check docs
;; (setq shackle-rules '(
;;                    ("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*" :popup t :size 0.4)
;;                    ))


;; custom keybindings
(global-set-key (kbd "s-q") 'delete-window)
(global-set-key (kbd "s-SPC") 'replace-string)
(global-set-key (kbd "s-e") 'project-find-file)
;; (global-set-key (kbd "s-e") 'projectile-find-file)
(global-set-key (kbd "s-w") 'project-switch-to-buffer)
;; (global-set-key (kbd "s-w") 'projectile-switch-to-buffer)
(global-set-key (kbd "s-g") 'lsp-find-definition)
(global-set-key (kbd "s-c") 'lsp-find-references)
(global-set-key (kbd "s-u") 'magit-file-dispatch)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "s-v") 'tab-next)
(global-set-key (kbd "s-x") 'tab-previous)
(global-set-key (kbd "s-o") 'other-window)
(global-set-key (kbd "s-<right>") 'next-buffer)
(global-set-key (kbd "s-<left>") 'previous-buffer)
(global-set-key (kbd "s-g") 'next-buffer)
(global-set-key (kbd "s-d") 'previous-buffer)
;; (global-set-key (kbd "s-b") 'counsel-switch-buffer)
;; (global-set-key (kbd "s-b") 'consult-buffer)
(global-set-key (kbd "s-b") 'helm-mini)
(global-set-key (kbd "s-a") 'evil-normal-state)
(global-set-key "\M-["  (lambda () (interactive) (scroll-down   1)) )
(global-set-key "\M-]"  (lambda () (interactive) (scroll-up 1)) )
;; (global-set-key (kbd "C-c C-p") 'wgrep-change-to-wgrep-mode)

;; https://www.reddit.com/r/emacs/comments/e2u5n9/code_folding_with_outlineminormode/
;; ;; Clean code folding via Outline minor mode.
;; (add-hook 'prog-mode-hook 'outline-minor-mode)
;; (add-hook 'text-mode-hook 'outline-minor-mode)

(use-package ediff
  :config
  (setq ediff-window-setup-function #'ediff-setup-windows-plain)
  (setq ediff-split-window-function #'split-window-horizontally)
  )

;; code folding
;; https://github.com/tarsius/bicycle

;; (use-package battery-notifier)
;; (battery-notifier-mode 1)

;; (add-hook 'battery-notifier-capacity-critical-hook
;;           (lambda () (call-process-shell-command "systemctl suspend")))

;; (use-package color-theme-sanityinc-tomorrow)

;; This overrides the default mark-in-region with a prettier-looking one,
;; and provides a couple extra commands
;; (use-package visual-regexp
;;   :ryo
;;   ("s" vr/mc-mark)
;;   ("?" vr/replace)
;;   ("M-/" vr/query-replace))

;; Emacs incremental search doesn't work with multiple cursors, but this fixes that
;; (use-package phi-search
;;   :bind (("C-s" . phi-search)
;;          ("C-r" . phi-search-backward)))

;; https://sachachua.com/dotemacs/
(defun my/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

      Move point to the first non-whitespace character on this line.
      If point is already there, move to the beginning of the line.
      Effectively toggle between the first non-whitespace character and
      the beginning of the line.

      If ARG is not nil or 1, move forward ARG - 1 lines first.  If
      point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'my/smarter-move-beginning-of-line)

;; (use-package solaire-mode
;;   :config
;;   (solaire-global-mode))

;; (use-package solaire-mode
;;   :custom (solaire-mode-remap-fringe t)
;;   :config (solaire-global-mode))

(use-package hydra)

;; https://github.com/alphapapa/unpackaged.el#smerge-mode
(use-package smerge-mode
  :config
  (defhydra unpackaged/smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (unpackaged/smerge-hydra/body)))))

(use-package pyimport)
;; (define-key python-mode-map (kbd "C-c C-i") #'pyimport-insert-missing)
(global-set-key (kbd "C-c C-i") 'pyimport-insert-missing)

;; (use-package smart-mode-line)
;; (sml/setup)

;; (use-package dimmer
;;   :config
;;   (setq dimmer-fraction 0.3)
;;   (setq dimmer-adjustment-mode :foreground)
;;   (setq dimmer-use-colorspace :rgb)

;;   (dimmer-mode 1))

;; (use-package evil
;;   :init
;;   (setq evil-want-keybinding nil)
;;   (setq evil-want-C-u-scroll t)
;;   :config
;;  (evil-mode 1)
;;  ;; (define-key evil-insert-state-map "kj" 'evil-normal-state)
;; )

;; (setq evil-normal-state-cursor '("firebrick" box))
;; (setq evil-emacs-state-cursor '("black" box))
;; (use-package evil-collection
;;   :after evil
;;   :config
;;   (evil-collection-init))

;; ;; ;; set leader key in all states
;; (evil-set-leader nil (kbd "C-SPC"))

;; ;; set leader key in normal state
;; (evil-set-leader 'normal (kbd "SPC"))

;; ;; ;; set local leader
;; (evil-set-leader 'normal "," t)
;; (evil-define-key 'normal 'global (kbd "<leader>fs") 'save-buffer)
;; (evil-define-key 'normal 'global (kbd "<leader>gg") 'magit-status)
;; (evil-define-key 'normal 'global (kbd "gr") 'lsp-find-references)
;; (evil-define-key 'normal 'global (kbd "<leader>bb") 'consult-buffer)
;; (evil-define-key 'normal 'global (kbd "<leader>ff") 'project-find-file)
;; (evil-define-key 'normal 'global (kbd "<leader>fb") 'project-switch-to-buffer)

;; (use-package general)
;; (require 'general)
;; (general-evil-setup)
;; (general-imap "j"
;;               (general-key-dispatch 'self-insert-command
;;                 "k" 'evil-normal-state))

;; (use-package ws-butler)
;; (add-hook 'prog-mode-hook #'ws-butler-mode)
;; (setq show-trailing-whitespace t)

;; ;; (global-whitespace-mode)
;; ;; (setq whitespace-style '(face trailing lines tabs big-indent))
;; (setq whitespace-style '(face trailing tabs))
;; (custom-set-faces
;;  '(whitespace-tab ((t (:background "red")))))
;; (global-whitespace-mode)

(setq-default indicate-empty-lines t)
(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)
;; (global-set-key (kbd "C-c e") 'python-black-buffer)


;; (use-package key-chord
;;   :config(key-chord-mode 1))
;; (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)

;; (use-package origami)
;; (require 'origami)
;; (global-set-key (kbd "C-c t t") 'origami-toggle-node)
;; (global-set-key (kbd "C-c t a") 'origami-toggle-all-nodes)
;; (global-set-key (kbd "C-c t n") 'origami-show-only-node)
;; (global-set-key (kbd "C-c t c") 'origami-close-node)
;; (global-set-key (kbd "C-c t o") 'origami-open-node)
;; (global-set-key (kbd "C-c t O") 'origami-open-all-nodes)
;; (global-set-key (kbd "C-c t C") 'origami-close-all-nodes)

;; (use-package fold-this)
;; (require 'fold-this)

;; (global-set-key (kbd "C-c C-f") 'fold-this-all)
;; (global-set-key (kbd "C-c C-F") 'fold-this)
;; (global-set-key (kbd "C-c M-f") 'fold-this-unfold-all)

;; (use-package realgud)

(use-package dap-mode)
(dap-mode 1)

(dap-ui-mode 1)
;; enables mouse hover support
(dap-tooltip-mode 1)
;; use tooltips for mouse hover
;; if it is not enabled `dap-mode' will use the minibuffer.
(tooltip-mode 1)
;; displays floating panel with debug buttons
;; requies emacs 26+
(dap-ui-controls-mode 1)

(require 'dap-python)



(global-set-key (kbd "C-c C-d a") 'dap-breakpoint-add)
(global-set-key (kbd "C-c C-d d") 'dap-breakpoint-delete)

(display-time-mode t)
;; (display-battery-mode t)
(use-package good-scroll)
(good-scroll-mode 1)
(setq ring-bell-function 'ignore)

;; https://ruzkuku.com/files/init.el.html

(use-package dogears
  ;; These bindings are optional, of course:
  :bind (:map global-map
              ("M-g d" . dogears-go)
              ("M-g M-b" . dogears-back)
              ("M-g M-f" . dogears-forward)
              ("M-g M-d" . dogears-list)
              ("M-g M-D" . dogears-sidebar)))

;; (use-package gumshoe
;;   :straight (gumshoe :type git
;;                      :host github
;;                      :repo "Overdr0ne/gumshoe"
;;                      :branch "master")
;;   :init
;;   ;; Enabing global-gumshoe-mode will initiate tracking
;;   (global-gumshoe-mode +1)
;;   ;; customize peruse slot display if you like
;;   (setf gumshoe-slot-schema '(time buffer position line))
;;   ;; personally, I use perspectives, so I use the provided extension
;;   ;; :after (perspective)
;;   ;; (global-gumshoe-persp-mode +1)
;;   ;; (setf gumshoe-slot-schema '(perspective time buffer position line))
;;   )
;; (global-set-key (kbd "M-g M-b") 'gumshoe-backtrack-back)
;; (global-set-key (kbd "M-g M-f") 'gumshoe-backtrack-forward)


;; http://ergoemacs.org/emacs/emacs_new_empty_buffer.html
(defun xah-new-empty-buffer ()
  "Create a new empty buffer.
New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.

It returns the buffer (for elisp programing).

URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2017-11-01"
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    $buf
    ))
(setq initial-major-mode (quote text-mode))
(global-set-key (kbd "C-c s") 'xah-new-empty-buffer)


(use-package hl-todo)
;; (setq hl-todo-keyword-faces
;;       '(("# TODO"   . "#FF0000")
;;         ("FIXME"  . "#FF0000")
;;         ("DEBUG"  . "#A020F0")
;;         ("GOTCHA" . "#FF4500")
;;         ("STUB"   . "#1E90FF")))
(setq global-hl-todo-mode t)

;; (use-package nano-modeline)
;; (use-package stripes)

(use-package bufler)
(bufler-mode t)
(global-set-key (kbd "C-x C-b") 'bufler-list)
(global-set-key (kbd "s-s") 'bufler-switch-buffer)


;; (use-package bespoke-modeline
;;   :straight (:type git :host github :repo "mclear-tools/bespoke-modeline") 
;;   :init
;;   ;; Set header line
;;   ;; (setq bespoke-modeline-position 'bottom)
;;   ;; Set mode-line height
;;   (setq bespoke-modeline-size 1)
;;   ;; Show diff lines in mode-line
;;   (setq bespoke-modeline-git-diff-mode-line t)
;;   ;; Set mode-line cleaner
;;   ;; (setq bespoke-modeline-cleaner t)
;;   ;; Use mode-line visual bell
;;   (setq bespoke-modeline-visual-bell t)
;;   :config
;;   (bespoke-modeline-mode))

(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

;;; init.el ends here
