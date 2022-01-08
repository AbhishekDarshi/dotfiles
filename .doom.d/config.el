;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
;;; Add to ~/.doom.d/config.el
;; test for font
;; a o i I l 1 m mm 0O Illegal1
(setq doom-font (font-spec :family "Fira Mono" :size 14 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "Fira Sans") ; inherits `doom-font''s :size
      doom-unicode-font (font-spec :family "Input Mono Narrow" :size 12)
      doom-big-font (font-spec :family "Fira Mono" :size 19))


;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'modus-vivendi)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled
(setq doom-theme 'doom-vibrant)
(setq doom-modeline-height 18)
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


(setq minions-mode-line-lighter ";")
;; NOTE: This will be expanded whenever I find a mode that should not
;; be hidden
(setq minions-direct (list 'defining-kbd-macro
                        'flycheck-mode
                        ;; 'flymake-mode
                        ))
(minions-mode 1)

;; (setq modus-themes-slanted-constructs nil
;;       modus-themes-bold-constructs nil
;;       modus-themes-fringes nil ; {nil,'subtle,'intense}
;;       ;; Options for `modus-themes-lang-checkers': nil,
;;       ;; 'straight-underline, 'subtle-foreground,
;;       ;; 'subtle-foreground-straight-underline, 'intense-foreground,
;;       ;; 'intense-foreground-straight-underline, 'colored-background
;;       modus-themes-lang-checkers 'straight-underline
;;       modus-themes-mode-line 'borderless-accented
;;       ;; Options for `modus-themes-syntax': nil, 'faint,
;;       ;; 'yellow-comments, 'green-strings,
;;       ;; 'yellow-comments-green-strings, 'alt-syntax,
;;       ;; 'alt-syntax-yellow-comments
;;       modus-themes-syntax 'alt-syntax
;;       modus-themes-hl-line 'accented-background
;;       modus-themes-paren-match 'intense ; {nil,'subtle-bold,'intense,'intense-bold}
;;       ;; Options for `modus-themes-links': nil, 'faint,
;;       ;; 'neutral-underline, 'faint-neutral-underline, 'no-underline,
;;       ;; 'underline-only
;;       modus-themes-links 'neutral-underline
;;       modus-themes-subtle-line-numbers t
;;       modus-themes-no-mixed-fonts nil
;;       modus-themes-prompts 'intense ; {nil,'subtle,'intense}
;;       modus-themes-completions 'opinionated ; {nil,'moderate,'opinionated}
;;       modus-themes-region 'bg-only ; {nil,'no-extend,'bg-only,'bg-only-no-extend}
;;       modus-themes-diffs 'fg-only ; {nil,'desaturated,'fg-only,'bg-only}
;;       modus-themes-org-blocks 'rainbow ; {nil,'grayscale,'rainbow}
;;       modus-themes-org-habit 'traffic-light ; {nil,'simplified,'traffic-light}
;;       modus-themes-variable-pitch-ui nil
;;       modus-themes-variable-pitch-headings nil
;;       modus-themes-scale-headings nil
;;       ;; modus-themes-success-deuteranopia nil
;;       modus-themes-scale-1 1.1
;;       modus-themes-scale-2 1.15
;;       modus-themes-scale-3 1.21
;;       modus-themes-scale-4 1.27
;;       modus-themes-scale-5 1.33)

(after! cc-mode
  (remove-hook 'python-mode-hook #'rainbow-delimiters-mode))

(after! cc-mode
  (remove-hook 'text-mode-hook #'rainbow-delimiters-mode))

(after! cc-mode
  (remove-hook 'elisp-mode-hook #'rainbow-delimiters-mode))

(use-package! modus-themes
;;  :straight (modus-themes :type git :host gitlab :repo "protesilaos/modus-themes")
  :init
  (setq modus-themes-slanted-constructs nil
        modus-themes-bold-constructs nil
        modus-themes-fringes 'subtle ; {nil,'subtle,'intense}
        ;; Options for `modus-themes-lang-checkers': nil,
        ;; 'straight-underline, 'subtle-foreground,
        ;; 'subtle-foreground-straight-underline, 'intense-foreground,
        ;; 'intense-foreground-straight-underline, 'colored-background
        modus-themes-lang-checkers 'straight-underline
        modus-themes-mode-line '(borderless)
        ;; Options for `modus-themes-syntax': nil, 'faint,
        ;; 'yellow-comments, 'green-strings,
        ;; 'yellow-comments-green-strings, 'alt-syntax,
        ;; 'alt-syntax-yellow-comments
        modus-themes-syntax 'faint
        modus-themes-hl-line 'accented-background
        modus-themes-paren-match 'intense ; {nil,'subtle-bold,'intense,'intense-bold}
        ;; Options for `modus-themes-links': nil, 'faint,
        ;; 'neutral-underline, 'faint-neutral-underline, 'no-underline,
        ;; 'underline-only
        modus-themes-links 'neutral-underline
        modus-themes-subtle-line-numbers nil
        modus-themes-no-mixed-fonts nil
        modus-themes-prompts 'intense ; {nil,'subtle,'intense}
        modus-themes-completions 'moderate ; {nil,'moderate,'opinionated}
        modus-themes-region 'no-extend ; {nil,'no-extend,'bg-only,'bg-only-no-extend}
        modus-themes-diffs 'fg-only ; {nil,'desaturated,'fg-only,'bg-only}
        modus-themes-org-blocks nil ; {nil,'grayscale,'rainbow}
        modus-themes-org-habit 'traffic-light ; {nil,'simplified,'traffic-light}
        ;; modus-themes-variable-pitch-ui nil
        ;; modus-themes-variable-pitch-headings nil
        ;; modus-themes-scale-headings nil
        ;; ;; modus-themes-success-deuteranopia nil
        ;; modus-themes-scale-1 1.1
        ;; modus-themes-scale-2 1.15
        ;; modus-themes-scale-3 1.21
        ;; modus-themes-scale-4 1.27
        ;; modus-themes-scale-5 1.33
        )
  ;; (load-theme 'modus-operandi t)
  ;; :config
  ;; (set-face-attribute 'cursor nil :background (modus-themes-color-alts 'blue 'yellow))
  :bind ("<f5>" . modus-themes-toggle))


(use-package! company-box
  :diminish ""
  :hook (company-mode . company-box-mode))

(use-package! company-prescient
  :config
  (company-prescient-mode +1))
(setq company-idle-delay nil)

(setq lsp-keymap-prefix "C-x l")
(setq lsp-eldoc-render-all nil)
(setq
 lsp-headerline-breadcrumb-enable nil
 lsp-enable-symbol-highlighting t
 lsp-modeline-diagnostics-enable nil
 lsp-lens-enable nil
 lsp-ui-sideline-enable nil
 lsp-modeline-code-actions-enable nil
 lsp-completion-show-kind nil
 )
(setq lsp-signature-auto-activate nil) ;; you could manually requiest them via `lsp-signature-activate`
(setq lsp-eldoc-enable-hover nil)
;; (setq lsp-eldoc-hook nil)
(setq lsp-enable-links nil)
(setq lsp-signature-render-documentation nil)
(setq lsp-ui-doc-enable nil)
(setq lsp-completion-enable-additional-text-edit nil)

(setq lsp-idle-delay 0.500)
(setq lsp-log-io nil) ; if set to true can cause a performance hit

(column-number-mode 1)

;; https://tecosaur.github.io/emacs-config/config.el

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "â€¦"                ; Unicode ellispis are nicer than "...", and also save /precious/ space
      password-cache-expiry nil                   ; I can trust my computers ... can't I?
      ;; scroll-preserve-screen-position 'always     ; Don't have `point' jump around
      scroll-margin 2)                            ; It's nice to maintain a little margin

(display-time-mode 1)

(display-battery-mode 1)

(use-package! lsp-pyright
  :hook
  (python-mode .
               (lambda ()
                 (setq indent-tabs-mode nil)
                 (setq tab-width 4)
                 (setq python-indent-offset 4)
                 (require 'lsp-pyright)
                 (lsp))))

(global-set-key (kbd "M-s ,") 'json-reformat-region)
(global-set-key (kbd "M-s l") 'vc-refresh-state)
(global-set-key (kbd "s-q") 'delete-window)
(global-set-key (kbd "s-e") 'counsel-projectile)
(global-set-key (kbd "s-w") 'projectile-switch-to-buffer)
(global-set-key (kbd "s-g") 'lsp-find-definition)
(global-set-key (kbd "s-r") 'lsp-find-references)
(global-set-key (kbd "s-u") 'magit-file-dispatch)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "s-v") 'tab-next)
(global-set-key (kbd "s-c") 'tab-previous)
(global-set-key (kbd "s-o") 'other-window)
(global-set-key (kbd "s-<right>") 'winner-redo)
(global-set-key (kbd "s-<left>") 'winner-undo)
(global-set-key (kbd "M-s r") '+ivy/project-search)
(global-set-key (kbd "s-l") 'next-buffer)
(global-set-key (kbd "s-h") 'previous-buffer)
(global-set-key (kbd "s-b") 'counsel-buffer-or-recentf)
(global-set-key "\M-p"  (lambda () (interactive) (scroll-down   1)) )
(global-set-key "\M-n"  (lambda () (interactive) (scroll-up 1)) )

(set-face-attribute 'mode-line nil :font "FiraSans" :height 100)
(set-face-attribute 'mode-line-inactive nil :font "FiraSans" :height 100)

(setq company-minimum-prefix-length 3)
(setq company-idle-delay 0.5)

(use-package! docker-tramp)

(global-visual-line-mode t)

;; (use-package! tree-sitter
;;   ;; :custom-face
;;   ;; (tree-sitter-hl-face:method.call   ((t (:inherit font-lock-function-name-face))))
;;   ;; (tree-sitter-hl-face:function.call ((t (:inherit font-lock-function-name-face))))
;;   ;; (tree-sitter-hl-face:operator      ((t (:inherit default))))
;;   ;; (tree-sitter-hl-face:type.builtin  ((t (:inherit font-lock-type-face))))
;;   ;; (tree-sitter-hl-face:number        ((t (:inherit highlight-numbers-number))))
;;   :config
;;   (global-tree-sitter-mode)
;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; (use-package! tree-sitter-langs)

;; keep this at last or load after theme reloads
;; disable bold face everywhere, done mainly for cozette
(set-face-bold-p 'bold nil)
;; (set-face-italic-p 'italic nil)

;; (use-package! rustic
;;   :bind (:map rustic-mode-map
;;               ;; ("M-j" . lsp-ui-imenu)
;;               ;; ("M-?" . lsp-find-references)
;;               ("C-c C-c l" . flycheck-list-errors)
;;               ("C-c C-c a" . lsp-execute-code-action)
;;               ("C-c C-c r" . lsp-rename)
;;               ("C-c C-c q" . lsp-workspace-restart)
;;               ("C-c C-c Q" . lsp-workspace-shutdown)
;;               ("C-c C-c s" . lsp-rust-analyzer-status))
;;   :config
;;   ;; uncomment for less flashiness
;;   (setq lsp-modeline-code-actions-enable nil)
;;   (setq lsp-eldoc-hook nil)
;;   (setq lsp-enable-symbol-highlighting nil)
;;   (setq lsp-signature-auto-activate nil)

;;   ;; comment to disable rustfmt on save
;;   ;; (setq rustic-format-on-save t)
;;   (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

;; (setq lsp-modeline-code-actions-enable nil)
;; (setq lsp-modeline-diagnostics-enable nil)

;; (defun rk/rustic-mode-hook ()
;;   ;; so that run C-c C-c C-r works without having to confirm
;;   (setq-local buffer-save-without-query t))


;; (setq ivy-posframe-parameters
;;       '((left-fringe . 0)
;;         (right-fringe . 0)))


;; uniquify changes conflicting buffer names from file<2> etc
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;;; :editor evil
;; Focus new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; gap between lang checker underline and text
(setq underline-minimum-offset 3)
