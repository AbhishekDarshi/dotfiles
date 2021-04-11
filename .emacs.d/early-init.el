(setq package-enable-at-startup t)



;; Allow loading from the package cache
(setq package-quickstart t)




(setq inhibit-splash-screen t)
(setq use-dialog-box t)                 ; only for mouse events
(setq use-file-dialog nil)

(setq inhibit-startup-echo-area-message "mars") ; read the docstring
(setq inhibit-startup-screen t)
(setq inhibit-startup-buffer-menu t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
;; (tooltip mode -1)
(menu-bar-mode -1)
;; display column number
(column-number-mode)
;; cursor changes
;; (blink-cursor-mode 1)
;; highlight current cursor line in buffer
(global-hl-line-mode t)
;; enable line numbers globally
(global-display-line-numbers-mode t)
(delete-selection-mode 1)
;; (setq blink-cursor-mode t)
;; set cursor type as horizontal bar
(setq-default cursor-type 'hbar)
