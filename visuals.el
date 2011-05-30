;; visuals.el

(add-to-list 'load-path 
	     (concat emacs-config-home "/visuals/"))

;; Highlight current line
;;(global-hl-line-mode 1)
;;(set-face-background 'hl-line "grey30")

;; <quote>
;; Turn off the confounded tool bar (PNH 8/20/2003)
;; </quote>
(if (functionp 'tool-bar-mode) (tool-bar-mode 0))

;; Show paren mode ()
(show-paren-mode t)
;; Paren mode options
(setq show-paren-style 'parenthesis)
(setq show-paren-delay 0)

;; Program colours! So pretty :)
(global-font-lock-mode t)

;; Makes the mark temporary
(setq transient-mark-mode t)

;; Disable menu bar in terminal-mode emacs
(if (boundp 'window-system)
    (if (not window-system)
	(menu-bar-mode 0)))

(require 'color-theme)

;; Set your own colour theme here
(color-theme-calm-forest)

;; Add custom stuff here
