;; visuals.el

;; Highlight current line
;; (global-hl-line-mode 1)
;; (set-face-background 'hl-line "lemon chiffon")

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
(if (not window-system)
    (menu-bar-mode 0))

