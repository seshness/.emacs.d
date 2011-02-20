;; visuals.el

;; Highlight current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "lemon chiffon")

;; <quote>
;; Turn off the confounded tool bar (PNH 8/20/2003)
;; </quote>
(if (functionp 'tool-bar-mode) (tool-bar-mode 0))

;; Show paren mode ()
(show-paren-mode t)

;; Program colours! So pretty :)
(global-font-lock-mode t)

;; Makes the mark temporary
(setq transient-mark-mode t)

(menu-bar-mode 1)