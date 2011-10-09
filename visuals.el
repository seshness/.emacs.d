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

;; show a menu only when running within X (save real estate when
;; in console)
(menu-bar-mode (if window-system 1 -1))

(require 'color-theme)
(require 'color-theme-wombat)
(require 'color-theme-desert)

;; Set your own colour theme here
(if (boundp window-system)
    (color-theme-calm-forest)
    (color-theme-wombat))

;; Show trailing whitespace and tabs
(set-face-background 'trailing-whitespace "#900000")
(setq-default show-trailing-whitespace t)
(add-hook 'font-lock-mode-hook 'highlight-tabs)
(defun highlight-tabs () ""
  (font-lock-add-keywords nil '(("[\t]+" (0 'trailing-whitespace t)))))

;; Have highlighting all the time
(global-font-lock-mode 1)

;; Default font fix for Cocoa Emacs on OS X
(set-frame-font "-apple-Monaco-medium-normal-normal-*-*-*-*-*-m-0-iso10646-1")

;; Rainbow parens
(require 'rainbow-parens)
(setq-default frame-background-mode 'light)
