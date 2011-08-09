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

;; Set your own colour theme here
(color-theme-calm-forest)

(set-face-attribute 'default nil :height 80)

;; Show trailing whitespace and tabs
(set-face-background 'trailing-whitespace "#900000")
(setq-default show-trailing-whitespace t)
(add-hook 'font-lock-mode-hook 'highlight-tabs)
(defun highlight-tabs () "" (font-lock-add-keywords nil '(("[\t]+" (0 'trailing-whitespace t)))))

(require 'color-theme)
(custom-set-faces
 '(font-lock-builtin-face ((t (:foreground "cyan"))))
 '(font-lock-keyword-face ((t (:foreground "cyan"))))
 '(font-lock-type-face ((t (:foreground "cyan"))))
 '(font-lock-comment-face ((t (:foreground "green"))))
 '(font-lock-constant-face ((t (:foreground "cyan"))))
 '(font-lock-function-name-face ((t (:foreground "magenta"))))
 '(font-lock-variable-name-face ((t (:foreground "yellow"))))
 '(font-lock-string-face ((t (:foreground "white"))))
 '(font-lock-warning-face ((t (:foreground "red"))))
 '(isearch ((t (:background "blue" :foreground "white"))))
 '(isearch-lazy-highlight-face ((t (:background "yellow" :foreground "black"))))
 '(makefile-space-face ((t (:background "red"))))
 '(show-paren-match-face ((((class color)) (:background "green"))))
 '(show-paren-mismatch-face ((((class color)) (:background "red"))))
)

;; Have highlighting all the time
(global-font-lock-mode 1)
