;; keycombos.el

;; Making doubly certain that the command key is set to Meta on a Mac
(if (eq system-type 'darwin)
    (progn (setq mac-command-key-is-meta t)
	   (setq mac-command-modifier 'meta)))

(global-set-key [S-return] 'newline-and-indent)

;; Auto fill mode
(setq-default fill-column 80)
(global-set-key (kbd "C-c q") 'auto-fill-mode)

;; Decrease mouse wheel jerkiness
(if (boundp 'mouse-wheel-scroll-amount)
    (setq mouse-wheel-scroll-amount '(0.0001)))

;; next / previous frame
(global-set-key (kbd "C-x n") 'next-multiframe-window)
(global-set-key (kbd "C-x p") 'previous-multiframe-window)

;; comment & uncomment the highlighted region
(global-set-key (kbd "C-x /") 'comment-or-uncomment-region)

;; Let's get some shell character love
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)

;; C-x C-d / C-x C-u respectively
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; enlarge window == C-x ^
;; adding shrink-window C-x %
(global-set-key (kbd "C-x %") 'shrink-window)