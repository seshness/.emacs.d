;; frameinfo.el

;; Global linum-mode, if it exists
(if (boundp 'global-linum-mode)
    (global-linum-mode 1))

;; I need to see line numbers in the status bar!
(line-number-mode 1)
;; Display (L,C) in status bar where L is the current line number and C is the
;; column number, when used in conjunction with the above instruction
(column-number-mode 0)

;; Reduce the size of the left & right fringe
;; (set-window-fringes nil 5 0 nil)
(if (and (boundp 'window-system) window-system)
    (progn (setq-default left-fringe-width 5)
           (setq-default right-fringe-width 0)))


;; set window size
(if (boundp 'window-system)
    (progn (if window-system
	       (set-frame-height (selected-frame) 50))
	   (if window-system
	       (set-frame-width (selected-frame) 150))))

;; set emacs window title to "{$BUFFER} - emacs"
(setq frame-title-format "%b - emacs")
