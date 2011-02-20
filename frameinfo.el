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
(if window-system
    (progn (setq-default left-fringe-width 5)
           (setq-default right-fringe-width 0)))

