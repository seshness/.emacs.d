;; general.el

;; s/yes/y and s/no/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Decrease mouse wheel jerkiness
(if (boundp 'mouse-wheel-scroll-amount)
    (setq mouse-wheel-scroll-amount '(0.0001)))

;; Default fill column: 80 characters
(setq-default fill-column 80)
