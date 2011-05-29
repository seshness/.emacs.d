;; general.el

;; s/yes/y/g and s/no/n/g
(fset 'yes-or-no-p 'y-or-n-p)

;; Decrease mouse wheel jerkiness
(if (boundp 'mouse-wheel-scroll-amount)
    (setq mouse-wheel-scroll-amount '(0.0001)))

;; Default fill column: 80 characters
(setq-default fill-column 80)

;; Disable copy on mouse drag region
(setq-default mouse-drag-copy-region nil)

;; Blinking cursor
(blink-cursor-mode t)

;; popup a speedbar
;; (when window-system ; not just X
;;   (speedbar 1))

;; Put special stuff in their own frames
(setq special-display-buffer-names
      (nconc '("*Backtrace*" "*VC-log*" "*compilation*" "*grep*")
             special-display-buffer-names))

;; keyboard scroll one line at a time
(setq scroll-step 1)
