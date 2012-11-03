;; frameinfo.el

;; Global linum-mode, if it exists
(when (and (boundp 'global-linum-mode)
         (boundp 'window-system))
  (global-linum-mode 1)
  ;; Weird fringe-rendering issues with OS X GUI Emacs
  (setq linum-format " %d "))

;; Display (L,C) in status bar where L is the current line number and C
;; is the column number.
(line-number-mode 1)
(column-number-mode 1)

(size-indication-mode 1)
(display-time-mode 1)
(display-battery-mode 1)

;; Reduce the size of the left & right fringe
;; (set-window-fringes nil 5 0 nil)
(if (and (boundp 'window-system) window-system)
    (progn (setq-default left-fringe-width 0)
           (setq-default right-fringe-width 0)))

;; set window size
;; (if (boundp 'window-system)
;;     (progn (if window-system
;;                (set-frame-height (selected-frame) 50))
;;            (if window-system
;;                (set-frame-width (selected-frame) 150))))

;; set emacs window title to "{$BUFFER} - emacs"
(setq frame-title-format "%b - emacs")
