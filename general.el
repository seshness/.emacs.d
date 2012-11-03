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

;; Put special stuff in a dedicated frame
(if (boundp 'window-system)
    (setq special-display-buffer-names
          (nconc '("*Backtrace*" "*VC-log*" "*compilation*" "*grep*")
                 special-display-buffer-names)))

;; keyboard scroll one line at a time
(setq scroll-step 1)

;; Split horizontally by default
(setq split-width-threshold 0)

;; Rebalance windows after splitting horizontally
(defadvice split-window-horizontally
  (after rebalance-windows activate)
  (balance-windows))
(ad-activate 'split-window-horizontally)

(defadvice delete-window
  (after rebalance-windows activate)
  (balance-windows))
(ad-activate 'delete-window)


;; byte compile emacs files
(defun recompile-emacs ()
  (interactive)
  (byte-recompile-directory "~/.emacs.d/")
  )

;; translates ANSI colors into text-properties, for eshell
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; show trailing whitespace ...
(set-face-background 'trailing-whitespace "#900000")
(setq-default show-trailing-whitespace t)
;; ... and terminate with extreme prejudice
(defun delete-trailing-whitespace-sometimes () ""
  (if (not (eq major-mode 'diff-mode))
      (delete-trailing-whitespace)))
(add-hook 'write-file-hooks 'delete-trailing-whitespace-sometimes)

;; OS X ls doesn't support ls --dired
(when (eq system-type 'darwin)
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))

;; Remove splash screen
(setq inhibit-splash-screen t)
