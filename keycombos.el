;; keycombos.el

;; Making doubly certain that the command key is set to Meta on a Mac
(if (eq system-type 'darwin)
    (progn (setq mac-command-key-is-meta t)
           (setq mac-command-modifier 'meta)
           ;;(setq mac-option-key-is-meta nil)
           ;;(setq mac-option-modifier nil)
           ))

(global-set-key [S-return] 'newline-and-indent)

;; Auto fill mode
(global-set-key (kbd "C-c q") 'auto-fill-mode)

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

;; goto line
(global-set-key (kbd "M-g") 'goto-line)

;; compile: C-c C-e
(global-set-key "\^c\^e" 'compile)

;; query-replace-regexp
(global-set-key (kbd "C-M-S-%") 'query-replace-regexp)

;; M-x revert-buffer
(global-set-key (kbd "C-c v") 'revert-buffer)

;; Mac-style window switching if not in terminal mode
(defun switch-to-next-frame ()
  (interactive)
  (other-frame 1))
(defun switch-to-prev-frame ()
  (interactive)
  (other-frame -1))
(if (boundp 'window-system)
    (progn (if window-system
	       (global-set-key (kbd "M-`") 'switch-to-next-frame))
	   (if window-system
	       (global-set-key (kbd "M-~") 'switch-to-prev-frame))))

;; Add custom stuff here
