;; coolstuff.el

(add-to-list 'load-path (expand-file-name (concat emacs-config-home
                                                  "plugins/")))

(add-to-list 'load-path "/Users/seshadri/.emacs.d/plugins")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "/Users/seshadri/.emacs.d/plugins/ac-dict")
(ac-config-default)

(global-auto-complete-mode t)
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

;; M-x iwb
(defun iwb ()
  "indent and untabify whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))
(global-set-key (kbd "C-M-|") 'iwb)

;; ido-mode
(require 'ido)
(ido-mode t)

;; tramp mode ssh by default
(setq tramp-default-method "ssh")
(auto-compression-mode 1)

;; Check disk to see if files have been updated
(global-auto-revert-mode 1)

;; M-x insert-timestamp
(defun insert-timestamp ()
  "timestamp insertion function."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S: ")))


;; Revert all buffers
(defun revert-all-buffers ()
  (interactive)
  (let ((current-buffer (buffer-name)))
    (loop for buf in (buffer-list)
          do
          (unless (null (buffer-file-name buf))
            (switch-to-buffer (buffer-name buf))
            (revert-buffer nil t)))
    (switch-to-buffer current-buffer)
    (message "All buffers reverted!")))

;; Autocomplete 'on steroids'
(require 'pabbrev)

;; git (try `git-blame-mode' and `(global-set-key "\C-xxb" 'git-blame-mode)')
(require 'git)
(setq git-blame-log-message-format "format:%an (%ar): %s (%h) ")
(autoload 'git-blame-mode "git-blame"
  "Minor mode for incremental blame for Git." t)

(require 'git-blame)
(require 'git)
(setq git-blame-log-message-format "format:%an (%ar): %s (%h) ")
(autoload 'git-blame-mode "git-blame"
  "Minor mode for incremental blame for Git." t)

;; unique buffer names using path
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; nyan-mode
(add-to-list 'load-path (expand-file-name (concat emacs-config-home
                                                  "plugins/nyan-mode")))
(require 'nyan-mode)

(autoload 'sese-mode "sese" "Subtitle Editor major mode" t)
(setq auto-mode-alist
      (cons '("\\.sese\\'" . sese-mode) auto-mode-alist))
