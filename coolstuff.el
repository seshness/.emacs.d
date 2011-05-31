;; coolstuff.el

;; auto-complete, with a nice pulldown thingy
;; requires the ./autocomplete/ folder to be present
(add-to-list 'load-path (concat emacs-config-home "/autocomplete/"))
(require 'auto-complete)
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

;; Add custom stuff here

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
