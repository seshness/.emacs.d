;; Load all command in this file
;; I'm treating it like a header file

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.

;; Uncomment for Emacs < 24
;; (when
;;     (load
;;      (expand-file-name "~/.emacs.d/elpa/package.el"))
;;   (package-initialize))


;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.

;; Uncomment for Emacs < 24
;; (when
;;     (load
;;      (expand-file-name "~/.emacs.d/elpa/package.el"))
;;   (package-initialize))

(when (>= emacs-major-version 24)
  (package-initialize))

(defvar emacs-config-home "~/.emacs.d/")
(add-to-list 'load-path (expand-file-name emacs-config-home))
(add-to-list 'load-path
             (expand-file-name (concat emacs-config-home "plugins/")))

;; user specific settings (eg. name etc)
(if (file-readable-p "user.el")
    (load-library "user.el"))

;; Programming language specific customizations
(load-library "languages.el")

;; Load standard key combo changes custom keycombos
;; Special stuff not in
;; vanilla emacs are under their own files
(load-library "keycombos.el")

;; Visuals
(load-library "visuals.el")

;; Line numbers and general frame stuff
(load-library "frameinfo.el")

;; files and general customizations
(load-library "files.el")

;; General
(load-library "general.el")

;; Cool stuff: autocomplete, iwb, etc.
;; Typically other useful functions or libraries
(load-library "coolstuff.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-indent-next-pair-timer-interval (quote ((emacs-lisp-mode 1.5) (default 0.0005))))
 '(blink-cursor-mode t)
 '(column-number-mode t)
 '(display-battery-mode t)
 '(display-time-mode t)
 '(inhibit-startup-screen t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(size-indication-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'scroll-left 'disabled nil)
