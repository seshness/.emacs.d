;; Load all command in this file
;; I'm treating it like a header file

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))


;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

(defvar emacs-config-home "~/.emacs.d/")
(add-to-list 'load-path (expand-file-name emacs-config-home))
(add-to-list 'load-path
             (expand-file-name (concat emacs-config-home "plugins/")))

;; user specific settings (eg. name etc)
(if (file-readable-p "user.el")
    (load "user.el"))

;; Programming language specific customizations
(load "languages.el")

;; Load standard key combo changes custom keycombos
;; Special stuff not in
;; vanilla emacs are under their own files
(load "keycombos.el")

;; Visuals
(load "visuals.el")

;; Line numbers and general frame stuff
(load "frameinfo.el")

;; files and general customizations
(load "files.el")

;; General
(load "general.el")

;; Cool stuff: autocomplete, iwb, etc.
;; Typically other useful functions or libraries
(load "coolstuff.el")
