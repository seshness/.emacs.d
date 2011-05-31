;; Load all command in this file
;; I'm treating it like a header file

(defvar emacs-config-home "~/.emacs.d/")
(add-to-list 'load-path (expand-file-name emacs-config-home))

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
<<<<<<< HEAD


;; Add custom stuff here
(defvar master-dir (getenv "ADMIN_SCRIPTS"))
(load-library (concat master-dir "/master.emacs"))

(global-set-key "\C-xxb" 'git-blame-mode)
=======
>>>>>>> parent of 71333ac... Added a line for customizations that don't want to be on master
