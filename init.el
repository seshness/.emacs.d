;; Load all command in this file
;; I'm treating it like a header file

(add-to-list 'load-path (expand-file-name "."))

;; user specific settings (eg. name etc)
(load "user.el")

;; Load standard key combo changes
;; custom keycombos for special stuff not in vanilla emacs are under their own files
(load "./keycombos.el")

;; Visuals
(load "./visuals.el")

;; Line numbers and general frame stuff
(load "./frameinfo.el")

;; files and general customizations
(load "files.el")

;; General
(load "general.el")

;; Cool stuff: autocomplete, iwb, etc.
;; Typically other useful functions or libraries
(load "coolstuff.el")

