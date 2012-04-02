;;; rinari-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (rinari-minor-mode rinari-launch) "rinari" "rinari.el"
;;;;;;  (20296 37288))
;;; Generated autoloads from rinari.el

(autoload (quote rinari-launch) "rinari" "\
Run `rinari-minor-mode' if inside of a rails projecct,
otherwise turn `rinari-minor-mode' off if it is on.

\(fn)" t nil)

(defvar rinari-major-modes (if (boundp (quote rinari-major-modes)) rinari-major-modes (list (quote find-file-hook) (quote mumamo-after-change-major-mode-hook) (quote dired-mode-hook))) "\
Major Modes from which to launch Rinari.")

(dolist (hook rinari-major-modes) (add-hook hook (quote rinari-launch)))

(autoload (quote rinari-minor-mode) "rinari" "\
Enable Rinari minor mode providing Emacs support for working
with the Ruby on Rails framework.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("rinari-pkg.el") (20296 37288 354810))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; rinari-autoloads.el ends here
