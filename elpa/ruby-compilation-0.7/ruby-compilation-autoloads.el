;;; ruby-compilation-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (ruby-compilation-this-buffer ruby-compilation-cap
;;;;;;  ruby-compilation-rake ruby-compilation-run pcomplete/cap
;;;;;;  pcomplete/rake) "ruby-compilation" "ruby-compilation.el"
;;;;;;  (20296 37287))
;;; Generated autoloads from ruby-compilation.el

(autoload (quote pcomplete/rake) "ruby-compilation" "\
Not documented

\(fn)" nil nil)

(autoload (quote pcomplete/cap) "ruby-compilation" "\
Not documented

\(fn)" nil nil)

(autoload (quote ruby-compilation-run) "ruby-compilation" "\
Run a ruby process dumping output to a ruby compilation buffer.

\(fn CMD)" t nil)

(autoload (quote ruby-compilation-rake) "ruby-compilation" "\
Run a rake process dumping output to a ruby compilation buffer.

\(fn &optional EDIT TASK ENV-VARS)" t nil)

(autoload (quote ruby-compilation-cap) "ruby-compilation" "\
Run a capistrano process dumping output to a ruby compilation buffer.

\(fn &optional EDIT TASK ENV-VARS)" t nil)

(autoload (quote ruby-compilation-this-buffer) "ruby-compilation" "\
Run the current buffer through Ruby compilation.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("ruby-compilation-pkg.el") (20296 37287
;;;;;;  591470))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ruby-compilation-autoloads.el ends here
