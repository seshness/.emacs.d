;; files.el

;; put autosave files (starting with #) somewhere sensible

;; (defvar autosave-dir
;;   (concat "/tmp/emacs_autosaves/" (user-login-name) "/"))
(defvar autosave-dir "~/emacs_autosaves/")
(make-directory autosave-dir t)

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
          (if buffer-file-name
              (concat "#" (file-name-nondirectory buffer-file-name) "#")
            (expand-file-name
             (concat "#%" (buffer-name) "#")))))

;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defvar backup-dir (concat "~/emacs_backups/"))
(setq backup-directory-alist (list (cons "." backup-dir)))

;; Set default tab width == 4 spaces
(setq default-tab-width 4)

;; Use spaces, not tabs, by default
(setq-default indent-tabs-mode nil)

;; Make scripts executable
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Set the TAGS file and ignore large file warning
(setq large-file-warning-threshold nil)

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name))
  (kill-new (file-truename buffer-file-name))
)
