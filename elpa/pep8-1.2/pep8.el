;;; pep8.el --- run the python pep8 checker putting hits in a grep buffer

;; Copyright (C) 2011 Darius Powell

;; Author: Darius Powell <dariusp686@gmail.com>
;; Version: 1.2
;; URL: http://bitbucket.org/dariusp686/emacs-pep8
;; Keywords: python, pep8, check, lint

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(defvar pep8-hist nil)

(defgroup pep8 nil
  "Run pep8 putting hits in a grep buffer."
  :group 'tools
  :group 'processes)

(defcustom pep8-cmd "pep8 --repeat"
  "The pep8 command."
  :type 'string
  :group 'pep8)

;;;###autoload
(defun pep8 ()
  (interactive)
  (let* ((cmd (read-shell-command "Command: " (concat pep8-cmd " " (file-name-nondirectory (or (buffer-file-name) ""))) 'pep8-hist))
         (null-device nil))
    (grep cmd)))

(provide 'pep8)

;;; pep8.el ends here
