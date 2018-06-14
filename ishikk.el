;;; ishikk.el --- Calendar for the weary fisher. -*- lexical-binding: t -*-

;; Copyright (C) 2018 Radon Rosborough

;; Author: Radon Rosborough <radon.neon@gmail.com>
;; Created: 12 Jun 2018
;; Homepage: https://github.com/raxod502/ishikk
;; Keywords: calendar
;; Package-Requires: ((emacs "26"))
;; Version: 1.0

;;; Code:

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

(defgroup ishikk nil
  "Calendar for the weary fisher."
  :prefix "ishikk-"
  :group 'applications
  :link '(url-link :tag "GitHub" "https://github.com/raxod502/ishikk")
  :link '(emacs-commentary-link :tag "Commentary" "ishikk"))

(defcustom ishikk-virtualenv-location
  (expand-file-name "var/ishikk/virtualenv/" user-emacs-directory)
  "Where to place the Python virtualenv for Ishikk.
The default value follows the conventions of the `no-littering'
package."
  :type 'directory)

(defcustom ishikk-package-location
  (file-name-directory (or load-file-name buffer-file-name))
  "The directory containing the Ishikk package files."
  :type 'directory)

(defcustom ishikk-virtualenv-setup-buffer "*ishikk-virtualenv-setup*"
  "The name of the buffer used by `ishikk-virtualenv-setup'."
  :type 'string)

(defun ishikk--virtualenv-python ()
  "Return the path to the Python executable in `ishikk-virtualenv-location'."
  (expand-file-name "bin/python" ishikk-virtualenv-location))

(defun ishikk--call-process-and-insert (program &rest args)
  "Insert command into current buffer, and invoke with output to that buffer.
PROGRAM and ARGS as in `call-process'. Return non-nil if the
command succeeds."
  (insert "$ " (mapconcat #'shell-quote-argument (cons program args) " ")
          "\n")
  (condition-case _
      (let ((rv (apply #'call-process program nil t t args)))
        (or (= 0 rv)
            (prog1 nil
              (unless (= (char-before) ?\n)
                (insert "\n"))
              (insert (format "[Command failed: code %S]" rv)))))
    (file-missing
     (unless (= (char-before) ?\n)
       (insert "\n"))
     (insert (format "[No such program: %S]" program)))))

(defun ishikk-virtualenv-setup ()
  (interactive)
  (message "Setting up Ishikk virtualenv...")
  (unless (executable-find "python3")
    (user-error
     "Failed to create virtualenv; could not find `python3' on $PATH"))
  (when (or (file-exists-p ishikk-virtualenv-location)
            (file-symlink-p ishikk-virtualenv-location))
    (delete-directory ishikk-virtualenv-location 'recursive 'trash))
  (when-let ((buf (get-buffer ishikk-virtualenv-setup-buffer)))
    (kill-buffer buf))
  (with-current-buffer (get-buffer-create ishikk-virtualenv-setup-buffer)
    (unless (and (ishikk--call-process-and-insert
                  "python3" "-m" "venv" ishikk-virtualenv-location)
                 (ishikk--call-process-and-insert
                  (ishikk--virtualenv-python) "-m"
                  "pip" "install" "-e" ishikk-package-location))
      (user-error "Failed to create virtualenv; see buffer %S for details"
                  ishikk-virtualenv-setup-buffer)))
  (message "Setting up Ishikk virtualenv...done"))

;;;; Closing remarks

(provide 'ishikk)

;;; ishikk.el ends here

;; Local Variables:
;; outline-regexp: ";;;;* "
;; End:
