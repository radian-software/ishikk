;;; ishikk.el --- Calendar for the weary fisher. -*- lexical-binding: t -*-

;; Copyright (C) 2018-2022 Radian LLC <contact+ishikk@radian.codes>

;; Author: Radian LLC <contact+ishikk@radian.codes>
;; Created: 12 Jun 2018
;; Homepage: https://github.com/radian-software/ishikk
;; Keywords: calendar
;; Package-Requires: ((emacs "26"))
;; Version: 1.0

;;; Code:

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

(eval-when-compile
  (require 'cl-macs))

(require 'json)

(defgroup ishikk nil
  "Calendar for the weary fisher."
  :prefix "ishikk-"
  :group 'applications
  :link '(url-link :tag "GitHub" "https://github.com/radian-software/ishikk")
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

(defcustom ishikk-process-buffer "*ishikk-process*"
  "The name of the buffer in which Ishikk runs external commands.
This is useful for debugging."
  :type 'string)

(defun ishikk--virtualenv-python ()
  "Return the path to the Python executable in `ishikk-virtualenv-location'."
  (expand-file-name "bin/python" ishikk-virtualenv-location))

(defmacro ishikk--with-process-buffer (&rest body)
  "Kill and recreate `ishikk-process-buffer', then evaluate BODY in it."
  (declare (indent 0))
  `(progn
     (when-let ((buf (get-buffer ishikk-process-buffer)))
       (kill-buffer buf))
     (with-current-buffer (get-buffer-create ishikk-process-buffer)
       ,@body)))

(defvar-local ishikk--output-start-marker nil
  "Marker for the beginning of process output.
This is set in `ishikk-process-buffer' by
`ishikk--call-process-and-insert'.")

(defvar-local ishikk--output-end-marker nil
  "Marker for the end of process output.
This is set in `ishikk-process-buffer' by
`ishikk--call-process-and-insert'.")

(defun ishikk--call-process-and-insert (program &rest args)
  "Insert command into current buffer, and invoke with output to that buffer.
PROGRAM and ARGS as in `call-process'. Set
`ishikk--output-start-marker' and `ishikk--output-end-marker' to
demarcate the command output. Return non-nil if the command
succeeds."
  (insert "$ " (mapconcat #'shell-quote-argument (cons program args) " ")
          "\n")
  (condition-case _
      (progn
        (setq ishikk--output-start-marker (point-marker))
        (let ((rv (apply #'call-process program nil t t args)))
          (setq ishikk--output-end-marker (point-marker))
          (or (= 0 rv)
              (prog1 nil
                (unless (= (char-before) ?\n)
                  (insert "\n"))
                (insert (format "[Command failed: code %S]" rv))))))
    (file-missing
     (setq ishikk--output-end-marker (point-marker))
     (unless (= (char-before) ?\n)
       (insert "\n"))
     (insert (format "[No such program: %S]" program)))))

(defun ishikk-virtualenv-setup ()
  "Create the Ishikk virtualenv in `ishikk-virtualenv-location'.
If one already exists, delete it first."
  (interactive)
  (message "Setting up Ishikk virtualenv...")
  (unless (executable-find "python3")
    (user-error
     "Failed to create virtualenv; could not find `python3' on $PATH"))
  (when (or (file-exists-p ishikk-virtualenv-location)
            (file-symlink-p ishikk-virtualenv-location))
    (delete-directory ishikk-virtualenv-location 'recursive 'trash))
  (ishikk--with-process-buffer
    (unless (and (ishikk--call-process-and-insert
                  "python3" "-m" "venv" ishikk-virtualenv-location)
                 (ishikk--call-process-and-insert
                  (ishikk--virtualenv-python) "-m"
                  "pip" "install" "-e" ishikk-package-location))
      (user-error "Failed to create virtualenv; see buffer %S for details"
                  ishikk-process-buffer)))
  (message "Setting up Ishikk virtualenv...done"))

(defun ishikk-virtualenv-setup-maybe ()
  "If the Ishikk virtualenv doesn't exist, create it.
The virtualenv is created in `ishikk-virtualenv-location'."
  (unless (file-executable-p (ishikk--virtualenv-python))
    (ishikk-virtualenv-setup)))

(cl-defun ishikk--read-events (vdir &optional &keyword start-date end-date)
  "Read the calendar events from a VDIR directory.
Return a list of alists. START-DATE and END-DATE keyword
arguments, if provided, are date strings that limit the results
returned."
  (unless (file-directory-p vdir)
    (user-error "Calendar directory does not exist: %S" vdir))
  (ishikk--with-process-buffer
   (unless
       (apply #'ishikk--call-process-and-insert
              `(,(ishikk--virtualenv-python) "-m" "ishikk" "read"
                ,@(when start-date
                    `("--start-date" ,start-date))
                ,@(when end-date
                    `("--end-date" ,end-date))
                ,(expand-file-name vdir)))
     (user-error
      "Failed to communicate with backend; see buffer %S for details"
      ishikk-process-buffer))
   (condition-case e
       (let ((json-array-type 'list))
         (json-read-from-string
          (buffer-substring ishikk--output-start-marker
                            ishikk--output-end-marker)))
     (json-error
      (unless (= (char-before) ?\n)
        (insert "\n"))
      (insert (format "[JSON decode error: %s]" (error-message-string e))))
     (user-error
      "Failed to decode JSON from backend; see buffer %S for details"
      ishikk-process-buffer))))

(defcustom ishikk-minutes-per-line 15
  "Number of minutes per line of text in the calendar.
Increasing this value will compress the calendar vertically."
  :type 'number)

(defun ishikk--event-row (event)
  "Get the vertical position of EVENT in the calendar buffer.
This is a line number that can be used by e.g. `goto-line'."
  (let* ((start-time (alist-get 'start-time event))
         (start-comps (decode-time start-time))
         (start-minutes (+ (/ (nth 0 start-comps) 60.0)
                           (nth 1 start-comps)
                           (* (nth 2 start-comps) 60))))
    (floor start-minutes ishikk-minutes-per-line)))

(defun ishikk--event-column (event start-day col-width)
  "Get the horizontal position of EVENT in the calendar buffer.
This is a column number that can be used by e.g. `move-to-column'.

START-DAY is a time marking the beginning of the first day
displayed in the calendar, and COL-WIDTH is the width of a day
column."
  (let* ((start-time (alist-get 'start-time event))
         (col-index (- (time-to-days start-time)
                       (time-to-days start-day))))
    (1+ (* col-index (1+ col-width)))))

(defun ishikk--insert-calendar (events start-day end-day width)
  "Render a calendar displaying given EVENTS list into the current buffer.
START-DAY and END-DAY are inclusive-exclusive bounds on which
days are included in the calendar view; they should be times from
the beginning of the relevant days. WIDTH is the maximum
allowable width of the calendar."
  (erase-buffer)
  (let ((num-days (- (time-to-days end-day)
                     (time-to-days start-day))))
    (unless (>= num-days 1)
      (user-error "Calendar date range does not include at least one day"))
    (let* ((col-width (1- (/ (1- width) num-days)))
           (height (ceiling (* 24 60) ishikk-minutes-per-line))
           (col-template (make-string col-width ? )))
      (dotimes (_ height)
        (insert "|")
        (dotimes (_ num-days)
          (insert col-template "|"))
        (insert "\n")))))

(defcustom ishikk-calendar-buffer "*ishikk-calendar*"
  "Name of buffer used to display the calendar."
  :type 'string)

(define-derived-mode ishikk-calendar-mode fundamental-mode "Ishikk"
  "Major mode for the Ishikk calendar.")

(define-key ishikk-calendar-mode-map (kbd "q") #'quit-window)

(defcustom ishikk-num-days 7
  "Number of days to display in the calendar."
  :type 'integer)

(defun ishikk ()
  "Display the Ishikk calendar in a new buffer."
  (interactive)
  (when-let ((buf (get-buffer ishikk-calendar-buffer)))
    (kill-buffer buf))
  (with-current-buffer (get-buffer-create ishikk-calendar-buffer)
    (ishikk-calendar-mode)
    (pop-to-buffer (current-buffer))))

;;;; Closing remarks

(provide 'ishikk)

;;; ishikk.el ends here

;; Local Variables:
;; outline-regexp: ";;;;* "
;; End:
