;;; secret-data -- a small simple package to get/save key-value pairs
;;; of information I don't want to actually store in repository, like
;;; passwords/keys, etc.

;;; Commentary:

;;; Code:

(require 'cl-lib)


(defsubst string-join (strings &optional separator)
  "Join all STRINGS using SEPARATOR."
  (mapconcat 'identity strings separator))


(defcustom secret-data-store-file "~/.emacs-secret"
  "The file name used to store secret (well, semi-secret) data."
  :group 'secret-data
  :type 'file
  :risky t
  )


(defcustom secret-data-logging-enabled nil
  "Whether `secret-data' logging messages are written."
  :group 'secret-data
  :type 'boolean
  :safe t
  )


(defun secret-data-log (msg &rest args)
  "Writes a message with format from MSG and values ARGS, only if `secret-data-logging-enabled` is t."
  (when secret-data-logging-enabled
    (apply 'message msg args)))


(defun split-on-equal-if-key (a-string)
  "Splits A-STRING on first '=' character, return list of two strings.  Return nil if no '=' is present, or if '=' is first character."
  (let* ((values (split-string a-string "="))
         (part1 (car values))
         ;;value may have embedded equal signs; put them back
         (part2 (string-join (cdr values) "=")))
    (if (or (string-equal part1 "")
            (< (length values) 2))
        nil
      (list part1 part2 ))))


(defun secret-data-parse ()
  "Parse contents of current buffer, which should contain lines of the form KEY=VALUE, into a standard a-list."
  (mapcar
   (lambda (x) (split-on-equal-if-key x))
   (split-string (buffer-string) "[\f\n\r]+"))
  )


(defun secret-data-get-key (some-key &optional file-name)
  "Retrieve the value associated with SOME-KEY from the data stored in FILE-NAME.  FILE-NAME defaults to the value of custom variable `secret-data-store-file'."
  (let ((fname (if file-name file-name secret-data-store-file)))
    (secret-data-log "FILE-NAME is '%s', fname is '%s'." file-name fname)
    (condition-case err-data
        (with-temp-buffer
          (insert-file-contents (expand-file-name fname))
          (cadr (assoc some-key (secret-data-parse)))
          )
      (file-error (message "Can't get secret data with key [%s]: can't open file %s: (%s)."
                           some-key fname (cl-caddr err-data))))))


(defun secret-data-write-data-to-file (a-list file-name)
  "Writes the contents of A-LIST to file with name FILE-NAME, as key-value pairs."
  (condition-case err-data
      (with-current-buffer (find-file-literally file-name)
        (mapc (lambda (x) (when x (insert (format "%s=%s\n" (car x) (cadr x))))) a-list)
        (save-buffer)
        (kill-buffer))
    (file-error (message "Failed writing data to file [%s]: (%s)."
                         file-name (cl-caddr err-data)))))


(defun replace-file-with-back (source-file replace-file)
  "Save the contents of the file named SOURCE-FILE to the file name REPLACE-FILE.  If REPLACE-FILE names an existing file, the existing file is renamed with the addition of a backup file extension."
  (when (file-exists-p replace-file)
    (rename-file replace-file (concat replace-file ".bkp") t))
  (rename-file source-file replace-file))


(defun secret-data-store-value (some-key some-value &optional file-name)
  "Assign to SOME-KEY the value SOME-VALUE in the data store in file FILE-NAME.  FILE-NAME defaults to the value of custom variable `secret-data-store-file'."
  (let ((fname (if file-name file-name secret-data-store-file)))
    (condition-case err-data
        (with-temp-buffer
          (insert-file-contents (expand-file-name fname))
          (let* ((read-a-list  (secret-data-parse))
                 (key-value (assoc some-key read-a-list)))
            (if key-value
                (setf (cadr (assoc some-key read-a-list)) some-value)
              (setq read-a-list (append read-a-list `((,some-key ,some-value)))))
            (let ((new-file-name (make-temp-file "secret-data-store")))
              (secret-data-write-data-to-file read-a-list new-file-name)
              (replace-file-with-back new-file-name file-name))))
      (file-error (message "Failed writing value to key [%s]: can't open file %s: (%s)."
                           some-key fname (cl-caddr err-data))))))

(provide 'secret-data)
;;; secret-data.el ends here
