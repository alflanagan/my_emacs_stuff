;;; test-eieio.el -- Try out eieio classes

;;; Commentary:

;; Some simple playing around with ideas. More mature classes will
;; be moved to their own files.

;;; Code:

(require 'eieio)

(defclass record ()
  ((name :initarg :name
         :initform ""
         :type string
         :custom string
         :documentation "The name of a person.")
   (birthday :initarg :birthday
             :initform "Jan 1, 1970"
             :custom string
             :type string
             :documentation "The person's birthday.")
   (phone :initarg :phone
          :initform ""
          :documentation "Phone number."))
  "A single record for tracking people I know.")

(defmethod call-record ((rec record) & optional scriptname)
  "Dial the phone for the record REC.
Execute the program named SCRIPTNAME to dial the phone."
  (message "Dialing the hpone for %s" (oref rec name))
  (shell-command (concat (or scriptname "dialphone.sh")
                         " "
                         (oref rec phone))))

(defvar test-eieio-rec)
(setq test-eieio-rec (record "Eric" :name "Eric" :birthday "June" :phone "555-5555"))

(defclass basic ()
  ((value :initarg :value
          :initform 0
          :custom integer
          :type integer
          :documentation "A value.")) "Doc String")

(require 'ert)
(ert-deftest test-eieio-eq ()
  "Test response of objects to eq."
  (let  ((test1  (basic "fred" :value 3))
         (test2  (make-instance 'basic "fred" :value 3)))
    (should (not (eq test1 test2)))))

(ert-deftest test-eieio-equal ()
  "Test response of objects to equal."
  (let  ((test1  (basic "fred" :value 3))
         (test2  (make-instance 'basic "fred" :value 3)))
    (should (equal test1 test2))))

(provide 'test-eieio)

;;; test-eieio.el ends here
