;;; about.scm --- display some info about ETRACK

;; Copyright (C) 2004-2009, 2011 Thien-Thi Nguyen
;; This file is part of ETRACK, released under GNU GPL with
;; ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

(define-module (etrack))

(define-command (about)                 ; init=#f
  (let ((fsysf (lambda (s) (flush-all-ports) (system s) (flush-all-ports))))
    (fso "etrack version: ~A~%last modified:~%" *ETRACK-VERSION*)
    (fso "~:{~A ~A~%~}"
         (map (lambda (file)
                (let ((full (in-vicinity *ETRACK-DATA* file)))
                  (list (strftime "%Y-%m-%d %H:%M:%S"
                                  (localtime (stat:mtime (stat full))))
                        full)))
              '("etrack.el" "etrack.scm")))
    (newline)
    (fsysf (fs "cat ~A/NEWS" *ETRACK-DATA*))))

;;; about.scm ends here
