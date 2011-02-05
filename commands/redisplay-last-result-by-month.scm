;;; redisplay-last-result-by-month.scm --- as advertized

;; Copyright (C) 2004-2009, 2011 Thien-Thi Nguyen
;; This file is part of ETRACK, released under GNU GPL with
;; ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

(define-module (etrack))

(define-command (redisplay-last-result-by-month mspec) ; init=#t
  (cond ((not (procedure? *by-month-query*))
         (fso "Sorry, could not display by month.~%"))
        (else
         (fso "BY MONTH ~A~%" mspec)
         (>>table (*by-month-query* mspec) 'histogram 42 10))))

;;; redisplay-last-result-by-month.scm ends here
