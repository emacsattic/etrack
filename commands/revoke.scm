;;; revoke.scm --- revoke db access from a USER

;; Copyright (C) 2004-2009 Thien-Thi Nguyen
;; This file is part of ETRACK, released under GNU GPL with
;; ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

(define-module (etrack)
  #:use-module ((database postgres) #:select (pg-result-status
                                              pg-error-message)))

(define-command (revoke user)           ; init=#t
  (let* ((res (Cfexec "REVOKE ALL ON ~A, ~A FROM ~A;"
                      (DK #:ename) (DK #:iseq) user))
         (ok? (eq? 'PGRES_COMMAND_OK (pg-result-status res))))
    (fso "~A\n" (if ok? "REVOKE" (pg-error-message CONN)))
    ok?))

;;; revoke.scm ends here
