;;; grant.scm --- grant db access to a USER

;; Copyright (C) 2004-2009 Thien-Thi Nguyen
;; This file is part of ETRACK, released under GNU GPL with
;; ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

(define-module (etrack)
  #:use-module ((database postgres) #:select (pg-result-status
                                              pg-error-message)))

(define-command (grant user)            ; init=#t
  (let* ((res (Cfexec "GRANT ALL ON ~A, ~A TO ~A;"
                      (DK #:ename) (DK #:iseq) user))
         (ok? (eq? 'PGRES_COMMAND_OK (pg-result-status res))))
    (fso "~A\n" (if ok? "GRANT" (pg-error-message CONN)))
    ok?))

;;; grant.scm ends here
