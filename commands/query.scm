;;; query.scm --- submit a query and display the result

;; Copyright (C) 2004-2009 Thien-Thi Nguyen
;; This file is part of ETRACK, released under GNU GPL with
;; ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

(define-module (etrack))

(define-command (query num)             ; init=#t
  (and (string? num) (set! num (string->number num)))
  (if (or (< num 0) (>= num (*queries* #:count)))
      (error "no such query:" num)
      (let* ((q (list-ref (*queries*) num))
             (name (car q))
             (rv (begin
                   (fso "~A\n" name)
                   ((cadr q)))))          ;;; application
        ;;(fso "~A => ~A\n" name rv)
        (>>table rv)
        (fso "EOQ\n\n"))))

;;; query.scm ends here
