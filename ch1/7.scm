(define (nth-element lst n)
  (let loop ((cur-list lst) (cur-n n))
   (if (null? cur-list)
       (report-list-too-short lst n)
       (if (zero? cur-n)
           (car cur-list)
           (loop (cdr cur-list) (- cur-n 1))))))


(define (report-list-too-short lst n)
  (error (format #f  "~a does not have ~a elements." lst n)))
