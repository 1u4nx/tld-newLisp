;;;; author: lu4nx <lx@shellcodes.org>
;;;; date: 2014-05-31

(context 'TLD)
(set 'tld-data (list))

(define (load-tld-data!)
  (let ((f (open "effective_tld_names.dat" "read")))
    (while (read-line f)
      (let ((line (current-line)))
        (if (and (not (starts-with line "//"))
                 (!= line ""))
            (push line tld-data))))))

(load-tld-data!)

(define (-get-tld domain)
  (if (not (find "." domain))
      nil)
  (let ((domain-itmes (parse domain ".")))
    (dotimes (i (length domain-itmes))
      (letn ((tld (join (i domain-itmes) "."))
             (tld* (string "*." tld))
             (tld! (string "!" tld)))
        (cond ((find tld tld-data) (throw tld))
              ((find tld! tld-data) (throw tld))
              ((find tld* tld-data) (throw domain)))))))

(define (get-tld domain)
  (catch (-get-tld domain)))

(context MAIN)
