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

(define (join-domain domain-item-list)
  (join domain-item-list "."))

(define (-get-tld domain)
  (if (not (find "." domain))
      nil)
  (let ((items (parse domain ".")))
    (dotimes (i (length items))
      (letn ((tld (join-domain (i items)))
             (tld* (string "*." tld))
             (tld! (string "!" tld)))
        (cond ((find tld tld-data) (throw tld))
              ((find tld! tld-data) (throw (join-domain
                                            (rest (i items)))))
              ((find tld* tld-data) (throw (join-domain
                                            ((- i 1) items)))))))))

(define (get-tld domain)
  (catch (-get-tld domain)))

(context MAIN)
