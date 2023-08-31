#lang racket/base

(require net/http-easy
         racket/contract
         racket/format
         racket/hash
         racket/list
         racket/string)

;; FIXME: Should this functionality not be broken in its own package that can be used outside of congame?
;; FIXME: We fail to set some information on the participant for export that we need, such as the status
;; For now, assume that the steps are in order (FIXME: check this -- is there ambiguity with respect to first-put and last-put?) and check if final step has 'id "completion-code"
;; FIXME: Get the anonymized username for debugging
;; FIXME: csv output is not reliable, since some feels may be merged when not escaping characters properly, which is error prone. Move to exporting to a sqlite DB in long run.

(provide
 current-api-url
 get-instance-participant-data
 get-participants-data/list
 get-all-instances-data
 get-and-write-data
 write-data)

(define current-api-url (make-parameter 'current-api-url-not-set))

;; gets the list of instances of the study
(define (api-study-instances study-id)
  (string-append (current-api-url)
                 "studies/" (number->string study-id) "/"
                 "instances.json"))

;; gets the participant data for this instance of the study
(define ((api-instance/participant-data study-id) instance-id)
  (string-append (current-api-url)
                 "studies/" (number->string study-id) "/"
                 "instances/" (number->string instance-id) "/"
                 "participants.json"))

(define api-keys
  (hash 'production "CONGAME_API_KEY"))

(define (api-key key)
  (getenv
   (hash-ref api-keys key)))

(define (get-study-instances study-id)
  (get (api-study-instances study-id)
       #:auth (lambda (url headers params)
                (values (hash-set headers 'authorization (api-key 'production)) params))))

(define (get-instance-participant-data study-id instance-id)
  (define api-url ((api-instance/participant-data study-id) instance-id))
  (define key (api-key 'production))
  (get api-url
       #:auth (lambda (url headers params)
                (values (hash-set headers 'authorization key) params))))

(define (get-participants-data/list study-id instance-id)
  (define instance-data
    (let ([res (get-instance-participant-data study-id instance-id)])
      (if (equal? #"HTTP/1.1 200 OK" (response-status-line res))
          (response-json res)
          (error "Couldn't get data" (response-status-line res)))))
  (hash-ref instance-data 'participants))

(define (get-participants-data study-id instance-id)
  (define instance-data
    (let ([res (get-instance-participant-data study-id instance-id)])
      (if (equal? #"HTTP/1.1 200 OK" (response-status-line res))
          (response-json res)
          (error "Couldn't get data" (response-status-line res)))))
  (for/list ([p (hash-ref instance-data 'participants)])
    (hash-union (hash 'participant-id (hash-ref p 'participant-id)
                      'study-id (hash-ref p 'study-id)
                      'instance-id (hash-ref p 'instance-id)
                      ;; FIXME get data on whether study is completed
                      )
                (for/hash ([entry (hash-ref p 'vars)])
                  (values (string->symbol (hash-ref entry 'id)) entry)))))

(define (var-labels vars-to-keep)
  (map (lambda (ks)
         (if (symbol? ks) ks (last ks)))
       vars-to-keep))

(define ((get-participant-data p) k)
  (cond [(or (equal? p #f) (equal? p '())) '()]
        [(symbol? k)
         (let ([res (hash-ref p k '())])
           (if (and (hash? res) (hash-has-key? res 'value))
               (hash-ref res 'value)
               res))]
        [(and (list? k)
              (empty? k))
         p]
        [(and (list? k)
              (not (empty? k)))
         ((get-participant-data ((get-participant-data p) (car k))) (cdr k))]))

(define (get-clean-data study-id instance-id vars-to-keep)
  (for/list ([p (get-participants-data study-id instance-id)])
    (map (get-participant-data p) vars-to-keep)))

(define (frm a)
  (define (escape s)
    (format "\"~a\"" (string-replace a "\"" "\\\"")))
  (cond [(string? a) (escape a)]
        [(symbol? a) (frm (symbol->string a))]
        [(boolean? a) (if (equal? a #f) "FALSE" "TRUE")]
        [(and (list? a) (empty? a)) "NA"]
        [else (~a a)]))

(define (get-all-instances-data study-id iids api-url vars-to-keep)
  (parameterize ([current-api-url api-url])
    (for/fold ([all-data '()])
              ([iid iids])
      (append (get-clean-data study-id iid vars-to-keep) all-data))))

(define/contract (dash->underscore s)
  (-> string? string?)
  (string-replace s "-" "_"))

(define (write-data data path vars-to-keep)
  (call-with-output-file path
    (lambda (out)
      (displayln (string-join (map (compose dash->underscore frm) (var-labels vars-to-keep)) ",") out)
      (for ([p data])
        (displayln (string-join (map frm p) ",") out)))))

(define (get-and-write-data
         #:study-id study-id
         #:iids iids
         #:api-url api-url
         #:vars-to-keep vars-to-keep
         #:path path)
  (define data (get-all-instances-data study-id iids api-url vars-to-keep))
  (write-data data path vars-to-keep))

