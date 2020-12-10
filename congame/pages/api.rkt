#lang racket/base

(require congame/components/export
         congame/components/study
         koyo/database
         racket/contract
         web-server/http
         "render.rkt")

(provide
 studies
 study-instances
 study-participants)

(define/contract ((studies db) _req)
  (-> database? (-> request? response?))
  (response/jsexpr
   (hash
    'studies (for/list ([s (in-list (list-studies db))])
               (hash
                'id (study-meta-id s)
                'name (study-meta-name s))))))

(define/contract ((study-instances db) _req study-id)
  (-> database? (-> request? id/c response?))
  (let/ec return
    (define the-study (lookup-study-meta db study-id))
    (unless the-study
      (return
       (response/jsexpr
        #:code 404
        (hash 'error "study not found"))))
    (response/jsexpr
     (hash
      'instances (for/list ([i (in-list (list-study-instances db study-id))])
                   (hash
                    'id (study-instance-id i)
                    'name (study-instance-name i)))))))

(define/contract ((study-participants db) _req study-id study-instance-id)
  (-> database? (-> request? id/c id/c response?))
  (let/ec return
    (define the-instance (lookup-study-instance db study-instance-id))
    (unless the-instance
      (return
       (response/jsexpr
        #:code 400
        (hash 'error "instance not found"))))
    (define participants
      (list-study-instance-participants/admin db study-instance-id))
    (response/jsexpr
     (study-participants->jsexpr db study-id study-instance-id participants))))
