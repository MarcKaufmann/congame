#lang racket/base

(require congame-web/components/auth
         congame-web/components/auth-token
         congame-web/components/tag
         congame-web/components/user
         congame-web/pages/render
         congame/components/export
         congame/components/study
         json
         koyo/database
         koyo/url
         racket/contract
         racket/port
         web-server/dispatchers/dispatch
         web-server/servlet)

(provide
 studies
 study-instances
 study-participants
 enroll-participant-from-identity)

;; TODO: n+1
(define/contract ((studies db) _req)
  (-> database? (-> request? response?))
  (response/jsexpr
   (hash
    'studies (for/list ([s (in-list (list-studies db))])
               (hash
                'id (study-meta-id s)
                'name (study-meta-name s)
                'instances (get-instances-json-by-study db (study-meta-id s)))))))

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
     (hash 'instances (get-instances-json-by-study db study-id)))))

(define/contract ((study-participants db) _req study-id study-instance-id)
  (-> database? (-> request? id/c id/c response?))
  (let/ec return
    (define the-instance (lookup-study-instance db study-instance-id))
    (unless the-instance
      (return
       (response/jsexpr
        #:code 400
        (hash 'error "instance not found"))))
    (define vars
      (list-study-instance-vars db study-instance-id))
    (define participants
      (list-study-instance-participants/admin db study-instance-id))
    (response/jsexpr
     (study-instance->jsexpr db study-id study-instance-id vars participants))))

(define/contract ((enroll-participant-from-identity db users) req)
  (-> database? user-manager? (-> request? response?))
  (define data (call-with-input-bytes (request-post-data/raw req) read-json))
  (define instance-id (hash-ref data 'instance-id))
  (define username (hash-ref data 'identity-email))
  (define identity-domain (hash-ref data 'identity-domain))
  (define identity-key (hash-ref data 'identity-key))
  (define the-user
    (user-manager-create-from-identity! users (current-user) username identity-domain identity-key))
  (define the-instance
    (lookup-study-instance db instance-id))
  (unless the-instance
    (next-dispatcher))
  (enroll-participant! db (user-id the-user) (study-instance-id the-instance))
  (define the-token
    (create-auth-token! db (user-id the-user)))
  (define target-path
    (reverse-uri
     #:query `((return-url . ,(reverse-uri 'study-page (study-instance-slug the-instance))))
     'token-login-page the-token))

  (response/jsexpr
   (hasheq 'target-path target-path)))

(define (get-instances-json-by-study db study-id)
  (for/list ([i (in-list (list-study-instances db study-id))])
    (hash
     'id (study-instance-id i)
     'name (study-instance-name i)
     'status (format "~a" (study-instance-status i)))))


;; tags ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 tags)

(define/contract ((tags db) _req)
  (-> database? (-> request? response?))
  (response/jsexpr
   (->jsexpr
    (hash
     'tags
     (for/hash ([t (list-tags db)])
       (values (tag-id t)
               (hash 'name (tag-name t)
                     'instances (get-tag-instance-ids db (tag-id t)))))))))
