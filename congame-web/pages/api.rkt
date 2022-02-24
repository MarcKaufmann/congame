#lang racket/base

(require congame/components/study
         congame/components/export
         json
         koyo/continuation
         koyo/database
         koyo/url
         racket/contract
         racket/port
         web-server/dispatchers/dispatch
         web-server/servlet
         congame-web/components/auth
         "../components/tag.rkt"
         congame-web/components/user
         "render.rkt"
         "study.rkt")

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

(define/contract ((enroll-participant-from-identity db auth users) req)
  (-> database? auth-manager? user-manager? (-> request? response?))
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

  (send/suspend/dispatch
   (lambda (embed/url)
     (response/jsexpr
      (hasheq 'target-path (embed/url
                            (lambda (_req)
                              ;; FIXME: use a proper route instead of the wrap-protect hack.
                              (define req-to-protect (redirect/get/forget))
                              (define protected-handler
                                (wrap-protect-continuations
                                 (lambda (req)
                                   (auth-manager-login!/nopass auth the-user)
                                   (parameterize ([current-user the-user])
                                     ((enroll db the-instance) req)))))
                              (protected-handler req-to-protect))))))))

(define (get-instances-json-by-study db study-id)
  (for/list ([i (in-list (list-study-instances db study-id))])
    (hash
     'id (study-instance-id i)
     'name (study-instance-name i)
     'status (format "~a" (study-instance-status i)))))


;; tags ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 tags)

(define/contract ((tags db) req)
  (-> database? (-> request? response?))
  (response/jsexpr
   (->jsexpr
    (hash
     'tags
     (for/hash ([t (list-tags db)])
       (values (tag-id t)
               (hash 'name (tag-name t)
                     'instances (get-tag-instance-ids db (tag-id t)))))))))
