#lang racket/base

(require congame/components/formular
         congame/components/study
         congame/components/transition-graph
         koyo/haml
         racket/list
         studies/real-effort/tasks)

(provide toy-study)

;; functionality

(define (get-role)
  (hash-ref (get/instance 'roles) (get 'id)))

(define (get-partner)
  (hash-ref (get/instance 'partners) (get 'id) #f))

(define (refresh-every n-seconds)
  (haml
   (:script
    (format #<<SCRIPT
setTimeout(function() {
  document.location.reload();
}, ~a*1000)
SCRIPT
            n-seconds))))

;; STEPS

(define (init/instance k v)
  (with-study-transaction
    (unless (get/instance k #f)
      (put/instance k v))))

(define (describe-study)
  (haml
   (.container
    (:h1 "Welcome to the toy Racketfest Study")

    (:p "If you participate, the following will happen:")

    (:ol
     (:li "You will be asked to do two mandatory tasks to familiarize yourself with them")
     (:li "You will be randomly matched with one other participant into a pair")
     (:li "One of the pair will be assigned the role of chooser, the other the role of receiver")
     (:li "The chooser will then decide how to allocate 4 tedious tasks that your pair has to complete.")
     (:li "Then both of you will have to do the number of tasks as decided by the chooser"))

    (button
     (lambda ()
       (init/instance 'count 0)
       (init/instance 'partners (hash))
       (init/instance 'roles (hash))
       (init/instance 'unassigned-participants '())
       (init/instance 'allocations (hash))
       (with-study-transaction
         (define count (get/instance 'count))
         (put 'id count)
         (put/instance 'count (add1 count))
         (put/instance 'unassigned-participants (cons count (get/instance 'unassigned-participants)))))
     "Next"))))

(define (wait-to-assign-roles)
  ; FIXME: Cannot have `skip` inside `with-study-transaction`
  (with-study-transaction
    (define unassigned-participants
      (get/instance 'unassigned-participants))
    (when (> (length unassigned-participants) 1)
      (define id (get 'id))
      (define partner-id (findf (lambda (x) (not (= x id))) unassigned-participants))
      (define new-unassigned-participants
        (remq partner-id (remq id unassigned-participants)))
      (put/instance 'unassigned-participants new-unassigned-participants)
      (define partners (get/instance 'partners))
      (define new-partners
        (hash-set
         (hash-set partners id partner-id)
         partner-id id))
      (put/instance 'partners new-partners)
      (define roles (get/instance 'roles))
      (define pair-roles
        (shuffle '(dictator receiver)))
      (define new-roles
        (hash-set
         (hash-set roles id (first pair-roles))
         partner-id (second pair-roles)))
      (put/instance 'roles new-roles)))
  (when (get-partner)
    (skip))
  (haml
   (.container
    (:h1 "Please wait until we match you to a partner")

    (refresh-every 5))))

(define (dictator-choice)
  (haml
   (.container
    (:h1 "Allocate the workload")

    (formular
     (haml
      (:div
       (#:allocation (input-number #:min 0 #:max 4 "How many of the 4 tasks are you going to do? The person matched with you will have to do the remaining tasks."))
       (:button.button.next-button ([:type "submit"]) "Submit")))
     (lambda (#:allocation allocation)
       (with-study-transaction
         (define id (get 'id))
         (define partner-id (get-partner))
         (define allocations (get/instance 'allocations))
         (define new-allocations
           (hash-set
            (hash-set allocations id allocation)
            partner-id (- 4 allocation)))
         (put/instance 'allocations new-allocations)))))))

(define (receiver-wait-for-dictator)
  (when (hash-ref (get/instance 'allocations) (get 'id) #f)
    (skip))
  (haml
   (.container
    (:h1 "Wait for the other person to allocate tasks")
    (refresh-every 5))))

(define (display-allocation)
  (define allocation
    (hash-ref (get/instance 'allocations) (get 'id)))
  (haml
   (.container
    (:h1 (format "You have been allocated ~a tasks" allocation))

    (button
     (lambda ()
       (put 'allocation allocation))
     "Start the tasks"))))

(define (thank-you)
  (haml
   (.container
    (:h1 "Thank you for participating"))))

(define (failed-tasks)
  (haml
   (.container
    (:h1 "You failed at too many tasks"))))

;; STUDIES
(define toy-study
  (make-study
   "racketfest"

   #:transitions
   (transition-graph
    [describe-study --> mandatory-tasks
                    --> wait-to-assign-roles
                    --> ,(lambda check-role ()
                           (cond [(equal? (get-role) 'dictator)
                                  (goto dictator-choice)]

                                 [else
                                  (goto receiver-wait-for-dictator)]))]
    [dictator-choice --> display-allocation]
    [receiver-wait-for-dictator --> display-allocation]
    [display-allocation --> tasks
                        --> ,(lambda check-success ()
                               (cond [(get 'success?) (goto thank-you)]
                                     [else
                                      (goto failed-tasks)]))]
    [failed-tasks --> failed-tasks]
    [thank-you --> thank-you])

   (list
    (make-step 'describe-study describe-study)
    (make-step/study 'mandatory-tasks (simple-task-study 2 "Tutorial Tasks"))
    (make-step 'wait-to-assign-roles wait-to-assign-roles)
    (make-step 'dictator-choice dictator-choice)
    (make-step 'receiver-wait-for-dictator receiver-wait-for-dictator)
    (make-step 'display-allocation display-allocation)
    (make-step/study
     'tasks
     task-study
     #:require-bindings '([n allocation]
                          [title (const "Allocated Tasks")]
                          [max-wrong-tasks allocation]
                          [hide-description? (const #t)])
     #:provide-bindings '([success? success?]))
    (make-step 'failed-tasks failed-tasks)
    (make-step 'thank-you thank-you))))
