#lang racket/base

(require data/monocle
         json
         racket/contract/base
         struct-define)

(provide
 role/c
 (contract-out
  [struct message
    ([role role/c]
     [message (or/c string? (listof string?))])]
  [message->jsexpr
   (-> message? jsexpr?)]
  [jsexpr->message
   (-> jsexpr? message?)]))

(define role/c
  (or/c 'user 'assistant))

(struct message
  (role message)
  #:transparent)

(define (message->jsexpr m)
  (struct-define message m)
  (hasheq
   'type "message"
   'role (symbol->string role)
   'content (content->jsexpr message)))

(define (content->jsexpr content)
  (if (pair? content)
      (for/list ([text (in-list content)])
        (hasheq
         'type "input_text"
         'text text))
      (content->jsexpr (list content))))

(define (jsexpr->message data)
  (message
   #;role (string->symbol (hash-ref data 'role))
   #;message (jsexpr->content (hash-ref data 'content))))

(define (jsexpr->content data)
  (if (string? data)
      data
      (for/list ([text-data (in-list data)])
        (hash-ref text-data 'text))))
