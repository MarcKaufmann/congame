#lang racket/base

(provide
 (struct-out step)
 (struct-out step/study)
 (struct-out step-page)
 (struct-out study))

(struct step (id handler handler/bot view-handler transition)
  #:transparent)

(struct step/study step (study)
  #:transparent)

(struct step-page (renderer xexpr-validator)
  #:transparent
  #:property prop:procedure
  (λ (p)
    ((step-page-renderer p))))

(struct study (name requires provides transitions steps view-handler failure-handler)
  #:transparent)
