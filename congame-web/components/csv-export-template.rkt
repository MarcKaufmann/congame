#lang racket/base

(require deta
         gregor)

(provide
 (schema-out csv-export-template))

(define-schema csv-export-template
  #:table "csv_export_templates"
  ([id id/f #:primary-key #:auto-increment]
   [study-instance-id id/f]
   [name string/f]
   [(fields null) jsonb/f]
   [(created-at (now/moment)) datetime-tz/f]))
