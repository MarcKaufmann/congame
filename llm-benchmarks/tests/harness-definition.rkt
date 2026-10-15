#lang racket/base

(require racket/file
         rackunit
         "../private/config.rkt"
         "../private/harness-definition.rkt"
         "../private/wizard-draft.rkt")

(define (with-root proc)
  (define root (make-temporary-file "harness-definition-~a" 'directory))
  (dynamic-wind void (lambda () (proc root)) (lambda () (delete-directory/files root))))

(define draft
  (for/fold ([draft (default-harness-draft "test-agent")])
            ([(key value) (in-hash (hasheq 'image "agent:1" 'script "#!/bin/sh\nexit 0\n"
                                           'environment_from_host "API_KEY"))])
    (hash-set draft key value)))

(test-case "create a runnable harness configuration without executing its script"
  (with-root
   (lambda (root)
     (define result (create-harness! root draft))
     (parameterize ([benchmark-config-root root])
       (check-equal? (load-harness "test-agent") (harness-config draft)))
     (check-equal? (file->string (build-path result "run.sh")) (hash-ref draft 'script))
     (check-not-false (member 'execute (file-or-directory-permissions (build-path result "run.sh"))))
     (check-exn #rx"already exists" (lambda () (create-harness! root draft)))
     (define clone (harness-template-draft result "clone"))
     (define cloned (create-harness! root clone))
     (check-equal? (file->string (build-path cloned "run.sh")) (hash-ref draft 'script)))))

(test-case "invalid harness settings fail before creating files"
  (with-root
   (lambda (root)
     (for ([(key value) (in-hash (hasheq 'id "../escape" 'script "" 'image ""
                                       'cpus "0" 'pids "-1" 'wall_seconds "no"
                                       'memory "0g" 'max_cost_usd "+inf.0"
                                       'environment "[]" 'environment_from_host "KEY=value"
                                       'labels "{\"model\":42}" 'config_directory "../outside"))])
       (check-exn exn:fail? (lambda () (create-harness! root (hash-set draft key value)))))
     (check-exn #rx"both configured"
                (lambda () (validate-harness! root (hash-set draft 'environment "{\"API_KEY\":\"value\"}"))))
     (check-false (directory-exists? (build-path root "harnesses"))))))

(test-case "resources copy with cleanup on failure"
  (with-root
   (lambda (root)
     (define resources (build-path root "resources"))
     (make-directory* (build-path resources "agent"))
     (make-directory (build-path resources "compiled"))
     (display-to-file "settings" (build-path resources "agent" "settings.json"))
     (define custom (hash-set (hash-set draft 'resources (path->string resources)) 'config_directory "agent"))
     (define result (create-harness! root custom))
     (check-equal? (file->string (build-path result "agent" "settings.json")) "settings")
     (check-false (directory-exists? (build-path result "compiled")))
     (make-file-or-directory-link "settings.json" (build-path resources "agent" "link"))
     (check-exn #rx"symlinks" (lambda () (create-harness! root (hash-set custom 'id "failed"))))
     (check-equal? (directory-list (build-path root "harnesses")) (list (string->path "test-agent"))))))

(test-case "drafts preserve incomplete fields and reject the wrong wizard kind"
  (with-root
   (lambda (root)
     (define path (save-wizard-draft! root (default-harness-draft) 3 #:kind "harness"))
     (check-equal? (hash-ref (load-wizard-draft path #:kind "harness") 'draft) (default-harness-draft))
     (check-exn #rx"Not a supported task draft" (lambda () (load-wizard-draft path)))
     (define task-path (save-wizard-draft! root #hasheq() 0))
     (check-exn #rx"Not a supported harness draft"
                (lambda () (load-wizard-draft task-path #:kind "harness"))))))
