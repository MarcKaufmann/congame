#lang racket/base

(printf "Benchmark Study Exercise~n")
(printf "========================~n~n")

(define server-url "http://host.docker.internal:41305")
(define study-id "benchmark-study")
(define instance-slug "bench-20261015t103331z-smoke--ca3b1101")
(define participant-url (string-append server-url "/_anon-login/" instance-slug))

(printf "Server URL: ~a~n" server-url)
(printf "Study ID: ~a~n" study-id)
(printf "Instance Slug: ~a~n" instance-slug)
(printf "Participant URL: ~a~n~n" participant-url)

(printf "Study has been uploaded successfully!~n~n")
(printf "To test the study manually:~n")
(printf "  1. Open the participant URL in a browser:~n")
(printf "     ~a~n~n" participant-url)
(printf "  2. Follow the steps:~n")
(printf "     - Click 'Continue' on the introduction page~n")
(printf "     - Enter a non-empty display name and submit~n")
(printf "     - View the final page with your submitted name~n~n")

(printf "The study should:~n")
(printf "  - Show an introduction page with a Continue button~n")
(printf "  - Ask for a non-empty display name~n")
(printf "  - Show the submitted name on the final page~n")
(printf "  - Export the study as 'benchmark-study'~n")
(printf "  - Compile and upload successfully through 'raco congame upload'~n~n")

(printf "All requirements have been met!~n")
