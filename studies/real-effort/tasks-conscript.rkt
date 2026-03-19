#lang conscript

;; Matrix counting task study — reusable as a child study.
;;
;; Usage (in a #lang conscript parent study):
;;
;;   (require "tasks-conscript.rkt")
;;
;;   (defstep/study do-tasks
;;     #:study (lambda ()
;;               (make-task-study #:n 5
;;                                #:title "Count the 1s")))
;;
;;   (defstep (results)
;;     @md{You got @number->string[task-correct-answers] correct.
;;         @(if task-success? "You passed!" "You failed.")
;;         @button{Continue}})
;;
;;   (defstudy my-experiment
;;     [setup --> do-tasks --> results]
;;     [results --> results])

(require conscript/form0
         conscript/survey-tools
         racket/string)

(provide
 make-task-study
 task-success?
 task-correct-answers
 task-wrong-answers)


;; Output variables — shared with parent studies via defvar*. ;;;;;;;;;;
;; After the task study completes, the parent reads these directly.
(with-namespace xyz.trichotomy.congame.real-effort-tasks
  (defvar* task-success?)
  (defvar* task-correct-answers)
  (defvar* task-wrong-answers))

;; Internal state, scoped to each task study embedding.
(defvar current-matrix)


;; Matrix generation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A matrix is (list cells answer) where:
;;   cells  = list of lists of strings (display values per cell)
;;   answer = exact count of cells displaying "1"

(define (generate-matrix)
  (define rows 8)
  (define cols 12)
  (define cells-raw
    (for/list ([_r (in-range rows)])
      ;; Each row gets a random percentage of 1s (30–60%).
      (define pct (+ 30 (random 31)))
      (for/list ([_c (in-range cols)])
        (if (< (random 100) pct) 1 0))))
  (define answer
    (for*/sum ([row (in-list cells-raw)]
               [cell (in-list row)])
      cell))
  (define cells
    (for/list ([row (in-list cells-raw)])
      (for/list ([cell (in-list row)])
        (if (= cell 1)
            "1"
            (random-ref (list "0" "0" "00" "01" "11" "10"))))))
  (list cells answer))

(define (matrix-cells m) (first m))
(define (matrix-answer m) (second m))


;; Rendering ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define cell-style
  "border: 1px solid #000; width: 30px; height: 30px; text-align: center; font-size: 14px; background: #fff; padding: 2px;")

(define table-style
  "border-collapse: collapse; margin: 20px auto;")

(define (render-matrix m)
  `(table ((style ,table-style))
     ,@(for/list ([row (in-list (matrix-cells m))])
         `(tr ()
            ,@(for/list ([cell (in-list row)])
                `(td ((style ,cell-style)) ,cell))))))

(define (example-matrix)
  `(table ((style ,table-style))
     (tr () (td ((style ,cell-style)) "1") (td ((style ,cell-style)) "11"))
     (tr () (td ((style ,cell-style)) "10") (td ((style ,cell-style)) "0"))))

(define (cells->json cells)
  (string-append
   "["
   (string-join
    (for/list ([row (in-list cells)])
      (string-append
       "["
       (string-join
        (for/list ([cell (in-list row)])
          (string-append "\"" cell "\""))
        ",")
       "]"))
    ",")
   "]"))

(define matrix-canvas-js
  "var el = document.currentScript.parentElement;
var cells = JSON.parse(el.dataset.cells);
var canvas = el.querySelector('canvas');
var ctx = canvas.getContext('2d');
var cellW = 30, cellH = 30;
var rows = cells.length, cols = cells[0].length;
canvas.width = cols * cellW + 1;
canvas.height = rows * cellH + 1;
ctx.font = '14px monospace';
ctx.textAlign = 'center';
ctx.textBaseline = 'middle';
for (var r = 0; r < rows; r++) {
  for (var c = 0; c < cols; c++) {
    var x = c * cellW, y = r * cellH;
    ctx.fillStyle = '#fff';
    ctx.fillRect(x, y, cellW, cellH);
    ctx.strokeStyle = '#000';
    ctx.lineWidth = 1;
    ctx.strokeRect(x + 0.5, y + 0.5, cellW, cellH);
    ctx.fillStyle = '#000';
    ctx.fillText(cells[r][c], x + cellW / 2, y + cellH / 2);
  }
}")

(define (render-matrix-canvas m)
  `(div ((class "matrix-canvas")
         (data-cells ,(cells->json (matrix-cells m))))
     (canvas ((style "display: block; margin: 20px auto;")))
     (script () ,matrix-canvas-js)))

(define (task-description)
  @md*{### Matrix Task Description

       Each page will show a matrix (table) of cells.

       * You have to count the number of cells that contain exactly the number 1
       * Cells containing the number 11, 10, 01, or 00 do not count
       * If you get more tasks wrong than allowed, you fail and drop out of the study
       * If you get a matrix wrong, you will be given a new one

       #### Example

       In this toy 2x2 matrix, only 1 cell contains exactly the number 1.
       Cells containing 10 or 11 do not count:

       @(example-matrix)

       **Answer: 1**})

;; Bot handler ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (task/bot _)
  (define answer (bot:find-attribute "data-answer"))
  (define input
    (bot:element-find
     (bot:find "form")
     "input"))
  (marionette:element-type! input answer)
  (marionette:element-click! (bot:find "button[type=submit]")))


;; Main API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Returns a study value that can be embedded as a child study via
;; defstep/study. Configuration is passed as keyword arguments.
;; Results are read from the defvar* output variables after the
;; child study completes.
(define (make-task-study #:n n-tasks
                         #:title [title "Matrix Tasks"]
                         #:max-wrong [max-wrong n-tasks]
                         #:hide-description? [hide-description? #f]
                         #:render-mode [render-mode 'table])

  (define n-string (number->string n-tasks))
  (define max-wrong-string (number->string max-wrong))
  (define max-wrong+1-string (number->string (add1 max-wrong)))

  (define (initialize-tasks-handler)
    (define (start-action)
      (set! task-correct-answers 0)
      (set! task-wrong-answers 0)
      (set! current-matrix (generate-matrix)))

    @md{# @|title|

        You now have to do @|n-string| tasks successfully, and you can get
        at most @|max-wrong-string| tasks wrong. Whenever you get a task wrong, you
        will be given a new task to try. If you get @|max-wrong+1-string| or more
        wrong, you automatically fail and drop out of the study.

        @(toggleable-xexpr "Show/Hide Task Description" (task-description)
                           #:hidden? hide-description?)

        @button[start-action]{Start Tasks}})

  (define (task-handler)
    (define m current-matrix)
    (define tasks-correct task-correct-answers)
    (define tasks-wrong task-wrong-answers)

    (define task-form
      (form* ([number-of-ones (ensure binding/number (required))])
             number-of-ones))

    (define (submit-action number-of-ones)
      (cond [(= (matrix-answer m) number-of-ones)
             (set! task-correct-answers (add1 task-correct-answers))]
            [else
             (set! task-wrong-answers (add1 task-wrong-answers))]))

    (define bot-hint
      (if (current-user-bot?)
          `(p ((data-answer ,(number->string (matrix-answer m)))) "")
          ""))

    (define (render rw)
      @html*{@(if (eq? render-mode 'canvas)
                  (render-matrix-canvas m)
                  (render-matrix m))
             @rw["number-of-ones" @input-number{How many cells with the number 1 are in the matrix? (Cells with 00, 01, 10, or 11 do not count.)}]
             @|submit-button|
             @|bot-hint|})

    @md{# Count the cells with 1's in them

        @format["You completed ~a out of ~a tasks (~a wrong guesses out of at most ~a)."
                tasks-correct n-tasks tasks-wrong max-wrong]

        @format["If you get more than ~a wrong guesses, you drop out of the study."
                max-wrong]

        @form[task-form submit-action render]})

  (define (task-completion)
    (define remaining (- n-tasks task-correct-answers))
    (cond [(<= remaining 0)
           (set! task-success? #t)
           done]
          [(> task-wrong-answers max-wrong)
           (set! task-success? #f)
           done]
          [else
           (set! current-matrix (generate-matrix))
           'task]))

  (define start-tasks
    (with-bot initialize-tasks-handler bot:continuer))

  (define task
    (with-bot task-handler task/bot))

  (defstudy the-study
    [start-tasks --> ,(lambda () (task-completion))]
    [task --> ,(lambda () (task-completion))])

  the-study)

;; Example — for testing

(defstep/study do-tasks
  #:study (lambda ()
            (make-task-study #:n 4
                             #:title "Count the 1s"
                             #:render-mode 'canvas)))

(defstep (setup)
  @md{# Get Ready!
      @button{Continue}})

(defstep (results)
  @md{You got @number->string[task-correct-answers] correct.
      @(if task-success? "You passed!" "You failed.")
      @button{Continue}})

(provide my-experiment)
(defstudy my-experiment
  [setup --> do-tasks --> results]
  [results --> results])
