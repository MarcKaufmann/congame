#lang racket/base

(define abstract-pjb-pilot-study
  (make-abstract-study
   "pjb-pilot-study"
   #:requires '()
   #:provides '(rest-treatment completion-code)
   #:failure-handler (lambda (s reason)
                       (put 'fail-status reason)
                       (eprintf "failed at ~e with reason ~e~n" s reason)
                       (put 'rest-treatment 'fail)
                       (put 'completion-code #f)
                       reason)
   #:transitions (transition-graph
                  [the-study <-- abstract-pjb-pilot-study-no-config(#:required-tasks {low,high} #:relax-treatment {types-of-relaxing-music} #:participationparticipation-fee ... #:tutorial-tasks ...) ; abstract-study ; takes parameters practice-tasks, tutorial-fee, and price-lists
                   ; starts and runs the-study
                   --> (λ ()
                         (if (get 'consent?)
                             'show-payments
                             'no-consent))]
                  [{show-payments : "none"}
                   ; Abstract/Model: information/parameters needed for running abstract study (simulation for data only)
                   ; Human: display information regarding payments
                   ; Bot: stuff needed for running the implementation: `continue`
                   --> done] ; How is `--> done` different from `--> ,(λ () done)`? Ah, I actually implemented the `done` in `--> done` as a named-step, so both are steps. `,(λ () done)` uses the widget `done`, I believe, and hence stops the study and passes control to the parent. But the parent study should not relinquish control, or we'll error out.
                  [no-consent
                   ; display information
                   ; require action `continue`
                   --> show-payments]
                  [fail-required-tasks
                   ; display information
                   ; require action `continue`
                   --> show-payments]
                  [fail-tutorial-tasks
                   ; display information
                   ; require action `continue` (Yes/No)
                   --> done])

                   ; Should I rename `study->participant` to `event`? And `participant->study` to `participant-action`?

; Identify where the crucial parameters for the study are configured T/...
; represents a treatment that we actively care about as treatment, as opposed to
; some random number (such as completion code) that just doesn't matter. Of
; course for some purposes we might care about different treatments.
; Distinguish between info about model-relevant parameters (tasks, treatment, etc) denoted by Signal() and info about the study that is needed for humans to make sense of it all (called Info())
(define-impl (init/impl sample signal)
  ...
  (put/expected 'd5))

(check/implements init/impl abstract-pjb... initialize)

(define-impl abstract-pjb-pilot-study-no-config
  (make-abstract-study
   "pjb-pilot-study-no-config"
   #:transitions (transition-graph
                  [initialize : (Sample required-tasks (list ...)) (Signal ...) (report d(5|s) ...)
                              (define (initialize/implemented )
                                (-> sampled-required-task provided-signal (hash disutility/c)))

                              (put/expected 'd5)
                              (put/expected 'd8)
                   ; IMPORTANT: Since I introduce a state variable, I have to initialize it somewhere for the abstract model
                   ; Sample(T/required-tasks) ~ Binomial({high, low})
                   ; Sample(T/relax-treatment) ~ Multinomial({types of relaxing music})
                   ; Action: continue? (Yes, No)
                   --> explain-study
                   ; Signal: (list required-tasks participation-fee practice-tasks *study-structure*)
                   ; Action: continue?
                   --> test-study-requirements
                   ; For abstract study, doesn't matter. abstract/skip, assume satisfies-requirements? is #t
                   ; Check(satisfies-requirements?)
                   ; (define satisfies-requirements (and play-audio? has-audio? has-time?)
                   ; Action: answer checks; continue?
                   --> ,(λ ()
                          (if (not (get 'satisfies-requirements?))
                              'requirements-failure
                              'tutorial-tasks))]
                  [tutorial-tasks
                   ; Abstract assume success? is #t, so just need mapping to s_t (which is symbolic at this point)
                   ; n max-wrong-tasks -- where is n and max-wrong-tasks determined? Important for treatments and mapping the flow of data through the study.
                   ; this step changes the state of the participant
                   ; s = tiredness, determines the disutility from work later
                   ; s_{before_tutorial} --> s_{after_tutorial} = f(s_t, n, max-wrong-tasks), where n is the number of tasks needed, and max-wrong-tasks the maximum number of tasks the person can get wrong. Might be stochastic in reality, might want to model at the individual-task level. For now overkill.
                   ; Action/Outcome: tutorial-success? -- if want more fine-grained (e.g. how many tasks wrong/right, whether stop before finishing), need more state variables. Ideally define at this level what we really need for later steps.
                   --> ,(λ ()
                          (if (not (get 'tutorial-success?))
                              (fail 'fail-tutorial-tasks)
                              'tutorial-illustrate-elicitation))]
                  [tutorial-illustrate-elicitation
                   ; Abstract: assume continue? is #t
                   ; (info "Elicitation Method")
                   ; Action: continue?
                   --> test-comprehension
                   ; Abstract: assume comprehension passes
                   ; (check/info "study comprehension") --> in abstract it's about information set
                   ; (concretely/check ("know required-tasks" "participation-fee payment if failing required tasks"))
                   --> consent
                   ; Abstract: assume consent? is #t
                   ; Action: consent?
                   --> ,(λ ()
                          (put-payment! 'tutorial-fee (get 'tutorial-fee))
                          (cond [(not (get 'consent?))
                                 (put 'rest-treatment 'NA:no-consent)
                                 done]
                                [else
                                 (put 'rest-treatment (next-balanced-rest-treatment))
                                 'tutorial-completion-enter-code]))]
                  ; Pay('tutorial-fee)
                  ; if consent? then Sample(rest-treatment) ~ Binomial({before, after}), balanced else done
                  ; Abstractly without attrition can ignore the if
                  ; Abstractly with modeling of attrition: have to realize that consent? may depend on various factors. Here should not have differential attrition, so we can ignore it.
                  [tutorial-completion-enter-code
                   ; Abstract: assume fine
                   ; Admin (i.e. pure implementation, no abstract value)
                   ; enter completion code
                   --> required-tasks
                   ; Abstract: map current s to yet another s, whose name/symbol depends on treatments and so on
                   ; n max-wrong
                   ; s_t --> s_{t + 1} = f(s_t, n, max-tasks-wrong)
                   ; Outcome: success?
                   --> ,(λ ()
                          (cond [(not (get 'success?))
                                 (fail 'fail-required-tasks)]
                                [else
                                 (case (get 'rest-treatment)
                                   [(get-rest-then-elicit) 'get-rest]
                                   [(elicit-then-get-rest) 'elicit-WTW-and-work])]))]
                  [get-rest
                   ; s_t --> s_{t+1} should go down, or at the very least change
                   ; depends on `relax-treatment`, i.e. what tracks participants are listening too
                   ; Outcome: data on how perception of how restful own soundtracks and and other 20-second snippets would have been; test that they correctly identify their own; in abstract model, this is not needed, only the mapping from s_t to s_{t+1} (or rather the mapping for disutility)
                   --> ,(λ ()
                          (case (get 'rest-treatment)
                            [(get-rest-then-elicit) 'elicit-WTW-and-work]
                            [(elicit-then-get-rest) 'debrief-survey]))]
                  [elicit-WTW-and-work
                   ; Abstract:
                   ; This is the place where we actually want to get some numbers from the model! Here it depends how I parameterize the original model, and how I want to model the changes in tiredness: do I want to track *tiredness* via s, and have amodel of d(s), or do I want to sample, model, and track d(s) itself? Pick one and try it out, then play around with it once that works.
                   ; Get d(5|s = {s_before_rest, s_after_rest}), d(8|s), d(11|s), d(15|s), levels determined by PRICE-LISTS
                   ; Compare d(5 | s_before)  to d(5 | s_after)
                   ; where s = s_{before} or s_{after} depending on treatment
                   ; Sample(choice-that-counts) as per instructions
                   ; Have them do work, obtain s_{final} from s
                   --> ,(λ ()
                          (case (get 'rest-treatment)
                            [(get-rest-then-elicit) 'debrief-survey]
                                                [(elicit-then-get-rest) 'get-rest]))]
                  [debrief-survey
                   ; Abstract: skip
                   ; Get control variables, get feedback, get how restful activity is perceived
                   ; Again for fully abstract study, this is not necessary, although we might extend abstract study to less and less abstract layers that take into account more and more detail of the study and attempt to model it
                   --> ,(λ () done)]
                  [requirements-failure
                   --> ,(λ () done)])
