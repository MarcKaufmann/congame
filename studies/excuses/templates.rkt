#lang at-exp racket

(require congame/components/resource
         congame/components/study
         racket/runtime-path
         koyo/haml
         "abstract-categorization.rkt")

(provide
 edpb-config
 instructions
 pilot-instructions
 conf
 $conf)

(define-static-resource reason-screenshot "resources/reason-screenshot.png")
(define-static-resource bonus-switching-screenshot "resources/bonus-for-switching-screenshot.png")

(define (~$ a)
  (format "$~a" (~r a #:precision 2)))

(define edpb-config
  (hash 'day2-date "TOMORROW"
        'session1-bonus 3
        'session2-bonus 6
        'estimated-performance-bonus 6
        'estimated-time 20
        'piece-rate-per-abstract 0.05
        'baseline-abstracts 30
        'estimated-extra-abstracts 35
        'tutorial-abstracts 2
        'pilot-example-option-A-n 50
        'pilot-example-option-B-n 40
        'pilot-completion-code "C1JF3EZV"
        'pilot-code-for-failed-comprehension "C1WQ1J4P"
        'pilot-tutorial-abstracts 2
        'pilot-baseline-abstracts 30
        'pilot-additional-low 30
        'pilot-additional-high 70
        'pilot-tutorial-duration-estimate "10"
        'pilot-study-duration-estimate 30
        'pilot-tutorial-fee 2
        'pilot-completion-fee 4
        'pilot-fail-comprehension-fee 2
        'pilot-correct-abstract-bonus 0.05))

(define (conf x)
  (~a (hash-ref edpb-config x)))

(define ($conf x)
  (~$ (hash-ref edpb-config x)))

(define (pilot-instructions)
   (haml
    @:div{
      @:h1{Main Study Instructions}

      @:p{If you decide to participate, the main session starts immediately after you consent to participate.}

      @:h2{Abstract Categorization Tasks}

      @:p.info{In the main session, you will categorize abstracts (short summaries of research papers) by topics. In order to complete the study, you have to categorize 66% of the abstracts correctly, otherwise you fail out of the study.}

      @:p{Specifically, we will give topics, and you should choose whether this abstract belongs to a given topic or not. In the main study, you will be asked to categorize the following:}
      @:ul{
           @:li{@:strong{Baseline Tasks:} Categorize @conf['pilot-baseline-abstracts] baseline abstracts into 'Social Preferences' or 'Other'.}
           @:li{@:strong{Additional Tasks:} Categorize an additional @conf['pilot-additional-low] to @conf['pilot-additional-high] abstracts based on your choice in the decision that counts}}

      @:p{On the next page, you will see an example abstract and its categorization. On the subsequent pages, you can try the categorization task yourself.}

      @:h2{Decisions about Additional Abstracts}

      @:p{At the start of the main session, you have to make a series of decision problems. Each decision problem is between two different categorizations (Option A and Option B) and consists of up to three steps:}

      @:ol{
        @:li{Reveal exactly one reason (sometimes)}
        @:li{Choose one of the two options}
        @:li{Pick a bonus such that you would switch to the other option}}

      @:p{We now discuss each of these steps in more detail based on an example where Option A consists of categorizing @conf['pilot-example-option-A-n] abstracts into 'Social Preferences' or 'Other', and Option B consists of categorizing @conf['pilot-example-option-B-n] abstracts into 'Banking' or 'Other'.}

      @:h4{Revealing Reasons}

      @:p{For some but not all decisions, you first have to reveal @:em{exactly one} reason for or against one of the options. Only then can you submit your choice. The screenshot below shows what this looks like.}

      @:p{These reasons will not provide additional information about how to do the tasks. Instead they are based on a previous survey with Prolific participants who were asked along several dimensions what they think about the different categories - such as how important, interesting, etc they find the topics.}

      @(haml (.screenshot (:img ([:src (resource-uri reason-screenshot)]))))

      @:h4{Choosing one option}

      @:p{After you potentially decided to reveal a reason, you will see a screen with the reason, followed by the choice between the two options.}

      @:h4{Picking a switching bonus}

      @:p{Next, you will be asked to pick the smallest bonus for which you would switch from the option that you chose to the other option. That is, how much extra money you would have to receive to agree to the other categorization task? Suppose that you had picked option B above. Then you would see the following screenshot:}

      @(haml (.screenshot (:img ([:src (resource-uri bonus-switching-screenshot)]))))

      @:p{Using the '+' and '-' button, you could increase the payment in increments of $0.10 until you reach an amount such that you would choose Option A with that bonus, rather than option B without a bonus. The maximum you can choose is $1.1.}

      @:h2{Determining Decision that Counts}

      @:p.info{We now describe how your choices affect the work and bonuses that you may receive. The main take-away is that you should report your true answers.}

      @:p{Once you have made all the decisions, one of the decisions will randomly be picked as @:em{the decision that counts}: whatever you chose in that decision determines which and how many additional abstracts you will categorize later.}

      @:p{For example, suppose that in the decision above you chose Option B: "Categorize @conf['pilot-example-option-B-n] abstracts today into 'Banking' or 'Other'". If this decision is randomly picked as the decision that counts, then you will have to first categorize @conf['pilot-baseline-abstracts] abstracts into 'Social Preferences' or 'Other' - the @:em{baseline work}, independent of your choice - followed by categorizing @conf['pilot-example-option-B-n] abstracts into 'Banking' or 'Other' - the @:em{additional work}.}

      @:p{If the decision that counts is one about the switching bonus, then a random bonus between $0.00 and $1.00 is picked ($0.10, $0.20, ...). If this is larger than the bonus you stated, then you do switch the option and receive the bonus. If the random bonus is lower than the one you stated, then you stay with your choice. That is, you do the option that you prefer according to the bonus you stated. Note that this means that if you state a bonus of $1.1, then you will never be asked to switch, since the highest bonus we may offer is $1.0.}

      @:p{Continuing on from our example, suppose that you had stated a bonus of $0.50 and that this choice is randomly picked as the decision that counts. Then if the random bonus offered is $0.40 or less, you will stick with Option B and do the additional work of categorizing @conf['pilot-example-option-B-n] abstracts into 'Banking' or 'Other', while if the random bonus offered is $0.50 or more, you will do the additional work of categorizing @conf['pilot-example-option-A-n] abstracts into 'Social Preferences' or 'Other'.}

      @:h2{Short Surveys and Feedback}

      @:p{There are several short surveys, including a final survey where you can provide general feedback.}

      @:h2{Payments}

      @:p.info{All payments will be made within 3 days of you completing the tutorial.}

      @:p{There are three types of payment:}

      @:ul{
        @:li{@:strong{Complete Tutorial (~@conf['pilot-tutorial-duration-estimate] mins):} If you complete the tutorial and pass the comprehension test, you receive @$conf['pilot-tutorial-fee] in the form of the baseline payment.}
        @:li{@:strong{Complete Main Session (~@conf['pilot-study-duration-estimate] mins):} If you also complete the main session, which requires you to do all the surveys and categorize at least 66% of the abstracts correctly, you receive an additional @$conf['pilot-completion-fee] in the form of bonus payments.}
        @:li{@:strong{Correct Abstract Categorization:} If you complete the study, you receive an additional @$conf['pilot-correct-abstract-bonus] for every abstract of the main session (not the tutorial) that you categorize correctly, to reward those who received more abstracts.}}

      }))

(define (instructions)
  (haml
   @.container{
               @:h1{Instructions}

               @:p{This study consists of two sessions. If you decide to participate, the first session will start immediately after you consent to participate. You can complete the second session anytime @conf['day2-date] between 6 AM and midnight (Pacific Time). You will receive a completion bonus of @conf['session1-bonus] USD for completing the first session, plus @conf['session2-bonus] USD if you also complete the second session. You can earn additional income depending on your decisions in the first session and your performance in the task described below. The average additional income will be around @conf['estimated-performance-bonus] USD. The two sessions will take around @conf['estimated-time] minutes to complete each.}

               @:p{In both sessions, you will have to do a series of simple tasks. This task consists of categorizing abstracts (short summaries of research papers) by topics. Specifically, we will give you topics, and you should choose which topic this abstract belongs to.

                      In both sessions you will have to categorize @conf['baseline-abstracts] abstracts each as a baseline. In one of the two sessions, you will also have to do some extra work, involving the categorization of around @conf['estimated-extra-abstracts] extra abstracts.}

               @:p{On the next page, you will see examples abstracts and their categorizations. On the subsequent pages, you can try the categorization task yourself.}

               @:h2{First session - TODAY}

               @:p{In the first session, you have to make a series of decisions about how many extra abstracts to categorize, how (i.e., which topic to select), and when. You can see an example choice here:}

               @:ul{
                    @:li{25 abstracts as animal rights vs. other in the second session OR}
                    @:li{32 abstracts as information technology vs. other in the second session}}

               @:p{Sometimes both options will involve work in the second session, sometimes both in the first session, and sometimes you will have to choose whether to work in the first or the second session.}

               @:p{In some choices, you will have the opportunity to reveal a reason for or against choosing either option. In these cases, you will learn whether the reasons are for or against the options before the decision. You will have to choose one option to reveal the reason for/against. These reasons will not provide additional information about how to do the tasks. Rather they will inform you about some aspects of the topics you can choose from.}

               @:p{After you have made all decisions, one will be randomly chosen, and the option you chose will be implemented. For example, let's say the example choice above is the randomly chosen decision, and you chose to do 25 abstracts as animal rights vs. other in the second session. Then you will have to categorize zero extra abstracts today and 25 extra abstracts as animal rights vs. other in the second session.}

               @:p{After you learn the outcome of your decisions, you will have to categorize the @conf['baseline-abstracts] baseline abstracts and then do the extra categorization corresponding to the randomly chosen decision.}

               @:p{When you finish the tasks, the first session will end, and you will receive a link to the second session.}

               @:h2{Second session - @conf['day2-date]}

               @:p{In the second session, first, you will have to categorize the @conf['baseline-abstracts] baseline abstracts and then do the extra categorization corresponding to the randomly chosen decision. After you finish the tasks, we will ask you to answer a couple of questions about how you made your decisions and how well a couple of sentences describe your behavior or attitude in particular situations.}


               @:h2{Payment}

               @:p{The payment for the study will consist of three parts:}

               @:ul{
                    @:li{First session: a completion bonus of @conf['session1-bonus] USD for completing the first session}
                    @:li{Second session: a completion bonus of @conf['session2-bonus] USD for completing the second session}
                    @:li{Additional income: @conf['piece-rate-per-abstract] USD for each correctly categorized abstract - this includes the @conf['baseline-abstracts] baseline abstracts each session and the extra abstracts that correspond to the randomly chosen decision.}}

               @:p{You will receive all payments (completion bonuses plus additional income) via the Prolific payment system within three days after completing the second session. If you only complete the first session, you will only receive the completion bonus for the first session within three days after the corresponding second session.}

               @button[void]{Continue}}))
