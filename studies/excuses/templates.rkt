#lang at-exp racket

(require congame/components/study
         koyo/haml
         "abstract-categorization.rkt")

(provide
 instructions
 pilot-instructions)

(define (~$ a)
  (format "$~a" (~r a #:precision 2)))

(define config
  (hash 'day2-date "TOMORROW"
        'session1-bonus 3
        'session2-bonus 6
        'estimated-performance-bonus 6
        'estimated-time 20
        'piece-rate-per-abstract 0.2
        'baseline-abstracts 15
        'estimated-extra-abstracts 35
        'pilot-tutorial-duration-estimate "5-10"
        'pilot-study-duration-estimate 20
        'pilot-tutorial-fee 2
        'pilot-completion-fee 5
        'pilot-correct-abstract-bonus 0.05))

(define (conf x)
  (~a (hash-ref config x)))

(define ($conf x)
  (~$ (hash-ref config x)))

(define (pilot-instructions)
  (page
   (haml
    @.container{
      @:h1{Instructions}

      @:p{This study consists of a brief (~@conf['pilot-tutorial-duration-estimate] mins) tutorial session followed by the main session. After the tutorial, you will receive the completion code and you will be asked whether you want to participate in the main study, which starts right after the tutorial. If you agree to participate, you receive additional payments as bonus payments if you complete the main session.}

      @:h2{Main Study}

      @:p{The main study is structured as follows:}

      @:ol{
        @:li{@:strong{Decision Stage:} You make a series of decisions about how many and what type of extra abstracts to categorize.
          @:ul{
            @:li{@:strong{Reveal Reasons:} For some decisions, both options have a button that can reveal a reason for or against that option, and you must reveal exactly one of the reasons before choosing an option.}}}
        @:li{@:strong{Determining the Decision that Counts:} After you have made all the decisions, one decision will be randomly chosen, and we will inform you about that choice.}
        @:li{@:strong{Categorize Abstracts:} You then categorize the extra abstracts from that decision that counts.}
        @:li{@:strong{Final survey:} You complete a final survey about the study, including any comments you may have.}}

      @:p{The rest of the tutorial walks you through each of these steps briefly, providing you with example choice, a few abstracts to categorize, and a comprehension test to check that you understood the instructions. After that, you can decide whether to participate in the main study.}

      @:h2{Payments}

      @:p{There are three types of payment:}

      @:ul{
        @:li{@:strong{Complete Tutorial (~@conf['pilot-tutorial-duration-estimate] mins):} If you complete the tutorial, you receive @$conf['pilot-tutorial-fee] in the form of the baseline payment.}
        @:li{@:strong{Complete Main Session (~@conf['pilot-study-duration-estimate] mins):} If you also complete the main session within the permitted time after the tutorial, you receive an additional @$conf['pilot-completion-fee] in the form of bonus payments.}
        @:li{@:strong{Correct Abstract Categorization:} In addition, you receive a bonus of @$conf['pilot-correct-abstract-bonus] for every abstract that you categorize correctly.}}

      @:p{All payments will be made within 3 days of you completing the tutorial.}

      @(button void "Start Tutorial")})))

(define (instructions)
  (page
   (haml
    @.container{
      @:h1{Instructions}

      @:p{This study consists of two sessions. If you decide to participate, the first session will start immediately after you consent to participate. You can complete the second session anytime @conf['day2-date] between 6 AM and midnight (Pacific Time). You will receive a completion bonus of @conf['session1-bonus] USD for completing the first session, plus @conf['session2-bonus] USD if you also complete the second session. You can earn additional income depending on your decisions in the first session and your performance in the task described below. The average additional income will be around @conf['estimated-performance-bonus] USD. The two sessions will take around @conf['estimated-time] minutes to complete each.}

      @:p{In both sessions, you will have to do a series of simple tasks. This task consists of categorizing abstracts (short summaries of research papers) by topics. Specifically, we will give topics, and you should choose which topic this abstract belongs to.

      In both sessions you will have to categorize @conf['baseline-abstracts] abstracts each as a baseline. In one of the two sessions, you will also have to do some extra work, involving the categorization of around @conf['estimated-extra-abstracts] extra abstracts.}

      @:p{On the next page, you will see an example abstract and its categorization. On the subsequent pages, you can try the categorization task yourself.}


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

      @button[void]{Continue}})))
