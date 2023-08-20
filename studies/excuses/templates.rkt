#lang at-exp racket

(require congame/components/study
         koyo/haml
         "abstract-categorization.rkt")

(provide instructions)

(define config
  (hash 'day2-date "TOMORROW"
        'session1-bonus 3
        'session2-bonus 6
        'estimated-performance-bonus 6
        'estimated-time 20))

(define (conf x)
  (~a (hash-ref config x)))

(define (instructions)
  (page
   (haml
    @.container{
      @:h1{Instructions}

      @:p{This study consists of two sessions. If you decide to participate, the first session will start immediately after you consent to participate. You can complete the second session anytime @conf['day2-date] between 6 AM and midnight (Pacific Time). You will receive a completion bonus of @conf['session1-bonus] USD for completing the first session, plus @conf['session2-bonus] USD if you also complete the second session. You can earn additional income depending on your decisions in the first session and your performance in the task described below. The average additional income will be around @conf['estimated-performance-bonus] USD. The two sessions will take around @conf['estimated-time] minutes to complete each.}

      @:p{In both sessions, you will have to do a series of simple tasks. The task consists of categorizing research articles into topics based on their abstracts (a roughly 100-word summary of the article). You will be given specific topics, and you will have to choose the articles concerned with that topic.

      In both sessions you will have to categorize [ 15 ] abstracts each as a baseline. In one of the two sessions, you will also have to do some extra work, involving the categorization of around [ 30-40 ] extra abstracts.}


      @:h2{First session - TODAY}

      @:p{In the first session, you have to make a series of decisions about how many extra abstracts to categorize, how (i.e., which topic to select), and when. You can see an example choice here:}

      @:ul{
        @:li{25 [ abstracts as animal rights vs. other ]  in the second session OR}
        @:li{32 [ abstracts as information technology vs. other ] in the second session}}

      @:p{Sometimes both options will involve work in the second session, sometimes both in the first session, and sometimes you will have to choose whether to work in the first or the second session.}

      @:p{In some choices, you will have the opportunity to reveal a reason for or against choosing either option. In these cases, you will learn whether the reasons are for or against the options before the decision. You will have to choose one option to reveal the reason for/against. These reasons will not provide additional information about how to do the tasks. Rather they will inform you about some aspects of the topics you can choose from.}

      @:p{After you have made all decisions, one will be randomly chosen, and the option you chose will be implemented. For example, let's say the example choice above is the randomly chosen decision, and you chose to do 25 abstracts as animal rights vs. other in the second session. Then you will have to categorize zero extra abstracts today and 25 extra abstracts as animal rights vs. other in the second session.}

      @:p{After you learn the outcome of your decisions, you will have to categorize the 15 baseline abstracts and then do the extra categorization corresponding to the randomly chosen decision.}

      @:p{When you finish the tasks, the first session will end, and you will receive a link to the second session.}

      @:h2{Second session - @conf['day2-date]}

      @:p{In the second session, first, you will have to categorize the 15 baseline abstracts and then do the extra categorization corresponding to the randomly chosen decision. After you finish the tasks, we will ask you to answer a couple of questions about how you made your decisions and how well a couple of sentences describe your behavior or attitude in particular situations.}


      @:h2{Payment}

      @:p{The payment for the study will consist of three parts:}

      @:ul{
        @:li{First session: a completion bonus of [ 3 ] USD for completing the first session}
        @:li{Second session: a completion bonus of [ 6 ] USD for completing the second session}
        @:li{Additional income: [ 0.2 ] USD for each [ correctly categorized abstract ] - this includes the baseline [ 15 abstracts ]  each session and the extra [ abstracts ] that correspond to the randomly chosen decision.}}

      @:p{You will receive all payments (completion bonuses plus additional income) via the Prolific payment system, within three days after completing the second session. If you only complete the first session, you will only receive the completion bonus for the first session, within three days after the corresponding second session.}

      @button[void]{Continue}})))
