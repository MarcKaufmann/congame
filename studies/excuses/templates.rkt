#lang at-exp racket

(require congame/components/study
         koyo/haml)

(provide instructions task-description)

(define config
  (hash 'day2-date "tomorrow"
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

      @:p{This study consists of two sessions. If you decide to take part, the first session will start immediately after you give consent to participate. You can complete the second session anytime @conf['day2-date] between 6 AM and midnight (Pacific Time). You will receive a completion bonus of @conf['session1-bonus] USD for completing the first session, plus @conf['session2-bonus] USD if you complete the second session as well. You can earn additional income that will depend on your decisions in the first session and your performance in the task that we describe below. The average additional income will be around @conf['estimated-performance-bonus] USD. The two sessions will take around @conf['estimated-time] minutes to complete each.}

      @:p{In both sessions, you will have to do a series of simple tasks. The task consists of
    [[[
    categorizing articles based on their abstracts (a roughly 100-word summary of the article) . You will be given certain topics and you will have to choose the articles that are concerned with that particular topic. Here you can try the task:

    Page with 3 abstracts and having to choose the ones that are on [ climate change ] comes here.

    In both sessions you will have to categorize [ 15 ] abstracts each as a baseline. In one of the two sessions, you will also have to do some extra work, involving the categorization of around [ 30-40 ] extra abstracts.
    ]]]}


      @:h2{First session - TODAY}

      @:p{In the first session, you will first have to make a series of decisions regarding how many extra  [ abstracts to categorize and how (i. e. which topic to select) ], and when. You can see an example choice here:}

      @:ul{
        @:li{25 [ abstracts as animal rights vs other ]  in the second session OR}
        @:li{32 [ abstracts as information technology vs other ] in the second session}}

      @:p{Sometimes both options will involve work in the second session, sometimes both in the first session, and sometimes you will have to choose between work in the first or in the second sessions.}

      @:p{In some choices, you will have the opportunity to reveal a reason for or against choosing either option. In these cases, you will learn before the decision if the reasons are for or against the options. You will have to choose one option to reveal the reason for. These reasons will not provide additional information about how to do the tasks, rather they will inform you about [ other aspects of the topics you can choose from. ]}

      @:p{After you have made all decisions, one of them will be randomly chosen and the option you chose will be implemented. For example, if the example choice above is the randomly chosen decision, and you chose to do 25 [ abstracts  as animal rights vs other ]  in the second session, then you will have to [ categorize ] zero extra [abstracts ] today and 25 [ abstracts categorized as animal rights vs other ] in the second session.}

      @:p{After you learn the outcome of your decisions, you will have to [ categorize ] the [ 15 ] baseline [ abstracts ], and then do the amount of extra [ abstract categorization ] corresponding to the randomly chosen decision.}

      @:p{When you are finished with the tasks, the first session will end and you will receive a link to the second session.}

      @:h2{Second session - @conf['session2-bonus]}

      @:p{In the second session, first you will have to [ categorize ] the [ 15 ] baseline [ abstracts ], and then you will have to do the amount of extra [ abstract categorization ] corresponding to the randomly chosen decision. After you are finished with the tasks, we will ask you to answer a couple of questions about how you made your decisions, and about how well a couple of sentences describe your behavior or attitude in some situations.}


      @:h2{Payment}

      @:p{The payment for the study will consist of three parts:}

      @:ul{
        @:li{First session: a completion bonus of [ 3 ] USD for completing the first session}
        @:li{Second session: a completion bonus of [ 6 ] USD for completing the second session}
        @:li{Additional income: [ 0.2 ] USD for each [ correctly categorized abstract ] - this includes the baseline [ 15 abstracts ]  each session and the extra [ abstracts ] that correspond to the randomly chosen decision.}}

      @:p{You will receive all payments (completion bonuses plus additional income) via the Prolific payment system, within three days after completing the second session. If you only complete the first session, you will only receive the completion bonus for the first session, within three days after the corresponding second session.}

      @button[void]{Continue}})))


(define (task-description)
  (page
   (haml
    (.container
     @:h1{Task Description}

     @:p{In this study, you will have to do a series of simple tasks. This task consists of categorizing abstracts (short summaries of research papers) by topics. Specifically, we will give specific topics, and you should choose which topic this abstract belongs to.}

     @:p{For example, we might provide you with the following topics FIXME and the following abstract FIXME.}

     @:p{This abstract belongs to the FIXME and FIXME categories.}

     @:p{Now it is your turn to do 3 tasks: continue to the next page to categorize 3 abstracts.}

     @button[void]{Continue}))))
