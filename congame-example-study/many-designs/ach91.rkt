#lang conscript

;; ACH91:
;;
;; 1. People do tasks (here matrices adding up to 10) and self-report how many they got right.
;; 2. in non-comp they get $0.10 per correct matrix up to at most $1.00
;; 3. in comp they *additionally* get $0.70 if they have the higher score of a randomly matched participant
;;      - Question: When do participants know these extra incentives? If they know ex ante, they probably will put more effort into the tasks, not just lie more.
;;      - Answer (probably, from design doc): they are told before they do the tasks, so they might put in more effort
;;      - Question: do people know or are they made to believe that the experimenter can find out whether they lied or not?
;;      - Answer (probably, from design doc): no, since the outcome is the self-reported minus the actual number of correctly solved matrices.
;; 4. Moral behavior is self-reported score minus actual score. Zero is perceived as moral, higher values as more cheating.
;;      - Comment: According to this, we should not see - ever - numbers that are negative. It could be that people are genuinely unsure about some of their answers.
;;      - Comment: it could be that the higher incentives make people rush more, so they don't double check their answers, which reduces the probability they are right without affecting their belief in them being right. Hard to argue that this is competition if it happens.
;;
;; Suggested improvements/variations:
;;
;; - Don't tell participants about the extra bonus of $0.70 (or all incentives?) until after they completed the matrices, but before self-reporting their score. This avoids different amounts of getting tasks right.
;; - Change whether the experimenter could find out whether the answer is true or not.
;;
;; Notes for other designs:
;;
;; - they collected demographics (gender, age, occupation), we might want to do that generically for all experiments
