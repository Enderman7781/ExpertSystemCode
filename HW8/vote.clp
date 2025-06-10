(deftemplate ballot (slot id)(multislot order))
(deftemplate candidate (slot no)(slot votes))

(deffacts initial (phase load-data))
(defrule assert-data
    ?f <- (phase load-data)
    =>
    (retract ?f)
    (load-facts "C:/Users/Leo/Desktop/Code Data/expert system/voting-03.txt")
    (printout t "The 1 round:" crlf)
    (assert (phase before-cnt))
    (assert (round 1))
    (assert (remove))
    (assert (processed))
    (assert (least 10000))
)
(defrule hard-trick
    ?f <- (phase before-cnt)
    (ballot (id B001)(order $?order))
    =>
    (retract ?f)
    (assert (can_cnt (length$ $?order)))
    (assert (phase count))
)
(defrule count-first
    (phase count)
    (ballot (id ?id)(order ?first $?))
    ?f1 <- (candidate (no ?first)(votes ?votes))
    ?f2 <- (processed $?processed)
    (test (not (member$ ?id $?processed)))
    =>
    (retract ?f2)
    (modify ?f1 (votes (+ ?votes 1)))
    (assert (processed $?processed ?id))
)

(defrule change-phase-1
    (declare (salience -10))
    ?f <- (phase count)
    =>
    (retract ?f)
    (assert (phase find-elected))
)

; >= 50% halting
(defrule find-elected-halt
    ?f <- (phase find-elected)
    (candidate (no ?id)(votes ?votes))
    (remove $?remove)
    (total ?total)
    (test (not (member$ ?id $?remove)))
    (test (> (/ (float ?votes) (float ?total)) 0.5))
    =>
    (printout t "Candidate " ?id " is elected. Number of votes: " ?votes crlf)
    (assert (phase halt))
    (retract ?f)
)

(defrule find-elected-else
    (phase find-elected)
    (candidate (no ?id)(votes ?votes))
    (remove $?remove)
    ?f_least <- (least ?least)
    (test (not (member$ ?id $?remove)))
    (test (< ?votes ?least))
    =>
    (retract ?f_least)
    (assert (least ?votes))
)

(defrule find-elected-remove
    (declare (salience -1))
    (phase find-elected)
    ?f_c <- (candidate (no ?id)(votes ?votes))
    ?f_rm <- (remove $?remove)
    (least ?least)
    (test (not (member$ ?id $?remove)))
    (test (= ?votes ?least))
    =>
    (retract ?f_rm)
    (retract ?f_c)
    (printout t "Candidate " ?id " ")
    (assert (remove $?remove ?id))
)

(defrule failed
    (declare (salience -5))
    ?f <- (phase find-elected)
    (can_cnt ?can_cnt)
    (remove $?remove)
    (test (= ?can_cnt (length$ $?remove)))
    =>
    (retract ?f)
    (assert (phase halt))
    (printout t crlf "Fail: All candidate are removed" crlf)
)

(defrule change-phase-2
    (declare (salience -10))
    ?f <- (phase find-elected)
    (least ?least)
    =>
    (retract ?f)
    (assert (phase next-round))
    (printout t "removed. Number of Votes: " ?least crlf)
)

(defrule reset
    (phase next-round)
    ?f_v <- (candidate (no ?no)(votes ?votes))
    =>
    (modify ?f_v (votes 0))
)

(defrule remove-elimated
    (phase next-round)
    ?f <- (ballot (id ?)(order $?front ?no $?rear))
    (remove $?remove)
    (test (member$ ?no $?remove))
    =>
    (modify ?f (order $?front $?rear))
)

(defrule change-phase-3
    (declare (salience -10))
    ?f <- (phase next-round)
    ?f_rnd <- (round ?round)
    ?f_least <- (least ?)
    ?f_p <- (processed $?)
    =>
    (printout t "The " (+ ?round 1) " round:" crlf)
    (retract ?f)
    (retract ?f_least)
    (retract ?f_rnd)
    (retract ?f_p)

    (assert (round (+ ?round 1)))
    (assert (least 10000))
    
    (assert (phase count))
    (assert (processed))
)

(defrule f-halt
    (declare (salience -5))
    ?f <- (phase next-round)
    (round ?round)
    (test (> ?round 15))
    =>
    (retract ?f)
    (assert(phase halt))
)
