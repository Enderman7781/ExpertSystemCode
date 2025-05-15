; a black dog bite a small white cat at the yard
; a dog bite the a cat in house
(defmodule MAIN (export deftemplate sentence))

(defrule MAIN::control-rule
    (not (sentence $?))
    =>
    (printout t "Enter a sentence (<Enter> to End): ")
    (assert (sentence(explode$ (readline)) <end>))
    (focus PRODUCTION RECONGNIZE)
)

(defmodule PRODUCTION (import MAIN deftemplate sentence)
(export deftemplate production))

(deffacts PRODUCTION::productions
    ;Dictionary
    (production <determiner> a)
    (production <determiner> an)
    (production <determiner> the)
    (production <adjective> small)
    (production <adjective> big)
    (production <adjective> white)
    (production <adjective> black)
    (production <noun> dog)
    (production <noun> cat)
    (production <noun> yard)
    (production <noun> house)
    (production <preposition> at)
    (production <preposition> in)
    (production <preposition> with)
    (production <verb> pursue)
    (production <verb> catch)
    (production <verb> bite)
    (production <verb> scratch)

    ; Term Deffiition
    (production <noun> <adjective> <noun>)
    (production <term> <determiner> <noun>)
    (production <indirect-object> <preposition> <term>)
    (production <sentence> <term> <verb> <term> <end>)
    (production <sentence> <term> <verb> <term> <indirect-object> <end>)
)

(defrule PRODUCTION::transfer
    ?f <- (sentence $?front ?s $?rear)
    (test(neq (sub-string 1 1 (str-cat ?s)) "<"))
    ?f2 <- (production ?pdct ?s)
    =>
    (retract ?f)
    (assert (sentence $?front ?pdct $?rear))
)

(defrule PRODUCTION::adj_to_noun
    (declare (salience 10))
    ?f <- (sentence $?front <adjective> <noun> $?rear)
    =>
    (retract ?f)
    (assert (sentence $?front <noun> $?rear))
)

(defrule PRODUCTION::det-noun_to_term
    (declare (salience 9))
    ?f <- (sentence $?front <determiner> <noun> $?rear)
    =>
    (retract ?f)
    (assert (sentence $?front <term> $?rear))
)

(defrule PRODUCTION::noun_to_term
    (declare (salience 8))
    ?f <- (sentence $?front <noun> $?rear)
    =>
    (retract ?f)
    (assert (sentence $?front <term> $?rear))
)

(defrule PRODUCTION::proposition-term_to_indr-obj
    (declare (salience 7))
    ?f <- (sentence $?front <preposition> <term> $?rear)
    =>
    (retract ?f)
    (assert (sentence $?front <indirect-object> $?rear))
)

(defmodule RECONGNIZE (import MAIN deftemplate sentence)
(import PRODUCTION deftemplate production))

(defrule RECONGNIZE::judge-halt
    (declare (salience 100))
    ?f <- (sentence $?s)
    (test (= (length$ $?s) 1))
    =>
    (halt)
)
(defrule RECONGNIZE::judge-success
    (declare (salience 5))
    ?f <- (sentence $?s)
    ?f2 <- (production <sentence> $?s)
    =>
    (printout t crlf "Correct!" crlf)
    (retract ?f)
)
(defrule RECONGNIZE::judge-fail
    ?f <- (sentence $?s)
    =>
    (printout t crlf "Wrong!" crlf)
    (retract ?f)
)





