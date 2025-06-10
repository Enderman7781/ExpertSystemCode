(deftemplate error(slot problem))
(deftemplate error-detailed(slot detailed))

(defrule error-problem-A-1
    (error-detailed(detailed noise-when-brake))
    =>
    (assert (error (problem brake-problem)))
)
(defrule error-problem-A-2
    (error-detailed(detailed noise-form-tire))
    =>
    (assert (error (problem brake-problem)))
)

(defrule error-problem-B-1
    (error-detailed(detailed water-thermometer-H))
    =>
    (assert (error (problem water-tank-problem)))
)
(defrule error-problem-B-2
    (error-detailed(detailed water-leak))
    =>
    (assert (error (problem water-tank-problem)))
)

(defrule error-problem-C-1
    (error-detailed(detailed noise-from-engine-room))
    =>
    (assert (error (problem engine-belt-is-too-loose)))
)

(defrule error-problem-D-1
    (error-detailed(detailed engine-cannot-catch))
    =>
    (assert (error (problem car-battery-has-no-power)))
)

(defrule deal-problem-A
    (error (problem brake-problem))
    =>
    (printout t "check brake fluid and pedal" crlf)
)
(defrule deal-problem-B
    (error (problem water-tank-problem))
    =>
    (printout t "repair the water tank or add water" crlf)
)

(defrule deal-problem-C
    (error (problem engine-belt-is-too-loose))
    =>
    (printout t "change the engine belt" crlf)
)

(defrule deal-problem-D
    (error (problem car-battery-has-no-power))
    =>
    (printout t "replace or change the car battery" crlf)
)

(deffacts initial 
    (error-detailed(detailed noise-when-brake))    
    (error-detailed(detailed noise-form-tire))
    (error-detailed(detailed water-thermometer-H))
    (error-detailed(detailed water-leak))
    (error-detailed(detailed noise-from-engine-room))
    (error-detailed(detailed engine-cannot-catch))
)

