(deftemplate teacher (slot ID) (slot weight))
(deftemplate class (slot ID))
(deftemplate classroom (slot ID) (slot type))
(deftemplate lesson (slot ID) (slot state) (slot class) (slot teacher) (slot type) (multislot time) (multislot room)) 
(deftemplate favorite-time (slot teacher) (multislot time))
(deftemplate refuse-time (slot teacher) (multislot time))

(deffacts result-data
  (count-lesson 0)
  (count-teacher-conflict 0)
  (count-class-conflict 0)
  (count-room-conflict 0)
  (count-room-error 0)
  (count-refuse 0)
  (count-favorite 0)
  (count-balance 0)
  (alltime 101 102 103 104 105 106 107 108 109 110 
	   201 202 203 204 205 206 207 208 209 210
	   301 302 303 304 305 306 307 308 309 310
	   401 402 403 404 405 406 407 408 409 410
	   501 502 503 504 505 506 507 508 509 510)
  (teacher (ID 61001) (weight 0))
  (teacher (ID 61002) (weight 0))
  (teacher (ID 61003) (weight 0))
  (teacher (ID 61004) (weight 0))
  (teacher (ID 61005) (weight 0))
  (teacher (ID 61006) (weight 0))
  (teacher (ID 61007) (weight 0))
  (teacher (ID 61008) (weight 0))
  (teacher (ID 61009) (weight 0))
  (teacher (ID 61010) (weight 0))
  (teacher (ID 61011) (weight 0))
  (teacher (ID 61012) (weight 0))
  (class (ID csie96))
  (class (ID csie97))
  (class (ID csie98))
  (class (ID csie99))
  (classroom (ID 31503) (type laboratory))
  (classroom (ID 32101) (type computer))
  (classroom (ID 31304) (type lecture))
  (classroom (ID 31305) (type lecture))
  (classroom (ID 31306) (type lecture))
  (favorite-time (teacher 61001) (time 202 203 302 303 402 403))
  (refuse-time (teacher 61001) (time 105 106 107 205 206 207 305 306 307 405 406 407 505 506 507))
  (favorite-time (teacher 61002) (time 105 106 205 206 305 306 405 406 505 506))
  (refuse-time (teacher 61002) (time 101 103 109 201 203 209 301 303 309 401 403 409 501 503 509))
  (favorite-time (teacher 61003) (time 204 205 206 304 305 306 404 405 406))
  (refuse-time (teacher 61003) (time 101 102 103 104 105 106 107 108 502 505 506 507 508 509 510))
  (favorite-time (teacher 61004) (time 403 404 405 406 503 504 505 506))
  (refuse-time (teacher 61004) (time 101 102 103 106 109 201 202 203 206 209 301 302 303 306 309))
  (favorite-time (teacher 61005) (time 103 104 105 203 204 205 303 304 305 403 404 405))
  (refuse-time (teacher 61005) (time 107 108 109 207 208 209 307 308 309 407 408 409 507 508 509))
  (favorite-time (teacher 61006) (time 105 106 205 206 305 306 405 406 505 506))
  (refuse-time (teacher 61006) (time 101 103 109 201 203 209 301 303 309 401 403 409 501 503 509))
  (favorite-time (teacher 61007) (time 205 206 207 305 306 307 405 406 407))
  (refuse-time (teacher 61007) (time 101 103 109 201 203 209 301 303 309 401 403 409 501 503 509))
  (favorite-time (teacher 61008) (time 205 206 207 305 306 307 405 406 407))
  (refuse-time (teacher 61008) (time 101 102 110 201 202 210 301 302 310 401 402 410 501 502 510))
  (favorite-time (teacher 61009) (time 206 207 208 306 307 308 406 407 408))
  (refuse-time (teacher 61009) (time 103 106 109 201 202 210 301 302 310 401 402 410 503 506 509))
  (favorite-time (teacher 61010) (time 205 206 207 305 306 307 405 406 407))
  (refuse-time (teacher 61010) (time 101 103 109 201 203 209 301 303 309 401 403 409 501 503 509))
  (favorite-time (teacher 61011) (time 102 103 202 203 302 303 402 403 502 503))
  (refuse-time (teacher 61011) (time 108 109 110 208 209 210 308 309 310 408 409 410 508 509 510))
  (favorite-time (teacher 61012) (time 204 205 206 207 404 405 406 407))
  (refuse-time (teacher 61012) (time 101 102 103 106 108 301 302 303 304 501 502 503 506 508 510))
)

(defrule assert-result
	(declare (salience 10000))
	=>
	(load-facts "C:/Users/Leo/Desktop/Code Data/expert system/final/result.txt"))

(defrule check-lesson-data
        (lesson (ID ?id) (state ?) (class ?class) (teacher ?teacher) (type ?type) (time ?t1 ?t2 ?t3&:(= (+ ?t1 2) ?t3)) (room ?room ?room ?room))
        (class (ID ?class))
        (teacher (ID ?teacher) (weight ?))
        (classroom (ID ?room) (type ?type))
        (alltime $? ?t1 ?t2 ?t3 $?)
        =>
        (assert (lesson-ok ?id)))

(defrule count-lesson-data
        ?f1 <- (lesson-ok $?)
        ?f2 <- (count-lesson ?num)
        =>
        (retract ?f1 ?f2)
        (assert (count-lesson (+ ?num 1))))

(defrule check-teacher-conflict
	(lesson (ID ?lesson1) (state ?) (class ?) (teacher ?teacher) (type ?) (time $? ?same $?) (room $?))
	(lesson (ID ?lesson2&:(> ?lesson2 ?lesson1)) (state ?) (class ?) (teacher ?teacher) (type ?) (time $? ?same $?) (room $?))
        =>
        (assert (teacher-conflict ?lesson1 ?lesson2 ?same)))

(defrule count-teacher-conflict
        ?f1 <- (teacher-conflict $?)
        ?f2 <- (count-teacher-conflict ?num)
        =>
        (retract ?f1 ?f2)
        (assert (count-teacher-conflict (+ ?num 1))))

(defrule check-class-conflict
	(lesson (ID ?lesson1) (state ?) (class ?class) (teacher ?) (type ?) (time $? ?same $?) (room $?))
	(lesson (ID ?lesson2&:(> ?lesson2 ?lesson1)) (state ?) (class ?class) (teacher ?) (type ?) (time $? ?same $?) (room $?))
        =>
        (assert (class-conflict ?lesson1 ?lesson2 ?same)))

(defrule count-class-conflict
        ?f1 <- (class-conflict $?)
        ?f2 <- (count-class-conflict ?num)
        =>
        (retract ?f1 ?f2)
        (assert (count-class-conflict (+ ?num 1))))

(defrule check-room-conflict
	(lesson (ID ?lesson1) (state ?) (class ?) (teacher ?) (type ?) (time $? ?same $?) (room ?room ?room ?room))
	(lesson (ID ?lesson2&:(> ?lesson2 ?lesson1)) (state ?) (class ?) (teacher ?) (type ?) (time $? ?same $?) (room ?room ?room ?room))
        =>
        (assert (room-conflict ?lesson1 ?lesson2 ?same)))

(defrule count-room-conflict
        ?f1 <- (room-conflict $?)
        ?f2 <- (count-room-conflict ?num)
        =>
        (retract ?f1 ?f2)
        (assert (count-room-conflict (+ ?num 1))))

(defrule check-room-type-error
	(lesson (ID ?lesson) (state ?) (class ?) (teacher ?) (type ?type) (time $?) (room $? ?room $?))
        (classroom (ID ?room) (type ~?type))	
        =>
        (assert (room-error ?lesson ?room)))

(defrule count-room-type-error
        ?f1 <- (room-error $?)
        ?f2 <- (count-room-error ?num)
        =>
        (retract ?f1 ?f2)
        (assert (count-room-error (+ ?num 1))))

(defrule check-refuse
	(lesson (ID ?lesson) (state ?) (class ?) (teacher ?teacher) (type ?) (time $? ?same $?) (room $?))
        (teacher (ID ?teacher) (weight ?))
        (refuse-time (teacher ?teacher) (time $? ?same $?))
        =>
        (assert (find-refuse ?lesson ?same)))

(defrule count-refuse
        ?f1 <- (find-refuse $?)
        ?f2 <- (count-refuse ?num)
        =>
        (retract ?f1 ?f2)
        (assert (count-refuse (+ ?num 1))))

(defrule check-favorite
	(lesson (ID ?lesson) (state ?) (class ?) (teacher ?teacher) (type ?) (time $? ?same $?) (room $?))
        (teacher (ID ?teacher) (weight ?))
        (favorite-time (teacher ?teacher) (time $? ?same $?))
        =>
        (assert (find-favorite ?lesson ?same)))

(defrule count-favorite
        ?f1 <- (find-favorite $?)
        ?f2 <- (count-favorite ?num)
        =>
        (retract ?f1 ?f2)
        (assert (count-favorite (+ ?num 1))))

(defrule check-teacher-favorite
        (lesson (ID ?) (state ?) (class ?) (teacher ?teacher) (type ?) (time $? ?same $?) (room $?))
        (teacher (ID ?teacher) (weight ?))
        (favorite-time (teacher ?teacher) (time $? ?same $?))
        =>
        (assert (teacher-favorite ?teacher ?same)))

(defrule generate-count-teacher-favorite
        (teacher (ID ?teacher) (weight ?))
        =>
        (assert (count-teacher-favorite ?teacher 0)))

(defrule count-teacher-favorite
        ?f1 <- (teacher-favorite ?teacher ?)
        ?f2 <- (count-teacher-favorite ?teacher ?num)
        =>
        (retract ?f1 ?f2)
        (assert (count-teacher-favorite ?teacher (+ ?num 1))))

(defrule print-teacher-weight
        (declare (salience -100))
        (count-teacher-favorite ?teacher ?num)
        =>
        (printout t "NO " ?teacher " ASSERT " ?num " FAV-time" crlf)) 

(defrule sum-difference
        (declare (salience -200))
        (count-favorite ?cf)
        ?f1 <- (count-teacher-favorite ?teacher ?num)
        ?f2 <- (count-balance ?cb)
        =>
        (retract ?f1 ?f2)
        (assert (count-balance (+ ?cb (** (- ?num (/ ?cf 12)) 2)))))

(defrule print-score
        (declare (salience -300))
        (count-balance ?cb)
        (count-lesson ?cl) 
        (count-teacher-conflict ?ctc)
        (count-class-conflict ?ccc)
        (count-room-conflict ?crc)
        (count-room-error ?cre)
        (count-refuse ?cr)
        (count-favorite ?cf)
        =>
        (bind ?ba (sqrt (/ ?cb 12)))
        (printout t "TOTAL " ?cf " FAV-TIME, AVG " (/ ?cf 12) " FAV-TIME" crlf)
        (printout t "EACH FAV-TIME Standard division is " ?ba crlf crlf)
        (printout t "Total has 40 class, finished " ?cl " , not-finished " (- 40 ?cl) "(each fail -30)" crlf)
        (printout t "Teacher Stack Total1 " ?ctc " " crlf)
        (printout t "Class Statck Total2 " ?ccc " " crlf)
        (printout t "Classroom Stack Total3 " ?crc " " crlf)
        (printout t "Type Error Total4 " ?cre " " crlf)
        (printout t "In Refuse Time Total5 " ?cr " " crlf)
        (printout t "Fav Total6 " ?cf " " crlf)
        (printout t "SD is " ?ba " , " crlf)
        (printout t "TOTAL SCORE 2*" ?cf " - 30*" (- 40 ?cl) " - 10*(" ?ctc "+" ?ccc "+" ?crc "+" ?cre "+" ?cr ") + 5*" (integer (/ (- 1.5 ?ba) 0.1)) " = " (- (+ (* 2 ?cf) (* 5 (integer (/ (- 1.5 ?ba) 0.1)))) (* 30 (- 40 ?cl)) (* 10 (+ ?ctc ?ccc ?crc ?cre ?cr))) crlf))



