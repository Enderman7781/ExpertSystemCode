(deftemplate teacher (slot ID) (slot weight))
(deftemplate class (slot ID))
(deftemplate classroom (slot ID) (slot type))
(deftemplate lesson (slot ID) (slot state) (slot class) (slot teacher) (slot type) (multislot time) (multislot room)) 
(deftemplate favorite-time (slot teacher) (multislot time))
(deftemplate refuse-time (slot teacher) (multislot time))
(deftemplate pass-lesson (multislot pass)(multislot weight))
(deftemplate node (slot lesson)(multislot trap))

(deffacts initial
  (alltime 101 102 103 104 105 106 107 108 109 110 
	   201 202 203 204 205 206 207 208 209 210
	   301 302 303 304 305 306 307 308 309 310
	   401 402 403 404 405 406 407 408 409 410
	   501 502 503 504 505 506 507 508 509 510)
           
)

;以下的規則會讀取data.txt內的教師、課程、班級、教室、喜好時間、拒絕時間等事實，並assert fact

(defrule assert-data
	(declare (salience 10000))
	=>
	(load-facts "C:/Users/Leo/Desktop/Code Data/expert system/final/data.txt")
        ;(open "C:/Users/Leo/Desktop/Code Data/expert system/final/result.txt" res "w")
)
(defrule initial-node
	(declare (salience 9999))
        (lesson (ID ?id))
        (not (node (lesson ?id)))
	=>
	(assert (node (lesson ?id)(trap)))
)

(defrule start-program
        (declare (salience 9998))
        =>
	(assert (phase f-get-lesson))
        (assert (pass-lesson))
)

;以下的片段排課規則供參考用, 非唯一方法

(defrule start-lesson
	?p <- (phase f-get-lesson)
	(not (select ?))
        ; 選擇 沒有被選過的課程以及沒有被排的教授
	(teacher (ID ?teacher) (weight ?weight))
	(lesson (ID ?lesson) (state 0) (teacher ?teacher))
        ;(pass-lesson (pass $?f)(weight $?))
        
        ; 測試 這個課程是可以被排進去的(沒有在不能排的列表中)

	(not (and (teacher (ID ?else) (weight ?w2&:(< ?w2 ?weight)))
              	  (lesson (state 0) (teacher ?else))
                 )
        )
	=>
        (printout res "F-TRY: " ?lesson crlf)
        (retract ?p)
	(assert (select ?lesson))
        (assert (phase schedule-3))
)

(defrule select-lesson
	?p <- (phase get-lesson)
	(not (select ?))
        ; 選擇 沒有被選過的課程以及沒有被排的教授
	(teacher (ID ?teacher) (weight ?weight))
	(lesson (ID ?lesson) (state 0) (teacher ?teacher))
        (pass-lesson (pass $?f ?rear)(weight $?))
        
        ; 測試 這個課程是可以被排進去的(沒有在不能排的列表中)
        (node (lesson ?rear)(trap $?trap))

	(not (and (teacher (ID ?else) (weight ?w2&:(< ?w2 ?weight)))
              	  (lesson (state 0) (teacher ?else))
                 )
        )
        (test (not (member$ ?lesson $?trap)))
	=>
        (printout res "TRY: " ?lesson crlf)
        (retract ?p)
	(assert (select ?lesson))
        (assert (phase schedule-3))
)

(defrule backtrack-select-lesson-have-select
        ; 選不出最佳解，代表沒有選項了，往前再退一步
        (declare (salience -10))
        ?p <- (phase get-lesson)
      
        ?pas <-(pass-lesson (pass $?f ?rr ?rear)(weight $?pr-w ?w-rear))
        ?f1 <- (lesson (ID ?rear)(state ?state)(time $?time)(room $?room)(teacher ?teacher))
        ?f2 <- (teacher (ID ?teacher) (weight ?weight))
        ?t <- (node (lesson ?rear)(trap $?))
        ?t2 <- (node (lesson ?rr)(trap $?rrtrap))
	=>
        (printout res "BACK FROM: " ?rear crlf)
        (retract ?pas)
        (modify ?f1 (state 0)(time)(room)(teacher ?teacher))
        (modify ?f2 (weight (- ?weight ?w-rear)))
        (modify ?t (trap))
        (modify ?t2 (trap $?rrtrap ?rear))
        (assert (pass-lesson (pass $?f ?rr)(weight $?pr-w)))
)

(defrule schedule-3-favorite-time
        ?p <- (phase schedule-3)
        ?f1 <- (select ?select)
        ?pl <- (pass-lesson (pass $?front)(weight $?pr-w))
 
        ;還沒排過的課
        ?f2 <- (lesson (ID ?select) (state 0) (class ?class) (teacher ?teacher) (type ?type) (time) (room))
        ;隨便一個教授
        ?f3 <- (teacher (ID ?teacher) (weight ?weight))

        ; 教授的喜好時間
        ; t3 = t1 + 2
        (alltime $? ?t1 ?t2 ?t3 $?)

        ; 檢查時間有沒有在同一天
        (test (and 
            (= (+ ?t1 1) ?t2)
            (= (+ ?t2 1) ?t3)
            (= (div ?t1 100) (div ?t2 100) (div ?t3 100)))
        )

        ; 教授的拒絕時間，喜好時間有沒有在拒絕時間內
        (refuse-time (teacher ?teacher) (time $?r-t))
        (test (not (or (member$ ?t1 $?r-t) (member$ ?t2 $?r-t) (member$ ?t3 $?r-t))))

        ; 所選的時間有k門都在教授的喜好時間內
        (favorite-time (teacher ?teacher) (time $?f-t))
        (test (and (member$ ?t1 $?f-t) (member$ ?t2 $?f-t) (member$ ?t3 $?f-t)))

        ; 選擇與課程相符的教室狀態
        (classroom (ID ?classroom) (type ?type))
        ; 不可以是排過的課、排過的教授、排過的地點
        (not (lesson (teacher ?teacher) (time $? ?t1|?t2|?t3 $?)))
        (not (lesson (class ?class) (time $? ?t1|?t2|?t3 $?)))
        (not (lesson (time $? ?t1|?t2|?t3 $?) (room ?classroom ?classroom ?classroom)))
        =>
        (retract ?p ?f1)
        (assert (phase get-lesson))
        ;喜好有三門, +3
        (modify ?f3 (weight (+ ?weight 3)))
        ; 這個時段教室有人了
        (modify ?f2 (state 1) (time ?t1 ?t2 ?t3) (room ?classroom ?classroom ?classroom))
        (modify ?pl (pass $?front ?select)(weight $?pr-w 3))
)

(defrule schedule-2-favorite-time
        ?p <- (phase schedule-3)
        ?f1 <- (select ?select)
        ?pl <- (pass-lesson (pass $?front)(weight $?pr-w))
 
        ;還沒排過的課
        ?f2 <- (lesson (ID ?select) (state 0) (class ?class) (teacher ?teacher) (type ?type) (time) (room))
        ;隨便一個教授
        ?f3 <- (teacher (ID ?teacher) (weight ?weight))

        ; 教授的喜好時間
        ; t2 = t1 + 1
        (alltime $? ?t1 ?t2 ?t3 $?)

        ; 檢查時間有沒有在同一天
        (test (and 
            (= (+ ?t1 1) ?t2)
            (= (+ ?t2 1) ?t3)
            (= (div ?t1 100) (div ?t2 100) (div ?t3 100)))
        )

        ; 教授的拒絕時間，喜好時間有沒有在拒絕時間內
        (refuse-time (teacher ?teacher) (time $?r-t))
        (test (not (or (member$ ?t1 $?r-t) (member$ ?t2 $?r-t) (member$ ?t3 $?r-t))))

        ; 所選的時間有k門都在教授的喜好時間內
        (favorite-time (teacher ?teacher) (time $?f-t))
        (test 
            (= 
                (+ 
                    (if (member$ ?t1 $?f-t) then 1 else 0)
                    (if (member$ ?t2 $?f-t) then 1 else 0)
                    (if (member$ ?t3 $?f-t) then 1 else 0)
                )
            2)
        )
        

        ; 選擇與課程相符的教室狀態
        (classroom (ID ?classroom) (type ?type))
        ; 不可以是排過的課、排過的教授、排過的地點
        (not (lesson (teacher ?teacher) (time $? ?t1|?t2|?t3 $?)))
        (not (lesson (class ?class) (time $? ?t1|?t2|?t3 $?)))
        (not (lesson (time $? ?t1|?t2|?t3 $?) (room ?classroom ?classroom ?classroom)))
        =>
        (retract ?p ?f1)
        (assert (phase get-lesson))
        ;喜好有2門, +2
        (modify ?f3 (weight (+ ?weight 2)))
        ; 這個時段教室有人了
        (modify ?f2 (state 1) (time ?t1 ?t2 ?t3) (room ?classroom ?classroom ?classroom))
        (modify ?pl (pass $?front ?select)(weight $?pr-w 2))
)

(defrule schedule-1-favorite-time
        ?p <- (phase schedule-3)
        ?f1 <- (select ?select)
        ?pl <- (pass-lesson (pass $?front)(weight $?pr-w))
 
        ;要排的課
        ?f2 <- (lesson (ID ?select) (state 0) (class ?class) (teacher ?teacher) (type ?type) (time) (room))
        ;對應的教授
        ?f3 <- (teacher (ID ?teacher) (weight ?weight))

        ; 教授的喜好時間
        ; t2 = t1 + 1
        (alltime $? ?t1 ?t2 ?t3 $?)

        ; 檢查時間有沒有在同一天
        (test (and 
            (= (+ ?t1 1) ?t2)
            (= (+ ?t2 1) ?t3)
            (= (div ?t1 100) (div ?t2 100) (div ?t3 100)))
        )

        ; 教授的拒絕時間，喜好時間有沒有在拒絕時間內
        (refuse-time (teacher ?teacher) (time $?r-t))
        (test (not (or (member$ ?t1 $?r-t) (member$ ?t2 $?r-t) (member$ ?t3 $?r-t))))

        ; 所選的時間有k門都在教授的喜好時間內
        (favorite-time (teacher ?teacher) (time $?f-t))
        (test 
            (= 
                (+ 
                    (if (member$ ?t1 $?f-t) then 1 else 0)
                    (if (member$ ?t2 $?f-t) then 1 else 0)
                    (if (member$ ?t3 $?f-t) then 1 else 0)
                )
            1)
        )

        ; 選擇與課程相符的教室狀態
        (classroom (ID ?classroom) (type ?type))
        ; 不可以是排過的課、排過的教授、排過的地點
        (not (lesson (teacher ?teacher) (time $? ?t1|?t2|?t3 $?)))
        (not (lesson (class ?class) (time $? ?t1|?t2|?t3 $?)))
        (not (lesson (time $? ?t1|?t2|?t3 $?) (room ?classroom ?classroom ?classroom)))
        =>
        (retract ?p ?f1)
        (assert (phase get-lesson))
        ;喜好有1門, +1
        (modify ?f3 (weight (+ ?weight 1)))
        ; 這個時段教室有人了
        (modify ?f2 (state 1) (time ?t1 ?t2 ?t3) (room ?classroom ?classroom ?classroom))
        (modify ?pl (pass $?front ?select)(weight $?pr-w 1))
)

(defrule schedule-0-favorite-time
        (declare (salience -4))
        ?p <- (phase schedule-3)
        ?f1 <- (select ?select)
        ?pl <- (pass-lesson (pass $?front)(weight $?pr-w))

        ;要排的課
        ?f2 <- (lesson (ID ?select) (state 0) (class ?class) (teacher ?teacher) (type ?type) (time) (room))
        ;對應的教授
        ?f3 <- (teacher (ID ?teacher) (weight ?weight))

        ; 教授的喜好時間
        ; t2 = t1 + 1
        
        (alltime $? ?t1 ?t2 ?t3 $?)

        ; 檢查時間有沒有在同一天
        (test (and 
            (= (+ ?t1 1) ?t2)
            (= (+ ?t2 1) ?t3)
            (= (div ?t1 100) (div ?t2 100) (div ?t3 100)))
        )

        ; 教授的拒絕時間，喜好時間有沒有在拒絕時間內
        (refuse-time (teacher ?teacher) (time $?r-t))
        (test (not (or (member$ ?t1 $?r-t) (member$ ?t2 $?r-t) (member$ ?t3 $?r-t))))


        ; 選擇與課程相符的教室狀態
        (classroom (ID ?classroom) (type ?type))
        ; 不可以是排過的課、排過的教授、排過的地點
        (not (lesson (teacher ?teacher) (time $? ?t1|?t2|?t3 $?)))
        (not (lesson (class ?class) (time $? ?t1|?t2|?t3 $?)))
        (not (lesson (time $? ?t1|?t2|?t3 $?) (room ?classroom ?classroom ?classroom)))
        =>
        (retract ?p ?f1)
        (assert (phase get-lesson))
        ;喜好有1門, +1
        ;(modify ?f3 (weight (+ ?weight 1)))
        ; 這個時段教室有人了
        (modify ?f2 (state 1) (time ?t1 ?t2 ?t3) (room ?classroom ?classroom ?classroom))
        (modify ?pl (pass $?front ?select)(weight $?pr-w 0))
)

(defrule backtrack
        (declare (salience -10))
        ?p <- (phase schedule-3)
        
        ; 當前要排的課沒有排成功
        ?s <- (select ?select)
        ; 挑出最末端節點
        ?pa <- (pass-lesson (pass $?f ?rear)(weight $?w))
        ; 對末端節點來說，這個選擇不是好選擇，是陷阱
        ?n <- (node (lesson ?rear)(trap $?trap))
        =>
        (printout t "FAIL: " ?select " and BACK TO " ?rear crlf)
        (retract ?s ?n ?p)
        (assert (select ?rear))
        ;(assert (phase get-lesson))
        (assert (node (lesson ?rear)(trap $?trap ?select)))
        ;(assert (pass-lesson (pass $?f)(weight $?w)))
)

(defrule halt
        (declare (salience -100))
        ?p <- (phase schedule-3)
        ?s <- (select ?select)
        ?pas <- (pass-lesson (pass $?f))
        (member$ ?select $?f)
        =>
        (printout t "HALT: " ?select " " $?f crlf)
        (halt)
)

; (defrule swap-1-f-class ;把排好的提早一節
;         (declare (salience -6))
;         ?p <- (phase schedule-3)
;         ?f1 <- (select ?select)
 
;         ;要排的課
;         ?f2 <- (lesson (ID ?select) (state 0) (class ?class) (teacher ?teacher) (type ?type) (time) (room))
;         ?s2 <- (lesson (ID ?swap)(state 1)(class ?s_class)(teacher ?s_teacher)(type ?s_type)(time ?s_t1 ?s_t2 ?s_t3)(room ?s_classroom ?s_classroom ?s_classroom))

;         ;對應的教授
;         ?f3 <- (teacher (ID ?teacher) (weight ?weight))
;         ?s3 <- (teacher (ID ?s_teacher)(weight ?s_weight))

;         ; 把排好的提早一節
;         ; 
        
;         (alltime $? ?t1&:(= (- ?s_t1 1) ?t1) ?s_t1 ?s_t2 ?s_t3 ?t2&:(= (+ ?s_t3 1) ?t2) ?t3&:(= (+ ?s_t3 2) ?t3) $?)
;         ; 教授的拒絕時間
;         (refuse-time (teacher ?teacher) (time $?r-t))
;         (refuse-time (teacher ?s_teacher)(time $?sr-t))

;         ; 時間有沒有在拒絕時間內 以及超過當日上限
;         ; 提早的一節有沒有在拒絕時間內
;         (test (not (member$ ?t1 $?sr-t)))

;         ; 移動一節有沒有在拒絕時間內
;         (test (not (or (member$ ?s_t3 $?r-t) (member$ ?t2 $?r-t) (member$ ?t3 $?r-t))))

;         (test (not (or  (>= (mod ?t1 100) 11)
;                         (>= (mod ?t2 100) 11)
;                         (>= (mod ?t3 100) 11)
;                 ))
;         )

;         ; 選擇與課程相符的教室狀態
;         (classroom (ID ?classroom) (type ?type))
;         ; 不可以是排過的課、排過的教授、排過的地點
;         (not (lesson (teacher ?teacher) (time $? ?s_t3|?t2|?t3 $?)))
;         (not (lesson (teacher ?s_teacher) (time $? ?t1|?s_t1|?s_t2 $?)))

;         (not (lesson (class ?class) (time $? ?s_t3|?t2|?t3 $?)))
;         (not (lesson (class ?s_class) (time $? ?t1|?s_t1|?s_t2 $?)))

;         (not (lesson (time $? ?s_t3|?t2|?t3 $?) (room ?classroom ?classroom ?classroom)))
;         (not (lesson (time $? ?t1|?s_t1|?s_t2 $?)(room ?s_classroom ?s_classroom ?s_classroom)))
;         =>
;         (retract ?p ?f1)
;         (assert (phase get-lesson))
;         ;喜好有1門, +1
;         ;(modify ?f3 (weight (+ ?weight 1)))
;         ; 這個時段教室有人了
;         (modify ?s2 (state 1) (time ?t1 ?s_t1 ?s_t2) (room ?s_classroom ?s_classroom ?s_classroom))
;         (modify ?f2 (state 1) (time ?s_t3 ?t2 ?t3) (room ?classroom ?classroom ?classroom))
; )

; (defrule swap-1-r-class ;把排好的延後一節
;         (declare (salience -6))
;         ?p <- (phase schedule-3)
;         ?f1 <- (select ?select)
 
;         ;要排的課
;         ?f2 <- (lesson (ID ?select) (state 0) (class ?class) (teacher ?teacher) (type ?type) (time) (room))
;         ?s2 <- (lesson (ID ?swap)(state 1)(class ?s_class)(teacher ?s_teacher)(type ?s_type)(time ?s_t1 ?s_t2 ?s_t3)(room ?s_classroom ?s_classroom ?s_classroom))

;         ;對應的教授
;         ?f3 <- (teacher (ID ?teacher) (weight ?weight))
;         ?s3 <- (teacher (ID ?s_teacher)(weight ?s_weight))

;         ; 把排好的提早一節
;         ; 
        
;         (alltime $? ?s_t1 ?s_t2 ?s_t3 $?)
;         ; 教授的拒絕時間
;         (refuse-time (teacher ?teacher) (time $?r-t))
;         (refuse-time (teacher ?s_teacher)(time $?sr-t))

;         ; 時間有沒有在拒絕時間內 以及超過當日上限
;         ; 提早的一節有沒有在拒絕時間內
;         (test (not (member$ ?t1 $?sr-t)))

;         ; 移動一節有沒有在拒絕時間內
;         (test (not (or (member$ ?s_t3 $?r-t) (member$ ?t2 $?r-t) (member$ ?t3 $?r-t))))

;         (test (not (or  (>= (mod ?t1 100) 11)
;                         (>= (mod ?t2 100) 11)
;                         (>= (mod ?t3 100) 11)
;                 ))
;         )

;         ; 選擇與課程相符的教室狀態
;         (classroom (ID ?classroom) (type ?type))
;         ; 不可以是排過的課、排過的教授、排過的地點
;         (not (lesson (teacher ?teacher) (time $? ?s_t3|?t2|?t3 $?)))
;         (not (lesson (teacher ?s_teacher) (time $? ?t1|?s_t1|?s_t2 $?)))

;         (not (lesson (class ?class) (time $? ?s_t3|?t2|?t3 $?)))
;         (not (lesson (class ?s_class) (time $? ?t1|?s_t1|?s_t2 $?)))

;         (not (lesson (time $? ?s_t3|?t2|?t3 $?) (room ?classroom ?classroom ?classroom)))
;         (not (lesson (time $? ?t1|?s_t1|?s_t2 $?)(room ?s_classroom ?s_classroom ?s_classroom)))
;         =>
;         (retract ?p ?f1)
;         (assert (phase get-lesson))
;         ;喜好有1門, +1
;         ;(modify ?f3 (weight (+ ?weight 1)))
;         ; 這個時段教室有人了
;         (modify ?s2 (state 1) (time ?t1 ?s_t1 ?s_t2) (room ?s_classroom ?s_classroom ?s_classroom))
;         (modify ?f2 (state 1) (time ?s_t3 ?t2 ?t3) (room ?classroom ?classroom ?classroom))
; )


; (defrule swap-2-f-class ;把排好的提早兩節
;         (declare (salience -7))
;         ?p <- (phase schedule-3)
;         ?f1 <- (select ?select)
 
;         ;要排的課
;         ?f2 <- (lesson (ID ?select) (state 0) (class ?class) (teacher ?teacher) (type ?type) (time) (room))
;         ?s2 <- (lesson (ID ?swap)(state 1)(class ?s_class)(teacher ?s_teacher)(type ?s_type)(time ?s_t1 ?s_t2 ?s_t3)(room ?s_classroom ?s_classroom ?s_classroom))

;         ;對應的教授
;         ?f3 <- (teacher (ID ?teacher) (weight ?weight))
;         ?s3 <- (teacher (ID ?s_teacher)(weight ?s_weight))

;         ; 把排好的提早一節
;         ; 
        
;         (alltime $? ?t1&:(= (- ?s_t1 2) ?t1)  ?t2&:(- (+ ?s_t1 1) ?t2) ?s_t1 ?s_t2 ?s_t3 ?t3&:(= (+ ?s_t3 2) ?t3) $?)
;         ; 教授的拒絕時間
;         (refuse-time (teacher ?teacher) (time $?r-t))
;         (refuse-time (teacher ?s_teacher)(time $?sr-t))

;         ; 時間有沒有在拒絕時間內 以及超過當日上限
;         ; 提早的一節有沒有在拒絕時間內
;         (test (not (or (member$ ?t1 $?sr-t) (member$ ?t2 $?sr-t))))

;         ; 移動一節有沒有在拒絕時間內
;         (test (not (or (member$ ?s_t2 $?r-t) (member$ ?s_t3 $?r-t) (member$ ?t3 $?r-t))))

;         (test (not (or  (>= (mod ?t1 100) 11)
;                         (>= (mod ?t2 100) 11)
;                         (>= (mod ?t3 100) 11)
;                 ))
;         )

;         ; 選擇與課程相符的教室狀態
;         (classroom (ID ?classroom) (type ?type))
;         ; 不可以是排過的課、排過的教授、排過的地點
;         (not (lesson (teacher ?teacher) (time $? ?s_t2|?s_t3|?t3 $?)))
;         (not (lesson (teacher ?s_teacher) (time $? ?t1|?t2|?s_t1 $?)))

;         (not (lesson (class ?class) (time $? ?s_t2|?s_t3|?t3 $?)))
;         (not (lesson (class ?s_class) (time $? ?t1|?t2|?s_t1 $?)))

;         (not (lesson (time $? ?s_t2|?s_t3|?t3 $?) (room ?classroom ?classroom ?classroom)))
;         (not (lesson (time $? ?t1|?t2|?s_t1 $?)(room ?s_classroom ?s_classroom ?s_classroom)))
;         =>
;         (retract ?p ?f1)
;         (assert (phase get-lesson))
;         ;喜好有1門, +1
;         ;(modify ?f3 (weight (+ ?weight 1)))
;         ; 這個時段教室有人了
;         (modify ?f2 (state 1) (time ?s_t2 ?s_t3 ?t3) (room ?classroom ?classroom ?classroom))
;         (modify ?s2 (state 1) (time ?t1 ?t2 ?s_t1) (room ?s_classroom ?s_classroom ?s_classroom))
; )


