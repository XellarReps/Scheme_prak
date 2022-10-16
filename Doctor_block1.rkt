; заготовка "Доктора". Сентябрь 2022
#lang scheme/base
(require racket/vector)

; В учебных целях используется базовая версия Scheme

; основная функция, запускающая "Доктора"
; параметр name -- имя пациента
(define (visit-doctor name)
  (printf "Hello, ~a!\n" name)
  (print '(what seems to be the trouble?))
  (doctor-driver-loop-v1 name)
)

; цикл диалога Доктора с пациентом
; параметр name -- имя пациента
(define (doctor-driver-loop name)
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
      (cond 
	    ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
             (printf "Goodbye, ~a!\n" name)
             (print '(see you next week)))
            (else (print (reply user-response)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                  (doctor-driver-loop name)
             )
       )
      )
)

(define (doctor-driver-loop-v1 name)
  (let loop ((history-msg #()))
    (newline)
    (print '**)
    (let ((user-response (read)))
      (cond 
	 ((equal? user-response '(goodbye))
          (printf "Goodbye, ~a!\n" name)
          (print '(see you next week))
         )
            
         (else (print (reply-v1 user-response history-msg))
            (loop (vector-append (vector user-response) history-msg))
         )
      )
    )
  )
)

; генерация ответной реплики по user-response -- реплике от пользователя 
(define (reply user-response)
      (case (random 0 2) ; с равной вероятностью выбирается один из двух способов построения ответа
          ((0) (hedge-answer))  ; 1й способ
          ((1) (qualifier-answer user-response)) ; 2й способ
      )
)

(define (reply-v1 user-response history-msg)
      (case (random 0 (if (vector-empty? history-msg) 2 3)) 
          ((0) (hedge-answer))  
          ((1) (qualifier-answer user-response)) 
          ((2) (history-answer history-msg)) 
      )
)

; 1й способ генерации ответной реплики -- случайный выбор одной из заготовленных фраз, не связанных с репликой пользователя
(define (hedge-answer)
       (pick-random-vector '#((please go on)
                              (many people have the same sorts of feelings)
                              (many of my patients have told me the same thing)
                              (please continue)
                              
                              (describe the problem in more detail)
                              (close the door please)
                              (it is ok))
         )
)

; случайный выбор одного из элементов непустого вектора
(define (pick-random-vector vctr)
  (vector-ref vctr (random 0 (vector-length vctr)))
)

; 2й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату случайно выбранного нового начала
(define (qualifier-answer user-response)
        (append (pick-random-vector '#((you seem to think that)
                                       (you feel that)
                                       (why do you believe that)
                                       (why do you say that)

                                       
                                       (what makes you think that)
                                       (what do you think is the reason that)
                                       (how often do  you feel that)
                                       )
                )
                (change-person user-response)
        )
 )

(define (history-answer history-msg)
  (append `(earlier you said that) (change-person (pick-random-vector history-msg)))
)

; генерация пар замен 
(define (create-exchange lst word mutually)
  (let rec ((lst lst))
    (if (null? lst)
        `()

        (if (= mutually 0) 
         (append
          (rec (cdr lst))
          (list(list (car lst) word))
         )

         (append
          (rec (cdr lst))
          (list(list (car lst) word))
          (list(list word (car lst)))
         )
        )
    )
  )
)

; замена лица во фразе
(define (change-person phrase)
  (many-replace-v3
   (append
    (create-exchange `(am) `are 1)
    (create-exchange `(i me we us) `you 1)
    (create-exchange `(mine ours) `yours 1)
    (create-exchange `(my our) `your 1)
    (create-exchange `(myself) `yourself 1)
    (create-exchange `(ourselves) `yourselves 1)
    (create-exchange `(shall) `will 0)
   )
   phrase
  )
)

; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs
(define (many-replace replacement-pairs lst)
        (cond ((null? lst) lst)
              (else (let ((pat-rep (assoc (car lst) replacement-pairs))) ; Доктор ищет первый элемент списка в ассоциативном списке замен
                      (cons (if pat-rep (cadr pat-rep) ; если поиск был удачен, то в начало ответа Доктор пишет замену
                                (car lst) ; иначе в начале ответа помещается начало списка без изменений
                            )
                            (many-replace replacement-pairs (cdr lst)) ; рекурсивно производятся замены в хвосте списка
                        )
                     )
               )
         )
)

(define (many-replace-v1 replacement-pairs lst)
  (cond
    ((null? lst) lst)
    (else (cons
           (select-exchange replacement-pairs (car lst))
           (many-replace-v1 replacement-pairs (cdr lst))
          )
    )
  )
)

(define (many-replace-v2 replacement-pairs lst)
  (let loop ((lst lst) (result `()))
    (if (null? lst)
        (reverse result)

        (loop (cdr lst)
              (cons
                (select-exchange replacement-pairs (car lst))
                result
              )
        )
    )
  )
)

(define (many-replace-v3 replacement-pairs lst)
  (map
   (lambda(x) (select-exchange replacement-pairs x))
   lst
  )
)

(define (select-exchange replacement-pairs first-elem)
  (let ((pat-rep (assoc first-elem replacement-pairs)))
    (if (not pat-rep)
        first-elem

        (cadr pat-rep)
    )
  )
)

; в Racket нет vector-foldl, реализуем для случая с одним вектором (vect-foldl f init vctr)
; у f три параметра i -- индекс текущего элемента, result -- текущий результат свёртки, elem -- текущий элемент вектора
(define (vector-foldl f init vctr)
 (let ((length (vector-length vctr)))
  (let loop ((i 0) (result init))
   (if (= i length) result
    (loop (add1 i) (f i result (vector-ref vctr i)))))))
	
; аналогично от конца вектора к началу
(define (vector-foldr f init vctr)
 (let ((length (vector-length vctr)))
  (let loop ((i (sub1 length)) (result init))
   (if (= i -1) result
    (loop (sub1 i) (f i result (vector-ref vctr i)))))))
