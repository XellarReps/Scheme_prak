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

(define (visit-doctor-v2 stop-name max-cnt-patients)
  (let loop ((patient-idx max-cnt-patients))
    (if (= patient-idx 0)
        `(time to go home) ;if true

        (let ((name (ask-patient-name))) ; if false
          (cond
            ((equal? name stop-name) `(time to go home))

            (else (printf "Hello, ~a!\n" name)
                  (print '(what seems to be the trouble?))
                  (doctor-driver-loop-v1 name)
                  (loop (sub1 patient-idx))
            )
          )
        )
    )
  )
)

(define (ask-patient-name)
 (begin
  (println '(next!))
  (println '(who are you?))
  (print '**)
  (car (read))
 ) 
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
          (newline)
         )
            
         (else (print (reply-v3 reply-structure user-response history-msg))
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

(define (reply-v2 user-response history-msg)
      (case (random (if (vector-empty? history-msg) 1 0) (if (contains-keyword? user-response) 4 3))
          ((0) (history-answer history-msg)) 
          ((1) (hedge-answer))  
          ((2) (qualifier-answer user-response))
          ((3) (keywords-answer user-response))
      )
)

(define (is-keyword? word)
  (memq word keyword-collection)
)

(define (contains-keyword? user-response)
  (ormap is-keyword? user-response)
)

;==========================================================================================================================
; Универсальный reply
; Имеется структура которая содержит следующие тройки (вес чеккер конструктор)
(define reply-structure
  (list
   (list
    1                                                  ; вес
    (lambda(x y) #t)                                   ; чеккер
    (lambda(x y)(hedge-answer))                        ; конструктор
   )
   (list
    2                                                  ; вес
    (lambda(x y) #t)                                   ; чеккер
    (lambda(x y)(qualifier-answer x))                  ; конструктор
   )
   (list
    3                                                  ; вес
    (lambda(x y) (if (vector-empty? y) #f #t))         ; чеккер
    (lambda(x y)(history-answer y))                    ; конструктор
   )
   (list
    100                                                ; вес
    (lambda(x y) (if (contains-keyword? x) #t #f))     ; чеккер
    (lambda(x y)(keywords-answer x))                   ; конструктор
   )
  )
)

; Возвращает те реплаи из структуры которые проходят чекеры
(define (get-available-reply-lst reply-structure user-response history-msg)
  (filter
   (lambda (x)
     (
      (cadr x) user-response history-msg
     )
   )
   reply-structure
  )
)

; Подсчет суммы весов выбранных реплаев из структуры
(define (get-all-weight lst)
  (foldl
   (lambda(x y) (+ (car x) y))
   0
   lst
  )
)

(define (reply-v3 reply-structure user-response history-msg)
  (let ((available-reply-lst (get-available-reply-lst reply-structure user-response history-msg)))
    (let ((sum-weights (get-all-weight available-reply-lst)))
      (let loop
        (
         (now-reply (car available-reply-lst))
         (replys (cdr available-reply-lst))
         (random-weight (random sum-weights))
         (curr-weight (caar available-reply-lst))
        )

        (if (<= random-weight curr-weight)
            ((caddr now-reply) user-response history-msg) ; if true

            (loop                                         ; if false
             (car replys)
             (cdr replys)
             (- random-weight curr-weight)
             (caar replys)
            )
        )
      )
    )
  )
)
;==========================================================================================================================

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

(define (create-exchanges lst-prs lst-words lst-param)
  (let ((len-lst-prs (length lst-prs)) (len-lst-words (length lst-words)) (len-lst-param (length lst-param)))
    (if (and (= len-lst-prs len-lst-words) (= len-lst-prs len-lst-param))
        
        (let create-exchanges-rec ((lst1 lst-prs) (lst2 lst-words) (lst3 lst-param) (res `())) ; if true
          (if (null? lst1)
              res

              (create-exchanges-rec (cdr lst1) (cdr lst2) (cdr lst3) (append res (create-exchange (car lst1) (car lst2) (car lst3))))
          )
        )

        `(lengths lst-prs lst-words lst-param must be equal) ; if false
    )
  )
)

; замена лица во фразе
(define (change-person phrase)
  (many-replace-v3
   (create-exchanges `((am) (i me we us) (mine ours) (my our) (myself) (ourselves) (shall)) `(are you yours your yourself yourselves will) `(1 1 1 1 1 1 0))
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

(define keywords-structure '#(
  ( ; начало данных 1й группы
    (depressed suicide exams university) ; список ключевых слов 1й группы
    ( ; список шаблонов для составления ответных реплик 1й группы 
      (when you feel depressed, go out for ice cream)
      (depression is a disease that can be treated)
      
      (how is your study going?)
      (are control activities at university emotionally happening for you?)
    )
  ) ; завершение данных 1й группы
  ( ; начало данных 2й группы ...
    (mother father parents brother sister uncle aunt grandma grandpa)
    (
      (tell me more about your *, i want to know all about your *)
      (why do you feel that way about your *?)

      (and how is your * feeling?)
      (do you communicate with your *?)
    )
  )
  (
    (university scheme lections seminars)
    (
      (your education is important)
      (how much time do you spend on your studies?)

      (do you have problems with * ?)
      (do you like * ?)
    )
  )
  (
    (head belly stomach leg ear heart)
    (
      (do you have a * ache?)
      (when was the last time you had a * ache?)
      (why do you think your * hurts?)
    )
  )
  (
    (antipyretic painkiller sedative)
    (
      (i can give you a * pill)
      (you need to take an * pill)
      (did you take an * pill yesterday)
    )
  )
))

;====================================================================================================
; две основные функции для работы со структурой keywords-structure
; первая функция достает все ключевые слова из этой структуры
; вторая фукнция возвращает ответы доктора для ключевого слова которое передали в качестве аргумента
(define keyword-collection
  (foldl
   (lambda (x y)
     (foldl
      (lambda(z w)
        (if (memq z w)
            w
            (cons z w)
        )
      )
      y
      (car x)
     )
   )
   '()
   (vector->list keywords-structure)
  )
)

(define (keyword-resp-get keyword)
  (foldl
   (lambda (x y)
     (append (if (memq keyword (car x)) (cadr x) '() ) y)
   )
   `()
   (vector->list keywords-structure)
  )
)
;====================================================================================================

(define (pick-random-lst lst)
  (list-ref lst (random (length lst)))
)

(define (keyword-adder elem lst)
  (if (memq elem keyword-collection)
      (cons elem lst) ; if true

      lst ; if else
  )
)

(define (keywords-answer user-response)
  (let ((keywords (foldl keyword-adder `() user-response)))
    (let ((random-keyword (pick-random-lst keywords)))
      (many-replace-v3
       (list(list `* random-keyword))
       (pick-random-lst (keyword-resp-get random-keyword))
      )
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
