#lang racket/gui

(require racket/format)
(require net/http-client)

; Make a frame
(define frame (new frame%
                   [label "Лизингов калкулатор"]
                   [min-width 300]
                   [min-height 300]
                   [stretchable-width #f]	 
   	 	[stretchable-height #f]
                   ))


;;---------------------------------- MENU -----------------------------------;;
(define menu-bar (new menu-bar%
                     (parent frame)))   


(define file-menu (new menu% (label "&File")
                             (parent menu-bar)))             

(new menu-item%              [label "&Change calculator"]
                             [parent file-menu]
                             [callback (lambda (button event)
                                         (send frame2 show #t)
                                         (send frame show #f))]
                             )


(define m-edit (new menu% [label "Edit"] [parent menu-bar]))
(append-editor-operation-menu-items m-edit #t)


(new menu% (label "&Help") (parent menu-bar))

;;--------------------------------------------------------------------------;;


; Make a new line with text message in the frame
(define (new-line)
  (new message% [parent frame]
                          [label ""]
                          )
)


(new-line)


;.........................................
(define type-lst (list "Лек автомобил"
                    "Товарен автомобил"
                    "Земеделска техника"
                    "Строителна техника"
                    "Машини и оборудване"))

(define type
  (new choice%	 
   	 	[label "Вид на актива     "]	 
   	 	[choices type-lst]	 
                [parent frame]
                )
)
 

;.........................................
(define new-used-lst (list "Нов"
                    "Употребяван"))

(define new-used
  (new choice%	 
   	 	[label "Нов или употребяван         "]	 
   	 	[choices new-used-lst]	 
   	 	[parent frame]
                )
)
;.........................................
(define customer-lst (list
                      "Физическо лице"
                      "Фирма"))

(define customer
  (new choice%	 
   	 	[label "Вие сте                             "]	 
   	 	[choices customer-lst]	 
   	 	[parent frame]
                )
)
;.........................................
(define buy-price
  (new text-field%	 
   	 	[label "Покупна цена"]	 
   	 	[parent frame]
            [horiz-margin 21])
)


;.........................................
(define advance-payment-lst (list "Не"
                                  "Да"))

(define advance-payment
  (new choice%
     [label "Авансово плащане                                "]
     [choices advance-payment-lst]
     [parent frame]
                )
)
;.........................................
(define first-payment-lst (list
                    "10%"
                    "15%"
                    "20%"
                    "25%"
                    "30%"
                    "35%"
                    "40%"
                    "45%"
                    "50%"
                    ))
(define first-payment
  (new choice%
     [label "Първоначална вноска                        "]
     [choices first-payment-lst]
     [parent frame]
                )
)
;.........................................

(define lease-term-lst (list
                    "1 година"
                    "2 години"
                    "3 години"
                    "4 години"
                    "5 години"
                    ))

(define lease-term
  (new choice%
     [label "Срок на лизинга                          "]
     [choices lease-term-lst]
     [parent frame]
                )
)
;.........................................
(define currency-lst (list
                    "BGN"
                    "EUR"
                    ))
(define currency
  (new choice%
     [label "Валута                                                    "]
     [choices currency-lst]
     [parent frame]
                )
)
;.........................................



;;======================    Премести  ,Изчисли, Резултат =========================;;

(new-line)

(new button% [parent frame]
             [label "Пресметни"]
             [style '(border)]
             [stretchable-width #t]
             [callback (lambda (button event)
                         (send frame-result show #t)                       
                         (send text-result insert (format (connection-server-1) ))
                         )]
             )





(new-line)


(new button% [parent frame]
             [label "Изчисти"]
             [callback (lambda (button event)
                         (send type set-selection 0)
                         (send new-used set-selection 0)
                         (send customer set-selection 0)
                         (send buy-price set-value "")
                         (send advance-payment set-selection 0)
                         (send first-payment set-selection 0)
                         (send lease-term set-selection 0)
                         (send currency set-selection 0)
                         
                                         )]
             )




; Make a frame to Result
(define frame-result (new frame%
                          [label "Result"]
                          ))



(define editor-canvas (new editor-canvas%
                           (parent frame-result)
                           [vert-margin 15]
                           [horiz-margin 15]
                           [min-width 250]
                           [min-height 280]
                           
                           [style '(no-focus)]
                           ))



(define text-result (new text%))


(send editor-canvas set-editor text-result)


;;=========================================================================;;




;; ###########-=-=-=-=-= The submitted string 1 -=-=-=-=-=########## ;;

(define (formatted-result)
  (string-append "Calc=1&"
                 "Type="           (convert-type)                            "&"
                 "NewUsed="        (convert-new-used)                        "&"
                 "Customer="       (convert-customer)                        "&"
                 "BuyPrice="       (send buy-price get-value)                "&"
                 "AdvancePayment=" (convert-advance-payment)                 "&"
                 "FirstPayment="   (convert-first-payment)                   "&"
                 "LeaseTerm="      (convert-lease-term)                      "&"
                 "Currency="       (send currency get-string-selection)  ))



;;   (convert-first-payment)    ;; konvertirah procentite 10% -> 10   v chislo

;;  (send first-payment get-string-selection)  ; tova beshe



;; ###########-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=########## ;;




;----------------------- Functions to convert from bg to english  ---------------------- ;;
(define (convert-type)
  (case (send type get-selection)
    [(0) "Car"]
    [(1) "Truck"]
    [(2) "Agricultural"]
    [(3) "Construction"]
    [(4) "Machinery"]
    ))


(define (convert-new-used)
  (case (send new-used get-selection)
    [(0) "New"]
    [(1) "Used"]
    ))


(define (convert-customer)
  (case (send customer get-selection)
    [(0) "Physical"]
    [(1) "Corporate"]
    ))


;;--->   (send buy-price get-value)

(define (convert-advance-payment)
  (case (send advance-payment get-selection)
    [(0) "No"]
    [(1) "Yes"]
    ))

;;-->   (send first-payment get-string-selection)

(define (convert-first-payment)
  (case (send first-payment get-selection)
    [(0) "10"]
    [(1) "15"]
    [(2) "20"]
    [(3) "25"]
    [(4) "30"]
    [(5) "35"]
    [(6) "40"]
    [(7) "45"]
    [(8) "50"]
    ))



(define (convert-lease-term)
  (case (send lease-term get-selection)
    [(0) "1"]
    [(1) "2"]
    [(2) "3"]
    [(3) "4"]
    [(4) "5"]
    ))


;;--->   (send currency get-string-selection)

;--------------------------------------------------------------------------------------- ;;




; Show the frame
(send frame show #t)





;;=================================== Server Connection 1 ===================================

(define (connection-server-1)
  (define-values (status headers in)
    (http-sendrecv "127.0.0.1"
                 "/"
                 #:port 8888
                 #:ssl? #f
                 #:version "1.1"
                 #:method "POST"
                 #:headers (list "Content-Type: application/x-www-form-urlencoded")
                 #:data (formatted-result)
                 )

    )
  

  (define return_result (port->string in))
  return_result
 
)








;;=========================================================================================
















;;======================================================================================;;
;;================================ 2-nd CALCULATOR =====================================;;
;;======================================================================================;;


; Make a frame
(define frame2 (new frame%
                   [label "Данъчен калкулатор"]
                   [min-width 360]
                   [min-height 300]
                   [stretchable-width #f]	 
   	 	[stretchable-height #f]))



;;---------------------------------  MENU   --------------------------------------;;
(define menu-bar2 (new menu-bar%
                     (parent frame2)))   


(define file-menu2 (new menu% (label "&File")
                             (parent menu-bar2)))             

(new menu-item%              [label "&Change calculator"]
                             [parent file-menu2]
                             [callback (lambda (button event)
                                         (send frame show #t)
                                         (send frame2 show #f))]
                             )


(define m-edit2 (new menu% [label "Edit"] [parent menu-bar2]))
(append-editor-operation-menu-items m-edit2 #t)


(new menu% (label "&Help") (parent menu-bar2))

;;------------------------------------------------------------------------------;;


; Make a static text message in the frame
(define (new-line2)
  (new message% [parent frame2]
                          [label ""])
)

(new-line2)



;.........................................
(define power-car
  (new text-field%	 
   	 	[label "Мощност на автомобила              "]	 
   	 	[parent frame2]
            [horiz-margin 21])
)


;.........................................
(define municipality-lst (list
                    "София"
                    "Пловдив"
                    "Варна"
                    "Бургас"
                    "Русе"
                    "Стара Загора"
                    "Плевен"
                    "Сливен"
                    "Добрич"
                    "Шумен"
                    "Перник"
                    "Хасково"
                    "Ямбол"
                    "Пазарджик"
                    "Благоевград"
                    "Велико Търново"
                    "Враца"
                    "Габрово"
                    "Асеновград"
                    "Видин"))

(define municipality
  (new choice%	 
   	 	[label "Община на собственика                  "]	 
   	 	[choices municipality-lst]	 
                [parent frame2]
                )
)
;.........................................
(define age-lst (list
                    "до 5г. вкл."
                    "над 5г. до 14г."
                    "над 14г."
                    ))

(define age
  (new choice%	 
   	 	[label "Възраст на автомобила                          "]	 
   	 	[choices age-lst]	 
                [parent frame2]
                )
)

;.........................................

(define catalyst-lst (list
                    "Не"
                    "Да"
                    ))

(define catalyst
  (new choice%	 
   	 	[label "Катализатор                                                                  "]	 
   	 	[choices catalyst-lst]	 
                [parent frame2]
                )
)
;.........................................




;;======================    Премести  ,Изчисли, Резултат =========================;;


(new-line2)
(new-line2)

(new button% [parent frame2]
             [label "Пресметни"]
             [style '(border)]
             [stretchable-width #t]
             [callback (lambda (button event)
                         (send text-field-result-2 set-value (connection-server-2))
                         )])

(new-line2)

(new button% [parent frame2]
             [label "Изчисти"]
             [callback (lambda (button event)
                         (send power-car set-value "")
                         (send municipality set-selection 0)
                         (send age set-selection 0)
                         (send catalyst set-selection 0)
                         (send text-field-result-2 set-value "")
                                         )]
             )


(new-line2)
(new-line2)



(define text-field-result-2
  (new text-field%
                        (label "Размер на данъка: ")
                        (parent frame2)
                        [init-value ""]
                        [horiz-margin 19]
                        [enabled #t]
                        ))

text-field-result-2



;;=========================================================================;;





;; ###########-=-=-=-=-= The submitted string -=-=-=-=-=########## ;;


(define (formatted-result2)
  (string-append "Calc=2&"
                 "Hp="           (send power-car get-value) "&"
                 "Area="         (convert-municipality)     "&"
                 "Age="          (convert-age)              "&"
                 "Catalyst="     (convert-catalyst)))

;; ###########-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=########## ;;




;----------------------- Functions to convert from bg to english  ---------------------- ;;


;;--->   (send power-car get-value)


(define (convert-municipality)
  (case (send municipality get-selection)
    [(0 6 7 9 10 12)       "0.92"]
    [(1 5 18)              "1.04"]
    [(2)                   "2.07"]
    [(3 4 13 14 16 17 19)  "0.70"]
    [(8 11 15)             "1.30"]
    ))


(define (convert-age)
  (case (send age get-selection)
    [(0) "up-to-5-years"]
    [(1) "over-5-to-14-years"]
    [(2) "over-14-years"]
    ))


(define (convert-catalyst)
  (case (send catalyst get-selection)
    [(0) "No"]
    [(1) "Yes"]
    ))

;--------------------------------------------------------------------------------------- ;;



; Show the frame
(send frame2 show #f)






;;=================================== Server Connection  2 ===================================


;;----- Tax Calculator (2)


(define (connection-server-2)
  (define-values (status headers in)
    (http-sendrecv "127.0.0.1"
                 "/"
                 #:port 8888
                 #:ssl? #f
                 #:version "1.1"
                 #:method "POST"
                 #:headers (list "Content-Type: application/x-www-form-urlencoded")
                 #:data (formatted-result2)
                 )

    )
  

  (define return_result (port->string in))
  return_result
 
)


;;=========================================================================================
