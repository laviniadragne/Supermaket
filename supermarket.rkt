#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)


; Aveți libertatea să vă structurați programul cum doriți (dar cu restricțiile
; de mai jos), astfel încât funcția serve să funcționeze conform specificației.
; 
; Restricții (impuse de checker):
; - trebuie să existe în continuare funcția (empty-counter index)
; - cozile de la case trebuie implementate folosind noul TDA queue

(define-struct counter (index tt et open queue) #:transparent)

(define (empty-counter index)
  (make-counter index 0 0 1 empty-queue))


; Daca elementul are indexul cautat aplic f pe el
; daca nu, apelez recursiv
(define (helper_update f counters index)
  (cond
    ((null? counters) '())
    ((= (counter-index (car counters)) index) (cons (f (car counters)) (helper_update f (cdr counters) index)))
    (else
     (cons (car counters) (helper_update f (cdr counters) index)))))

; Daca lista e goala intoarce null
; altfel foloseste-te de helper
(define (update f counters index)
  (if (null? counters)
      '()
      (helper_update f counters index)))


; Daca elementul are indexul cautat si nu are coada goala
; aplic f pe el,  daca nu, apelez recursiv
(define (apply_update f counters index)
   (cond
    ((null? counters) '())
    ((queue-empty? (counter-queue (car counters))) (cons (car counters) (apply_update f (cdr counters) index)))
    ((= (counter-index (car counters)) index) (cons (f (car counters)) (apply_update f (cdr counters) index)))
    (else
     (cons (car counters) (apply_update f (cdr counters) index)))))


  
(define (tt+ minutes)
  (lambda (C)
    (match C
      [(counter index tt et open queue)
       ; Am acces la campurile index, tt, et si queue cu match si creez o noua
       ; structura cu ele
       (make-counter index (+ tt minutes) et open queue)])))

(define (et+ minutes)
  (lambda (C)
    (match C
      [(counter index tt et open queue)
       ; Am acces la campurile index, tt, et si queue cu match si creez o noua
       ; structura cu ele
       (make-counter index tt (+ et minutes) open queue)])))


; Functia actualizeaza si et-ul si tt-ul
(define (add_delay minutes)
  (lambda (C)
    (match C
      [(counter index tt et open queue)
       ; Am acces la campurile index, tt, et si queue cu match si creez o noua
       ; structura cu ele
       (struct-copy counter C [tt (+ tt minutes)] [et (+ et minutes)])])))




(define (add-to-counter name items)     ; testată de checker
  (λ (C)                                ; nu modificați felul în care funcția își primește argumentele
    (match C
      [(counter index tt et open queue)
       ; Am acces la campurile index, tt, et si queue cu match si creez o noua
       ; structura cu ele, adaugand la timpul de asteptare numarul de items
       ; si la coada perechea (name . n-items)
       ; daca casa este goala modific si et-ul
       ; perechea este construita folosind cons
       ; daca coada e goala adaug n-items si la et
       (if (queue-empty? queue)
           (make-counter index (+ tt items) (+ et items) open (enqueue (cons name items) queue))
           (make-counter index (+ tt items) et open (enqueue (cons name items) queue)))])))


(define (general-min field counters)
  (cond
    ; Daca sunt la ultimul element si este o casa inchisa, pun infinit in pereche, pentru ca
    ; la linia 105 sa se actualizeze tt-ul cu unul valid
    ((and (null? (cdr counters)) (= (counter-open (car counters)) 0)) (cons +inf.0 +inf.0))
    ; Daca am ajuns la ultimul element din lista de structuri, returnez perechea (index . field) 
    ((and (null? (cdr counters)) (= (counter-open (car counters)) 1)) (cons (counter-index (car counters)) (field (car counters))))
    ; Daca timpul primei structuri din lista <= timpul ((al 2-lea element al perechii) (=> cdr)) celorlalte, intoarce prima structura
    ; pentru ca am <= si se apeleaza recursiv se va retine elementul cu index minim
    ((and (<= (field (car counters)) (cdr (general-min field (cdr counters)))) (= (counter-open (car counters)) 1))
          (cons (counter-index (car counters)) (field (car counters))))
    ; Continua pe restul listei
    (else 
     (general-min field (cdr counters)))))

(define (min-tt counters)
  (if (null? counters)
      '()
      (general-min counter-tt counters)))


(define (min-et counters)
  (if (null? counters)
      '()
      (general-min counter-et counters)))


; Gasesc doar casele deschise
(define (find-open-counters counters acc)
  (if (null? counters)
      acc
      (if (= (counter-open (car counters)) 1)
          (find-open-counters (cdr counters) (append acc (list (car counters))))
          (find-open-counters (cdr counters) acc))))
      


; Elimina primul om de la coada
; Daca era un singur om, se reseteaza et-ul si tt-ul
(define (remove-first-from-counter C)   ; testată de checker
  (match C
    [(counter index tt et open queue)
     ; daca e un singur client la coada
         (if (queue-empty? (dequeue queue))
             (make-counter index 0 0 open empty-queue)
             ; tt = suma (restul timpilor) de la coada, fara primul client
             ; et = n-items (al 2-lea elem al perechii => cdr) al celui de-al doilea client din coada
             ; coada = restul cozii fara primul client
             (make-counter index (sum_tt (dequeue queue)) (cdr (top (dequeue queue))) open (dequeue queue)))]))


; Face suma tt-urilor dintr-o coada
(define (sum_tt queue)
  (if (queue-empty? queue)
      0
      (+ (cdr (top queue)) (sum_tt (dequeue queue)))))


; Functia intoarce diferenta dintre a si b, iar daca aceasta este negativa
; intoarce 0
(define (make_dif a b)
  (if (< (- a b) 0)
      0
      (- a b)))


; Returneaza numarul de minute la care iese
; ultimul om de la o casa C, in
; intervalul [0; minutes]
; Daca casa are coada goala returneaza 0
(define (sum_ultimate_et C minutes S)
  (if (and (<= (counter-et C) minutes) (not (queue-empty? (counter-queue C))))
      (sum_ultimate_et (remove-first-from-counter C) (- minutes (counter-et C)) (+ S (counter-et C)))
      S))


; Functia calculeaza timpul care trebuie scazut
; pana la minutes din et
; ex: Daca ultimul client paraseste casa la min 17 si minutes = 20,
; va returna 3.
(define (calculate_minutes C minutes)
  (- minutes (sum_ultimate_et C minutes 0)))


; Functia aplica update-ul de scoatere de la o casa, a primului client,
; pe baza indexului, pe baza listei de exit_clients formata anterior
(define (remove_client counters exit_clients)
  (if (null? exit_clients)
      counters
      (remove_client (apply_update remove-first-from-counter counters (car (car exit_clients))) (cdr exit_clients))))


; Functia actualizeaza, pe baza listelor de minute, de la fiecare moment
; de timp, et-ul caselor 
(define (update_counters_et counters minutes exit_clients)
  (map pass-time-through-counter-helper (create_list_minutes counters minutes) (remove_client counters exit_clients)))


; Creeaza o lista de minute ce trebuie scazute
; pentru fiecare casa pana la acel moment
(define (create_list_minutes counters minutes)
  (if (null? counters)
      '()
      (cons (calculate_minutes (car counters) minutes) (create_list_minutes (cdr counters) minutes))))


; Functia e un helper pentru functia de trecere a minutelor, pentru
; ca nu am nevoie de o functie lambda in update_counters_et
(define (pass-time-through-counter-helper minutes C)
  ((pass-time-through-counter minutes) C))

  
; TODO
; Implementați o funcție care calculează starea unei case după un număr dat de minute.
; Funcția presupune, fără să verifice, că în acest timp nu a ieșit nimeni din coadă, 
; deci va avea efect doar asupra câmpurilor tt și et.
; (cu alte cuvinte, este responsabilitatea utilizatorului să nu apeleze această funcție
; cu minutes > timpul până la ieșirea primului client din coadă)
; Atenție: casele fără clienți nu trebuie să ajungă la timpi negativi!
(define (pass-time-through-counter minutes)
  (λ (C)
    (match C
      [(counter index tt et open queue)
       ; Am acces la campurile index, tt, et si queue cu match si creez o noua
       ; structura cu ele, adaugand la timpul de asteptare numarul de items
       ; si la coada perechea (name . n-items)
       ; scad minutes din et-ul si tt-ul 
       (make-counter index (make_dif tt minutes) (make_dif et minutes) open queue)
       ])))



; Functia calculeaza timpul total pentru toate casele deschise
(define (calculate_tt counters)
  (cond
    ((null? counters) 0)
    ((= (counter-open (car counters)) 1) (+ (counter-tt (car counters)) (calculate_tt (cdr counters))))
    (else
     (calculate_tt (cdr counters)))))


; Functia adauga o casa slow noua la lista de slow-counters
; Casa va avea indexul = length(fast + slow) + 1;
(define (update_slow slow-counters fast-counters)
  (append slow-counters (list (empty-counter (add1 (+ (length slow-counters) (length fast-counters)))))))

; Functia creeaza o lista cu toate casele, inclusiv cu cea noua abia adugata
(define (new_counters fast-counters slow-counters)
  (append fast-counters (update_slow slow-counters fast-counters)))


; Functia adauga case slow cat timp tt <= average
(define (add_slow_counters counters average slow-counters fast-counters)
  (cond
    ; Suma tt-urilor este calculata doar pentru casele cu (counter-open == 1)
    ((> (/ (calculate_tt counters) (length (find-open-counters counters '()))) average) (add_slow_counters (new_counters fast-counters slow-counters)
                                                                                                            average
                                                                                                            (update_slow slow-counters fast-counters)
                                                                                                            fast-counters))
    (else
     slow-counters)))


; Functia adauga la numarul de produse ale clientilor, timpul et-ul initial al casei, pentru a obtine minutul
; la care acestia vor parasi magazinul
(define (exit_minut_for_every_client acc intarziere q)
  (if (queue-empty? q)
      acc
      ; numele clientului ; numarul de produse + intarziere         intarziere = minutul actualului
      (exit_minut_for_every_client (cons (cons (car (top q)) (+ (cdr (top q)) intarziere)) acc) (+ intarziere (cdr (top q))) (dequeue q))))



; Functia creaza o lista de perechi de forma  (nume_client . exit_timp)
(define (create_exit_minut_list C index) 
  (reverse (exit_minut_for_every_client
            (list (cons (car (top (counter-queue C))) (counter-et C)))
            (counter-et C)
            (dequeue (counter-queue C)))))



; Functia verifica daca in lista de exit-time-uri de la acea casa
; la acel moment este vreun client care trebuie sa iasa
; si il adauga la lista de acc
(define (out_at_specific_moment minut out_L index acc)
  ; exit_time
  (cond
    ((null? out_L) acc)
    ((= (cdr (car out_L)) minut) (append acc (cons index (car (car out_L)))))
    (else
     (out_at_specific_moment minut (cdr out_L) index acc))))


; Functia intoarce true daca e cineva out la acel minut
(define (now_out minut out_L index)
  (if (null? (out_at_specific_moment minut out_L index '()))
      #f
      #t))


; Cat timp mai am minute
; Cat timp mai am case
; Creez lista de la prima casa
; Exista in ea un om de out?
; Daca da, ii fac append la lista mea
; Daca nu, trec la urmatoarea casa
(define (iterate_counters counters L minut)
  (if (null? counters)
      (reverse L)
      (if (now_out minut (create_exit_minut_list (car counters) (counter-index (car counters))) (counter-index (car counters)))
          ; Trebuie sa paraseasca cineva magazinul, il adaug cu nume si index
          (iterate_counters (cdr counters)
                            (append L (list (out_at_specific_moment minut
                                                                    (create_exit_minut_list (car counters) (counter-index (car counters)))
                                                                    (counter-index (car counters))
                                                                    '()
                                                                    )))
                            minut)
          (iterate_counters (cdr counters) L minut))))


; Cat timp mai este timp, iterez prin toate casele si caut un client care paraseste
; casa la momentul acela, daca la un moment t nu iese niciun client pe pozitia t in lista
; va fi '()
(define (exist_time counters x L)
  (if (zero? x)
      L
      (exist_time counters (- x 1) (append L (iterate_counters counters '() x)))))


; Functia curata lista de exit-time uri de '()
(define (clear_exit_list L acc)
  (cond
    ((null? L) (reverse acc))
    ((not (null? (car L))) (clear_exit_list (cdr L) (append acc (list (car L)))))
    (else
     (clear_exit_list (cdr L) acc))))


; Functia obtine lista finala de exit-uri de forma (index . nume)
(define (obtain_final_list x good_counters)
  (clear_exit_list (exist_time good_counters x '()) '()))

                           
; Functia verifica daca casa nu are coada goala si are un et < x
(define (counter_not_empty C x)
  (if (<= (counter-et C) x)
      (if (not (queue-empty? (counter-queue C)))
          #t
          #f)
      #f))


; Functia creeaza liste de case doar cu cele cu et-ul <= x ul dat
(define (create_good_counters counters good_C x)
  (cond
    ((null? counters) good_C)
    ((counter_not_empty (car counters) x) (create_good_counters (cdr counters) (append good_C (list (car counters))) x))
    (else
     (create_good_counters (cdr counters) good_C x))))



; Functia creaza o lista cu toti clientii in ordine, care au iesit
; de la case pana in x minute
(define (create_clients_exit_list counters x)
  (if (null? (obtain_final_list x (create_good_counters counters '() x)))
      '()
      (obtain_final_list x (create_good_counters counters '() x))))


(define (construct_counters fast slow)
  (append fast slow))


; Functia intoarce o lambda cu casa inchisa
(define (modify_open)
  (lambda (C)
    (match C
      [(counter index tt et open queue)
       ; Am acces la campurile index, tt, et si queue cu match si creez o noua
       ; structura cu ele
       (struct-copy counter C [open 0])])))


; Functia face o lista de perechi (index . coada) daca coada nu e goala
; adica daca mai sunt oameni la casa
(define (make-pair-counter-queue counters acc)
  (cond
    ((null? counters) acc)
    ((not (queue-empty? (counter-queue (car counters)))) (make-pair-counter-queue (cdr counters)
                                                                                  (append acc (list (cons (counter-index (car counters)) (counter-queue (car counters)))))))
    (else
     (make-pair-counter-queue (cdr counters) acc))))
    


(define (serve_helper requests fast-counters slow-counters L)
  (if (null? requests)
      (cons L (make-pair-counter-queue (append fast-counters slow-counters) '()))
      (match (car requests)
        [(list 'delay index minutes)
         ; In functie de indexul dat se modifica casa respectiva si se apeleaza recursiv servirea
         (serve_helper (cdr requests) (update (add_delay minutes) fast-counters index) (update (add_delay minutes) slow-counters index) L )]

         [(list 'close index)
         (serve_helper (cdr requests) (update (modify_open) fast-counters index) (update (modify_open) slow-counters index) L)]

        
        [(list 'ensure average)
         (serve_helper (cdr requests) fast-counters (add_slow_counters (append fast-counters slow-counters) average
                                                                       slow-counters
                                                                       fast-counters)
                                                                       L)
         ]

        [(list name n-items)
         (if (<= n-items ITEMS)
             ; indexul casei cu tt-ul minim
             (serve_helper (cdr requests) (update (add-to-counter name n-items) fast-counters (car (min-tt (append fast-counters slow-counters))))
                           (update (add-to-counter name n-items) slow-counters (car (min-tt  (append fast-counters slow-counters)))) L)
        
             (serve_helper (cdr requests) fast-counters (update (add-to-counter name n-items) slow-counters (car (min-tt slow-counters ))) L)
             )]

        [x                                                                                        
         (serve_helper (cdr requests) (update_counters_et fast-counters x (create_clients_exit_list (construct_counters fast-counters slow-counters) x))
                       (update_counters_et  slow-counters x (create_clients_exit_list (construct_counters fast-counters slow-counters) x))
                       (append L (create_clients_exit_list (construct_counters fast-counters slow-counters) x)))]

        

        )))


; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 3, apare un nou tip de cerere, așadar
; requests conține 5 tipuri de cereri (cele moștenite din etapa 3 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă              (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute         (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)           (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                       (ca înainte)
;   - (close <index>) - casa index este închisă                                            (   NOU!   )
; Sistemul trebuie să proceseze cele 5 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele DESCHISE cu tt minim; nu se va întâmpla
;   niciodată ca o persoană să nu poată fi distribuită la nicio casă                       (mică modificare)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți);
;   nu aplicați vreun tratament special caselor închise                                    (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele DESCHISE, 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>         (mică modificare)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică. (ca înainte)
; - când o casă se închide, ea nu mai primește clienți noi; clienții care erau deja acolo
;   avansează normal, până la ieșirea din supermarket                                    
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul:
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista cozilor (de la case DESCHISE sau ÎNCHISE) care încă au clienți:
;   - fiecare element va avea forma (index_casă . coadă) (coada este de tip queue)
;   - lista este sortată după indexul casei

(define (serve requests fast-counters slow-counters)
  (serve_helper requests fast-counters slow-counters '()))
