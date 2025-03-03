#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

;; ATENȚIE: Pentru această etapă a temei este necesar să implementați
;;          întâi TDA-ul queue în fișierul queue.rkt.
;; Reveniți la sarcinile din acest fișier după ce ați implementat tipul 
;; queue și ați verificat implementarea folosind checker-ul.


; Structura counter nu se modifică.
; Ceea ce se modifică este implementarea câmpului queue:
; - în loc de listă, acesta va fi o coadă (o structură de tip queue)
; - acest lucru nu este vizibil în definiția structurii counter,
;   ci în implementarea operațiilor acestui tip de date (tipul counter)
(define-struct counter (index tt et queue) #:transparent)


; TODO
; Actualizați funcțiile de mai jos astfel încât ele să folosească
; o structură de tip queue pentru reprezentarea cozii de persoane.
; Elementele cozii continuă să fie perechi (nume . nr_produse).
; Este esențial să respectați "bariera de abstractizare", adică să
; operați cu coada doar folosind interfața acestui TDA:
; - empty-queue
; - queue-empty?
; - enqueue
; - dequeue
; - top
; Obs: Doar câteva funcții vor necesita actualizări.
(define (empty-counter index)           ; testată de checker
  (counter index 0 0 empty-queue))

;caut clasa si aplic  pe ea
(define (find-counter counters index iter f res)
  (if (= iter 0)
      (reverse res)
      (if (= (counter-index (car counters)) index)
          (append (reverse res) (cons (match (car counters) [(counter index tt et queue) (f (car counters))]) (cdr counters)))
          (find-counter (cdr counters) index (- iter 1) f (append (list (car counters)) res))
          )
      )
  )

(define (update f counters index)
     (find-counter counters index (length counters) f '()))

;pt o casa trebuie sa mai adaug minute, voi vrimi candva casa si mai apoi minutele
(define tt+
  (λ(c) (λ(min)
          (match c [(counter index tt et queue) (struct-copy counter c [tt (+ tt min)])]))))


(define et+
   (λ(c) (λ(min)
          (match c [(counter index tt et queue) (struct-copy counter c [et (+ et min)])]))))


(define (add-to-counter name items)     ; testată de checker
  (λ (C)                                ; nu modificați felul în care funcția își primește argumentele
    (struct-copy counter C [index (counter-index C)]
                 [tt (+ (counter-tt C) items)]
                 [et (if (queue-empty? (counter-queue C))
                         (+ (counter-et C) items)
                         (counter-et C))]
                 [queue (enqueue (cons name items) (counter-queue C))])
                            ))


(define (help-min counters index timp f)
  (if (null? counters)
      (cons index timp)
      (if (< (f (car counters)) timp)
           (help-min (cdr counters) (counter-index (car counters)) (f (car counters)) f)
           (if (and (= (f (car counters)) timp) (< (counter-index (car counters)) index))
                  (help-min (cdr counters) (counter-index (car counters)) (f (car counters)) f)
                  (help-min (cdr counters) index timp f))
           )
      )
  )

(define (min-tt counters) (help-min counters (counter-index (car counters)) (counter-tt (car counters)) counter-tt))
(define (min-et counters) (help-min counters (counter-index (car counters)) (counter-et (car counters)) counter-et)) ; folosind funcția de mai sus

;se observa coada dupa dequeue
;Deci se fac actualizari ale tt + et dupa ce se aplica dequeue
(define (remove-first-from-counter C)   ; testată de checker
  (struct-copy counter C
               [index (counter-index C)]
               [tt (if (queue-empty? (dequeue (counter-queue C)))
                       0
                       (- (counter-tt C) (counter-et C)))]
               [et (if (queue-empty? (dequeue (counter-queue C)))
                       0
                       (cdr (top (dequeue (counter-queue C)))))]
                                     
               [queue (dequeue (counter-queue C))]))





; TODO
; Implementați o funcție care calculează starea unei case după un număr dat de minute.
; Funcția presupune, fără să verifice, că în acest timp nu a ieșit nimeni din coadă, 
; deci va avea efect doar asupra câmpurilor tt și et.
; (cu alte cuvinte, este responsabilitatea utilizatorului să nu apeleze această funcție
; cu minutes > timpul până la ieșirea primului client din coadă)
; Atenție: casele fără clienți nu trebuie să ajungă la timpi negativi!
(define (pass-time-through-counter minutes)
  (λ (C)
   (struct-copy counter C
               [index (counter-index C)]
               [tt (if (< (counter-tt C) minutes)
                       0
                       (- (counter-tt C) minutes))]
               [et (if (< (counter-et C) minutes)
                       0
                       (- (counter-et C) minutes))]
                                     
               [queue (counter-queue C)])))
  

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 2, apar modificări în:
; - formatul listei de cereri (parametrul requests)
; - formatul rezultatului funcției (explicat mai jos)
; requests conține 4 tipuri de cereri (3 moștenite din etapa 2 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă            (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute       (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)         (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                     (   NOU!   )
; Obs: Au dispărut cererile de tip remove-first, pentru a lăsa loc unui mecanism mai 
; sofisticat de a scoate clienții din coadă (pe măsură ce trece timpul).
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)  (ca înainte)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți) (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>       (ca înainte)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică.
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista caselor în starea finală (ca rezultatul din etapele 1 și 2)
; Obs: Pentru a contoriza ieșirile din cozi, puteți să lucrați într-o funcție ajutătoare
; (cu un parametru în plus față de funcția serve), pe care serve doar o apelează.

(define (cc counters)
    (car (min-tt counters)))

(define (c-in list index)
  (if (null? list)
      0
      (if (= (counter-index (car list)) index)
          1
          (c-in (cdr list) index))))

(define doub++ (λ (C) (λ(minutes) (match C
                                     [(counter index tt et queue) (struct-copy counter C [tt (+ tt minutes)]  [et (+ et minutes)])]
                                     ))))

(define (empty-all-q list)
  (if (null? list)
      #t
      (if (null? (counter-queue (car list)))
          (empty-all-q  (cdr list))
          #f))) ;dc list e goala inseama ca niciuncounter nu are liste asa ca true, altfel s a oprit;

(define (compute-avg liste medie)
  (if (null? liste)
      medie
      (compute-avg (cdr liste) (+ medie (counter-tt (car liste))))))

(define (for-ensure fast-counters slow-counters average requests)
      (if (> (/ (compute-avg (append fast-counters slow-counters) 0) (length (append fast-counters slow-counters))) average)
        (for-ensure fast-counters (append slow-counters (list (empty-counter (+ 1 (+ (length fast-counters) (length slow-counters)))))) average requests)
        (serve (cdr requests) fast-counters slow-counters)))

;et trece testul, deci acum orice om din aceasta casa care are nr-items mai mic decat timpul iese


;(update (λ (C) (remove-first-from-counter C)) fast-counters (counter-index (find-c-with-q fast-counters 99999 null)))

(define (caut-casa-deunde-elimin-client case timp result lista-case-de-actualizata)
  (if (null? case)
      result
      (if (= (counter-et (car case)) timp) ;dc am gasit casa (in ordine) cu timpul de iesire bun o scot
          (caut-casa-deunde-elimin-client (cdr case) timp ;caut in restul caselor dc am cumva vreau alt client la o alta casa fast care elimina acum
                                          (append result (list (cons (counter-index (car case)) (car (top (counter-queue (car case))))))) ;adaug la res perechi dintre nr case si cel care iese acum/(nume top cea mai mica casa cu acest timp) 
                                          (update (λ (C) (dequeue (counter-queue C)) case (counter-index (car case)))) ) ;actualizez coada aplic pe casa
                         
          (caut-casa-deunde-elimin-client (cdr case) timp result lista-case-de-actualizata))))
           ;dc prima lista(cea la care sunt acum are un timp diferit de timp, evident ar trebui sa ie mai mare, atunci merge mai departe(inseama ca aceasta casa nu are niciun client de eliminat la momentul -timp-;
    
(define (help x iter fast slow list provizoriu)

  (if (< x iter) ;cand am treminat timpul returnez lista de oameni care au plecat;
      list
      (cond ((and (empty-counter fast) (empty-counter slow))  (help x (+ x 1) fast slow list provizoriu)) ;dc nu am case, termin
            ((and (not (empty-counter fast)) (empty-counter slow)) ;dc am doar case fast, dar nu si slow
             (if (empty (caut-casa-deunde-elimin-client fast x provizoriu fast)) ;dc cele fast nu au clienti care sa termine la t=x (nu am nimic in queue al casei de eliminat)
                 (help x (+ x 1) fast slow list '()) ;nimic, nu se executa nimic, doar se termina recursivitatea cam ineficient, poate scap de asta mai tarziu
                 (help x (+ x 1) fast slow (append list provizoriu) '()))) ;inseama ca res de mai sus din caut-case-deunde-elimin-client a fost actalizat la cei ce au fost scosi, la fel si fast deci acum trebuie sa retin ce am scos si sc scad timpul, am scos tot ce puteam pentu moment
            ((and (empty-counter fast) (not (empty-counter slow))) ;elimin de la cele slow, evident dc avem timpul corespunzator
             (if (empty (caut-casa-deunde-elimin-client slow x provizoriu)) ;dc nu am elmnta pe nimeni, cresc timpul
                 (help x (+ x 1) fast slow list '())
                 (help x (+ x 1) fast slow (append list provizoriu) '())))

             ((and (not (empty-counter fast)) (not (empty-counter slow))) ;ii dau sa scoata din tot la rand in caz ca exista ceva de scos
              (if (empty (caut-casa-deunde-elimin-client fast x provizoriu fast)) ;dc nu am fast, scot din cele slow(dc se poate)
                  ((if (empty (caut-casa-deunde-elimin-client slow x provizoriu)) ;dc nu am elmnta pe nimeni, cresc timpul
                       (help x (+ x 1) fast slow list '()) ;nu am nici din slow, continue;
                       (help x (+ x 1) fast slow (append list provizoriu) '()))) ;altfel adaug ceva la list si continue
                  ((if (empty (caut-casa-deunde-elimin-client slow x (append list provizoriu))) ;in punctul asta prvizorui ar trebui inca sa fie ce a amas din apelul cout-casa
                       (help x (+ x 1) fast slow list '()) ;nu am nici din slow, continue;
                       (help x (+ x 1) fast slow (append list provizoriu) '()))))))) ;altfel adaug ceva la list si continue
                   ;altfel am de adaugat din fast dar nu stiu dc am de adaugat si din slow, ii trimit pt alelul lui slow lista cu provizoriu actualizata sa pestrez rezultatul si
                   ;mai apoi cand se revine si din slow ar trebui sa fie rezultatul in forma mai buna;
                  
  ) 

(define (serve requests fast-counters slow-counters)
   (if (null? requests)
      (append fast-counters slow-counters)
      (match (car requests)

         [(list 'ensure average)
             (if (> (/ (compute-avg (append fast-counters slow-counters) 0) (length (append fast-counters slow-counters))) average)
                 (serve requests fast-counters (append slow-counters (list (empty-counter (+ 1 (+ (length fast-counters) (length slow-counters)))))))
                 (serve (cdr requests) fast-counters slow-counters))]
        
        [(list name n-items)
         (if (> n-items ITEMS)
            (serve (cdr requests) fast-counters (update (λ (C) ((add-to-counter name n-items) C)) slow-counters (cc slow-counters)))
            (if (<= (cdr (min-tt fast-counters)) (cdr (min-tt slow-counters)))
                (serve (cdr requests) (update (λ (C) ((add-to-counter name n-items) C)) fast-counters (car (min-tt fast-counters))) slow-counters)
                (serve (cdr requests) fast-counters (update (λ (C) ((add-to-counter name n-items) C)) slow-counters (cc slow-counters)))))]
        
        [(list 'delay index minutes)
         (if (= (c-in fast-counters index) 1) 
             (serve (cdr requests) (update (λ (C) ((doub++ C) minutes)) fast-counters index) slow-counters)
             (serve (cdr requests) fast-counters (update (λ (C) ((doub++ C) minutes)) slow-counters index)))]
        
      [x

       (help x 1 fast-counters slow-counters '() )

       
       ]
       
             
        )))


        
