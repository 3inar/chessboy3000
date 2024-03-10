#lang racket

; reads all lines of record.csv
(define record (file->lines "record.csv"))

; for each line in record, split on , and trim whitespace on the results
(define games
  (map (lambda (x)
         (map string-trim (string-split x ",")))
       record))


; love some mutable state
(define ratings (make-hash))

(define (update-elo gm) 
  (define rating-white (hash-ref ratings (first gm) 800.0))
  (define rating-black (hash-ref ratings (second gm) 800.0))
  (define K 30)

  ; score for white and black 
  (define sw (cond
               [(equal? (third gm) "1-0") 1]
               [(equal? (third gm) "0-1") 0]
               [(equal? (third gm) "1/2-1/2") .5]
               [else (error "invalid match outcome")]))
  (define sb (- 1 sw))

  ; used in calculating expected scores
  (define qw (expt 10 (/ rating-white 400)))
  (define qb (expt 10 (/ rating-black 400)))

  ; expected scores
  (define ew (/ qw (+ qw qb)))
  (define eb (- 1 ew))
  
  ; rating update is difference in actual score and expected score scaled by K
  ; love a good side effect
  (hash-set! ratings (first gm) (+ rating-white (* K (- sw ew))))
  (hash-set! ratings (second gm) (+ rating-black (* K (- sb eb)))))

(for-each update-elo games)

(define ranking (sort (hash->list ratings) #:key cdr >))

; ranking = sort!(collect(ranking), by=last, rev=true)
; padto = 4*Int(ceil((longestname + 1)/4.0))
; 
; println("A218B/HDL chess federation official ranking")
; println("-------------------------------------------")
; for i in 1:length(ranking)
;   if i < 10
;     pad1 = "  "
;   else
;     pad1 = " "
;   end
; 
;   pad2 = repeat(" ", padto - length(ranking[i][1]))
; 
;   println(string(i) * "." * pad1 * 
;           ranking[i][1] * pad2 * 
;           string(Int(round(ranking[i][2]))))
; end

; pad string w spaces to target length
(define (pad str len) 
  (if (<= len (string-length str))
      str 
      (pad (string-join (list str " ") "") len)))

; get list of players for a set of games

; list of active players: people who played in the last 50 games
; this is kind of ugly?? used (let ()) before. maybe better?
(define active ((lambda ()
  (define n_drop (- (length games) 50)) 
  (define (extract-players gms)
    (remove-duplicates 
      (foldr (lambda (x res) (append (take x 2) res)) '() gms)))
  (extract-players (drop games n_drop)))))


(define (positions n_players)
  (map (lambda (x) (string-join (list (~r x) ".") ""))
       (range 1 (+ n_players 1))))

; filter inactive players out
(define ranked (filter (lambda (x) (member (car x) active)) ranking))

(displayln "A218B/HDL chess federation official ranking")
(displayln "-------------------------------------------")

(for-each (lambda (pos player) 
            (define longest_name (apply max (map string-length active)))
            (define ppos (pad pos 3)) ; must change when we get 100 players
            (define pplayer (pad (car player) longest_name))
            (define rating (~r (exact-round (cdr player))))
            (displayln (string-join (list ppos pplayer rating)))) 
          (positions (length ranked))
          ranked)
; string-join
; exact-round
; (~r 4) -> "4"
