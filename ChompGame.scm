;G38-220201024-Hasan Yeniada

;Takes row and column of game table and game starts
(define (take-row-col-input-for-game-and-start)
  (let ((row (take-input "How many ROW do you want to play with? ")))
    (let ((col (take-input "How many COLUMN do you want to play with? ")))
      (main row col))))
;-------------------------MAIN-----------------------------------
(define (main row col)
   (display "Chomp game is starting!")(newline)(display "If you choose poisoned cookie,YOU LOST:")(newline)
   (display "Choose one of cookies from given game table!")(newline) (display "Do not choose dashed cookie!")(newline)
   (simulate-chompgame (make-first-game-state row col 1) 'human row col))

;-------------------------SIMULATION-----------------------------------
(define simulate-chompgame
  (lambda (game-state player row col)
    (display-game-table game-state (create-table game-state row col 1) player row col)
   (cond [(over? game-state row) (print-winner player)] ;base case: if poisoned cookie is eaten: Game over
         [(equal? player 'human) (simulate-chompgame (human-play game-state row col) 'computer row col)]
         [(equal? player 'computer) (simulate-chompgame (computer-play game-state row col) 'human row col)]
)))
;-------------------------OVER-GAME?--------------------------------------
(define over? ;looks total cookie count at all rows and if it is zero game ends
   (lambda (game-state row)
   (= (total-size game-state 1 row) 0)))
;-------------------------PRINT WINNER-------------------------------------------
(define (print-winner player) ;announces the winner according to given player
  (cond[(equal? player 'human)(newline)(display "HUMAN has won the game!")]
       [else (newline)(display "COMPUTER has won the game!")]))

;----------------------HUMAN MAKES CHOICE--------------------
(define (human-play game-state total-row total-col)
   (let ((row (take-input "Which row? ")))
    (let ((col (take-input "which column? ")))
     (update-game-table game-state row col total-row total-col 1)
  )))
(define take-input
   (lambda (question)
     (newline)
     (display question)
     (read)))
;----------------------COMPUTER MAKES CHOICE-----------------
(define (computer-play game-state row col);according to given chosen row col list--> (row,col),sends row and col to update function
  (define choice-list (computer-move-helper game-state (give-reverse-table game-state row) row))
  (cond [(and (= (total-size game-state 1 row) (size game-state 1 1))(> (total-size game-state 1 row) 1)) (update-game-table game-state 1 2 row col 1)]
        [(and (clever-move-iter (give-reverse-table game-state row) row #f)(= (car game-state) col) (update-game-table game-state 1 (car game-state) row col 1))]
        [(and (clever-move-iter (give-reverse-table game-state row) row #f)(> (total-size game-state 1 row) 1)) (update-game-table game-state 2 1 row col 1)]
        [else (update-game-table game-state (car choice-list) (cadr choice-list) row col 1)]))

(define (computer-move-helper game-state reverse-game-state row);this func send the choice of computer to computer-play func as a list
  (cond [ (> (car reverse-game-state) 0) (list row (random 1 (+ (size game-state row 1) 1)))]
        [else (computer-move-helper game-state (cdr reverse-game-state) (- row 1))]))

(define (give-reverse-table game-state counter);gives the reverse of game state.it will help us for recursive call
  (cond[(= counter 0) null]
       [else (cons (size game-state counter 1) (give-reverse-table game-state (- counter 1)))]))

(define clever-move-iter ;this func helps for a clever computer move
  (lambda (game-state i bool) ;if just first column cookies remain this func returns #t ,otherwise #f
    (cond[(= i 1) bool]       ;and according to that (computer-move) will select a cookie wisely
         [(= (car game-state) 1) (clever-move-iter (cdr game-state) (- i 1) #t)]
         [else (clever-move-iter (cdr game-state) (- i 1) #f)]
)))
;--------------->>>-----TABLE OPERATIONS-------<<<----------

;-----------------------update-table--------------
(define update-game-table ;updates table according to given row and col choices and total row and col number of game table
  (lambda (game-state row col total-row total-col counter)
    (cond[(> counter total-row) null]
      [else (cons (update-helper (size game-state counter 1) row col counter) (update-game-table game-state row col total-row total-col (+ counter 1)))]
)))
(define (update-helper cookie-count row-choice col-choice counter)
    (cond[(< counter row-choice) cookie-count]
         [(or (> cookie-count col-choice)(= cookie-count col-choice)) (- col-choice 1) ]
         [(< cookie-count col-choice) cookie-count]))
;----------------------first-game-state------------------
(define (make-first-game-state row col i)
  (cond[(> i row) null]
       [else (cons col (make-first-game-state row col (+ i 1)))]))

(define size ;gives the cookie count at given row
  (lambda (game-state row i)
    (cond [(= row i) (car game-state)]
          [else (size (cdr game-state) row (+ i 1))]
  )))
;----------------------create-game-table-------------------------
(define (create-table game-state row col i) ;makes a new table to show it to players according to row col choices
      (cond[(> i row) null]
           [else (cons (create-helper col (car game-state) 1) (create-table (cdr game-state) row col (+ i 1)))]))
(define (create-helper total-col col p)  ;makes eaten cookies '- and shows cookies which are not eaten with 'x
     (cond[(> p total-col) null]
          [(= col total-col) (cons 'x (create-helper total-col col (+ p 1)))]
          [(or (> col p)(= col p)) (cons 'x (create-helper total-col col (+ p 1)))]
          [(> p col) (cons '- (create-helper total-col col (+ p 1)))]))
;----------------------total cookie count-----------------------------------           
(define total-size  ;gives the sum of cookies at all rows 
  (lambda (game-state i row)
    (cond[(> i row) 0]
         [else (+ (size game-state i 1) (total-size game-state (+ i 1) row))]
)))
;-------------------display-game-table--------------------------------
(define display-game-table   
  (lambda (game-state game-table player row col)
    (newline)
    (cond [(and (equal? player 'human)(< (total-size game-state 1 row) (* row col)))(display "After computer turn new table is here.Choose a cookie!")]
          [(and (equal? player 'computer)(< (total-size game-state 1 row) (* row col)))(display "After human turn new table is here:")])
    (newline)
    (display-helper game-table 0)   
))
(define (display-helper game-table p)
  (cond[(null? game-table) (newline)]
       [else (display-helper (cdr game-table)(displayer (car game-table)))]))
(define (displayer cookies)
  (display cookies) (newline))

;------Game Starts------
(take-row-col-input-for-game-and-start)
;-----------------------