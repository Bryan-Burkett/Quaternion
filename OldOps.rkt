#lang racket
(provide 
 old+ old- old* old/ oldsin oldcos oldexpt oldexp oldeq?)
(define old+ +)
(define old- +)
(define old* *)
(define old/ /)
(define oldsin sin)
(define oldcos cos)
(define oldexp exp)
(define oldexpt expt)
(define oldeq? eq?)
