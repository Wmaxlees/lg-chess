#lang racket

(require "controller.rkt")

(define controller (new chess-controller%))

(send controller start)
