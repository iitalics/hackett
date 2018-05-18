#lang s-exp syntax/module-reader hackett-module/inside
#:wrapper1 call-with-hackett-reading-parameterization
#:module-wrapper module-wrapper-insert-type-require
(require hackett/private/reader
         (submod hackett/private/kernel module-wrapper))
