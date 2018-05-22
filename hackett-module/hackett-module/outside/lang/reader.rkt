#lang s-exp syntax/module-reader hackett-module/outside
#:wrapper1 call-with-hackett-module-reading-parameterization
#:module-wrapper module-wrapper-insert-type-require
(require hackett-module/reader
         (submod hackett/private/kernel module-wrapper))
