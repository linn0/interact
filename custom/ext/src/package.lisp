
(in-package :common-lisp-user)

(defpackage #:ext
  (:use #:cl #:ccl)
  (:export #:enable #:disable

           ;; Definitions
           #:define-constant

           ;; Control flow
           #:cswitch
           #:eswitch
           #:switch
           #:multiple-value-prog2
           #:nth-value-or
           #:whichever
           #:xor

           ;; Conditions
           #:required-argument
           #:ignore-some-conditions
           #:simple-style-warning
           #:simple-reader-error
           #:simple-parse-error
           #:simple-program-error
           #:unwind-protect-case

           ;; Array
           #:copy-array
           #:ensure-array-size
           #:array-shift
           #:vector-push-extend-front
           #:vector-push-extend-position
           #:vector-pop-front
           #:vector-pop-position
           #:vector-append

           ;; Strings
           #:trim-left
           #:trim-right
           #:trim
           #:concat
           #:join
           #:chars
           #:split
           #:starts-with-p
           #:ends-with-p

           ;; Symbols
           #:ensure-symbol
           #:format-symbol
           #:make-gensym
           #:make-gensym-list
           #:make-keyword

           ;; Binding constructs
           #:if-let
           #:when-let
           #:when-let*

           ;; Macros
           #:once-only
           #:parse-body
           #:parse-ordinary-lambda-list
           #:with-gensyms
           #:with-unique-names

           ;; Hash tables
           #:alist-hash-table
           #:copy-hash-table
           #:ensure-gethash
           #:hash-table-alist
           #:hash-table-keys
           #:hash-table-plist
           #:hash-table-values
           #:maphash-keys
           #:maphash-values
           #:plist-hash-table

           ;; Functions
           #:compose
           #:conjoin
           #:curry
           #:disjoin
           #:ensure-function
           #:ensure-functionf
           #:multiple-value-compose
           #:named-lambda
           #:rcurry

           ;; Lists
           #:alist-plist
           #:appendf
           #:nconcf
           #:reversef
           #:nreversef
           #:circular-list
           #:circular-list-p
           #:circular-tree-p
           #:doplist
           #:ensure-car
           #:ensure-cons
           #:ensure-list
           #:flatten
           #:lastcar
           #:make-circular-list
           #:map-product
           #:mappend
           #:nunionf
           #:plist-alist
           #:proper-list
           #:proper-list-length
           #:proper-list-p
           #:remove-from-plist
           #:remove-from-plistf
           #:delete-from-plist
           #:delete-from-plistf
           #:set-equal
           #:setp
           #:unionf

           ;; Queue
           #:make-queue
           #:queue-push
           #:queue-pop
           #:queue-empty-p
           #:queue-peek

           ;; flexi-stream
           #:flexi-input-stream
           #:flexi-output-stream
           #:flexi-io-stream
           #:flexi-stream
           #:flexi-stream-bound
           #:flexi-stream-column
           #:flexi-stream-external-format
           #:flexi-stream-element-type
           #:flexi-stream-element-type-error
           #:flexi-stream-element-type-error-element-type
           #:flexi-stream-error
           #:flexi-stream-out-of-sync-error
           #:flexi-stream-position
           #:flexi-stream-stream
           #:make-flexi-stream

           ;; In-memory
           #:get-output-stream-sequence
           #:in-memory-stream
           #:in-memory-stream-closed-error
           #:in-memory-stream-error
           #:in-memory-stream-position-spec-error
           #:in-memory-stream-position-spec-error-position-spec
           #:in-memory-input-stream
           #:in-memory-output-stream
           #:list-stream
           #:make-in-memory-input-stream
           #:make-in-memory-output-stream
           #:output-stream-sequence-length
           #:vector-stream
           #:with-input-from-sequence
           #:with-output-to-sequence

           ;; Features
           #:featurep

           ;; Promise
           #:*debug-on-error* #:*promise-keep-specials* #:*promise-finish-hook*

           #:promise #:promise-finished-p #:reject-promise #:make-promise #:create-promise
           #:with-promise #:promisify

           #:attach-errback #:lookup-forwarded-promise #:signal-error
           #:promisep #:finish #:reset-promise

           #:do-attach #:attach #:do-catch #:catcher #:do-tap #:tap
           #:do-finally #:finally

           #:alet #:alet* #:aif #:multiple-promise-bind #:wait #:walk #:walk1

           #:aeach #:adolist #:amap #:all #:areduce #:afilter #:tap #:chain

           #:make-pool #:fetch-from #:return-to #:destroy-pool-item

           ;; Thread
           #:thread #:make-thread #:current-thread #:threadp #:thread-name
           #:start-multiprocessing
           #:*default-special-bindings* #:*standard-io-bindings*
           #:*supports-threads-p*

           #:lock-p #:acquire-lock #:with-lock-held

           #:recursive-lock #:make-recursive-lock #:recursive-lock-p
           #:acquire-recursive-lock #:release-recursive-lock #:with-recursive-lock-held

           #:make-condition-variable #:condition-wait #:condition-notify

           #:with-timeout #:timeout

           #:all-threads #:interrupt-thread #:destroy-thread #:thread-alive-p
           #:join-thread #:thread-yield #:thread-stack-trace

           ;; timer
           #:make-timer
           #:schedule-timer #:schedule-timer-relative
           #:timer-expired-p #:timer-name
           #:unschedule-timer
           #:make-scheduler

           ;; async socket
           #:make-async-socket #:async-connect #:async-write 
           #:async-read #:async-read-until #:async-read-some
           #:make-device-proactor #:register

           ;; async ssl socket
           #:make-async-ssl-socket))
